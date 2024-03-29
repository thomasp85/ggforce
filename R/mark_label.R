#' @importFrom grid gpar
inherit_gp <- function(..., gp, call = caller_env()) {
  new_gp <- list2(...)
  for (par in names(new_gp)) {
    old_par <- par
    inherited_par <- new_gp[[par]]
    if (isTRUE(new_gp[[par]] == 'inherit')) {
      inherited_par <- gp[[old_par]]
    } else if (isTRUE(new_gp[[par]] == 'inherit_fill')) {
      old_par <- 'fill'
      inherited_par <- gp[[old_par]]
    } else if (isTRUE(grepl('inherit_col', new_gp[[par]]))) {
      old_par <- 'col'
      inherited_par <- gp[[old_par]]
    }
    if (is.null(inherited_par)) {
      cli::cli_abort("Can't inherit {.field {old_par}} as it is not given in the root {.cls gpar}")
    }
    new_gp[[par]] <- inherited_par
  }
  inject(gpar(!!!new_gp))
}
subset_gp <- function(gp, index, ignore = c('font')) {
  gp_names <- names(gp)
  gp_names <- gp_names[-unique0(unlist(lapply(ignore, grep, gp_names)))]
  for (par in gp_names) {
    gp[[par]] <- rep_len(gp[[par]], index)[index]
  }
  gp
}

#' @importFrom polyclip polyoffset polyminkowski polyclip
#' @importFrom grid convertX convertY
place_labels <- function(rects, polygons, bounds, anchors, ghosts) {
  res <- vector('list', length(rects))
  bbox <- list(
    x = c(0, bounds[1], bounds[1], 0),
    y = c(0, 0, bounds[2], bounds[2])
  )
  if (!is.null(ghosts) && length(ghosts$x) > 0) {
    ghosts$x <- convertX(ghosts$x, 'mm', TRUE)
    ghosts$y <- convertY(ghosts$y, 'mm', TRUE)
    ghosts <- Map(
      function(xmin, xmax, ymin, ymax) {
        list(x = c(xmin, xmax, xmax, xmin), y = c(ymin, ymin, ymax, ymax))
      },
      xmin = ghosts$x - 2,
      xmax = ghosts$x + 2,
      ymin = ghosts$y - 2,
      ymax = ghosts$y + 2
    )
    ghosts <- polyoffset(ghosts, 0)
    polygons <- c(polygons, ghosts)
  }
  for (i in seq_along(rects)) {
    if (all(rects[[i]] == 0)) next()
    r <- rects[[i]] / 2 + 2
    rect <- list(x = c(-r[1], r[1], r[1], -r[1]),
                 y = c(-r[2], -r[2], r[2], r[2]))
    b <- polyminkowski(bbox, rect)
    for (p in polygons) {
      b <- polyclip(b, polyminkowski(p, rect)[1], 'union')
    }
    if (length(b) == 1) next()
    b <- lapply(b[-1], function(p) cbind(p$x, p$y))
    closest <- points_to_path(matrix(anchors[[i]], ncol = 2), b, TRUE)
    res[[i]] <- closest$proj
    rect$x <- rect$x + closest$proj[1]
    rect$y <- rect$y + closest$proj[2]
    polygons[[length(polygons) + 1]] <- polyoffset(rect, 10)
  }
  res
}
#' @importFrom polyclip polyoffset
#' @importFrom grid convertWidth convertHeight nullGrob polylineGrob
#' @importFrom stats runif
make_label <- function(labels, dims, polygons, ghosts, buffer, con_type,
                       con_border, con_cap, con_gp, anchor_mod, anchor_x,
                       anchor_y, arrow) {
  polygons <- lapply(polygons, function(p) {
    if (length(p$x) == 1 & length(p$y) == 1) {
      list(
        x = runif(200, p$x-0.00005, p$x+0.00005),
        y = runif(200, p$y-0.00005, p$y+0.00005)
      )
    } else {
      list(
        x = p$x,
        y = p$y
      )
    }
  })

  anchors <- lapply(seq_along(polygons), function(i) {
    x <- mean(range(polygons[[i]]$x))
    if (length(anchor_x) == length(polygons) && !is.na(anchor_x[i])) x <- anchor_x[i]
    y <- mean(range(polygons[[i]]$y))
    if (length(anchor_y) == length(polygons) && !is.na(anchor_y[i])) y <- anchor_y[i]
    c(x, y)
  })
  p_big <- polyoffset(polygons, convertWidth(buffer, 'mm', TRUE))

  area <- c(
    convertWidth(unit(1, 'npc'), 'mm', TRUE),
    convertHeight(unit(1, 'npc'), 'mm', TRUE)
  )
  labelpos <- place_labels(dims, p_big, area, anchors, ghosts)
  if (all(lengths(labelpos) == 0)) {
    return(list(nullGrob()))
  }
  labels_drawn <- which(!vapply(labelpos, is.null, logical(1)))
  labels <- Map(function(lab, pos) {
    if (is.null(pos) || inherits(lab, 'null')) return(nullGrob())
    lab$vp$x <- unit(pos[1], 'mm')
    lab$vp$y <- unit(pos[2], 'mm')
    lab
  }, lab = labels, pos = labelpos)
  connect <- inject(rbind(!!!Map(function(pol, pos, dim) {
    if (is.null(pos)) return(NULL)
    dim <- dim / anchor_mod
    pos <- cbind(
      c(pos[1] - dim[1], pos[1] + dim[1], pos[1] + dim[1], pos[1] - dim[1]),
      c(pos[2] - dim[2], pos[2] - dim[2], pos[2] + dim[2], pos[2] + dim[2])
    )
    pos <- points_to_path(pos, list(cbind(pol$x, pol$y)), TRUE)
    pos$projection[which.min(pos$distance), ]
  }, pol = polygons, pos = labelpos, dim = dims)))
  labeldims <- inject(rbind(!!!dims[lengths(labelpos) != 0])) / 2
  labelpos <- inject(rbind(!!!labelpos))
  if (con_type == 'none' || !con_type %in% c('elbow', 'straight')) {
    connect <- nullGrob()
  } else {
    con_fun <- switch(con_type, elbow = elbow, straight = straight)
    connect <- con_fun(
      labelpos[, 1] - labeldims[, 1], labelpos[, 1] + labeldims[, 1],
      labelpos[, 2] - labeldims[, 2], labelpos[, 2] + labeldims[, 2],
      connect[, 1], connect[, 2]
    )
    if (con_border == 'one') {
      connect <- with_borderline(
        labelpos[, 1] - labeldims[, 1],
        labelpos[, 1] + labeldims[, 1], connect
      )
    }
    connect <- end_cap(connect, con_cap)
    connect <- zip_points(connect)
    if (!is.null(arrow)) arrow$ends <- 2L
    con_gp <- subset_gp(con_gp, labels_drawn)
    connect <- polylineGrob(connect$x, connect$y,
      id = connect$id,
      default.units = 'mm', gp = con_gp, arrow = arrow
    )
  }
  c(labels, list(connect))
}

#' @importFrom grid valid.just textGrob nullGrob viewport grobWidth grobHeight
#' rectGrob gpar grid.layout unit gTree gList grobDescent
labelboxGrob <- function(label, x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
                         description = NULL, width = NULL, min.width = 50,
                         default.units = 'mm', hjust = 0,
                         pad = margin(2, 2, 2, 2, 'mm'), gp = gpar(), desc.gp = gpar(),
                         vp = NULL) {
  width <- as_mm(width, default.units)
  min.width <- as_mm(min.width, default.units)
  pad <- as_mm(pad, default.units)
  pad[c(1, 3)] <- as_mm(pad[c(1, 3)], default.units, FALSE)
  if (!is.null(label) && !is.na(label)) {
    if (!is.null(width)) {
      label <- wrap_text(label, gp, width - pad[2] - pad[4])
    }
    just <- c(hjust[1], 0.5)
    lab_grob <- textGrob(label, x = just[1], y = just[2], just = just,
                         gp = gp)
  } else {
    lab_grob <- nullGrob()
  }
  if (!is.null(width)) {
    final_width <- max(width, min.width) - pad[2] - pad[4]
  } else {
    if (as_mm(grobWidth(lab_grob)) > (min.width - pad[2] - pad[4])) {
      final_width <- as_mm(grobWidth(lab_grob)) + pad[2] + pad[4]
    } else {
      final_width <- max(as_mm(grobWidth(lab_grob)), min.width) - pad[2] - pad[4]
    }
  }
  if (!is.null(description) && !is.na(description)) {
    description <- wrap_text(description, desc.gp, final_width)
    just <- c(rep_len(hjust, 2)[2], 0.5)
    desc_grob <- textGrob(description, x = just[1], y = just[2], just = just,
                          gp = desc.gp)
    if (is.null(width)) {
      final_width_desc <- min(final_width, as_mm(grobWidth(desc_grob)))
      final_width <- as_mm(grobWidth(lab_grob))
      if (final_width < final_width_desc) {
        final_width <- final_width_desc
      }
    }
  } else {
    desc_grob <- nullGrob()
    if (is.null(width)) final_width <- as_mm(grobWidth(lab_grob))
  }
  bg_grob <- rectGrob(gp = gpar(col = NA, fill = gp$fill))
  lab_height <- as_mm(grobHeight(lab_grob), width = FALSE)
  desc_height <- as_mm(grobHeight(desc_grob), width = FALSE)
  sep_height <- if (lab_height > 0 && desc_height > 0) {
    pad[1]
  } else if (lab_height > 0) {
    font_descent(gp$fontfamily, gp$fontface, gp$fontsize, gp$cex)
  } else {
    0
  }
  vp <- viewport(
    x = x,
    y = y,
    width = unit(final_width + pad[2] + pad[4], 'mm'),
    height = unit(pad[1] + pad[3] + lab_height + desc_height + sep_height,
                  'mm'),
    layout = grid.layout(
      5, 3,
      widths = unit(c(pad[2], final_width, pad[4]), 'mm'),
      heights = unit(c(pad[1], lab_height, sep_height, desc_height, pad[3]),
                     'mm')
    )
  )
  lab_grob$vp <- viewport(layout.pos.col = 2, layout.pos.row = 2)
  desc_grob$vp <- viewport(layout.pos.col = 2, layout.pos.row = 4)
  gTree(children = gList(bg_grob, lab_grob, desc_grob), vp = vp,
        cl = 'mark_label')
}
#' @export
#' @importFrom grid widthDetails
widthDetails.mark_label <- function(x) {
  x$vp$width
}
#' @export
#' @importFrom grid heightDetails
heightDetails.mark_label <- function(x) {
  x$vp$height
}
#' @importFrom grid textGrob grobWidth
wrap_text <- function(text, gp, width) {
  text <- gsub('-', '- ', text)
  text <- strsplit(text, split = ' ', fixed = TRUE)[[1]]
  text <- paste0(text, ' ')
  text <- sub('- ', '-', text)
  txt <- ''
  for (i in text) {
    oldlab <- txt
    txt <- paste0(txt, i)
    tmpGrob <- textGrob(txt, gp = gp)
    if (as_mm(grobWidth(tmpGrob)) > width) {
      txt <- paste(trimws(oldlab), i, sep = '\n')
    }
  }
  trimws(txt)
}
#' @importFrom grid unit is.unit convertWidth convertHeight
as_mm <- function(x, def, width = TRUE) {
  if (is.null(x)) return(x)
  if (!is.unit(x)) x <- unit(x, def)
  if (width) {
    convertWidth(x, 'mm', TRUE)
  } else {
    convertHeight(x, 'mm', TRUE)
  }
}
straight <- function(xmin, xmax, ymin, ymax, x, y) {
  conn_point <- get_end_points(xmin, xmax, ymin, ymax, x, y)
  list(
    as.matrix(conn_point),
    cbind(x = x, y = y)
  )
}
elbow <- function(xmin, xmax, ymin, ymax, x, y) {
  lines <- straight(xmin, xmax, ymin, ymax, x, y)
  end_pos <- lines[[1]] - lines[[2]]
  end_angle <- atan2(end_pos[, 2], end_pos[, 1]) %% (2 * pi)
  angle_bin <- end_angle %/% (pi / 4)
  angle_lower <- end_angle %% (pi / 4) < 0.5
  elbow <- lapply(seq_along(angle_bin), function(i) {
    a_bin <- angle_bin[i]
    a_lower <- angle_lower[i]
    if (a_bin == 0 || a_bin == 4) {
      if (a_lower) {
        c(end_pos[i, 1] - end_pos[i, 2], 0)
      } else {
        c(end_pos[i, 2], end_pos[i, 2])
      }
    } else if (a_bin == 1 || a_bin == 5) {
      if (a_lower) {
        c(end_pos[i, 1], end_pos[i, 1])
      } else {
        c(0, end_pos[i, 2] - end_pos[i, 1])
      }
    } else if (a_bin == 2 || a_bin == 6) {
      if (a_lower) {
        c(0, end_pos[i, 2] + end_pos[i, 1])
      } else {
        c(end_pos[i, 1], -end_pos[i, 1])
      }
    } else if (a_bin == 3 || a_bin == 7) {
      if (a_lower) {
        c(-end_pos[i, 2], end_pos[i, 2])
      } else {
        c(end_pos[i, 1] + end_pos[i, 2], 0)
      }
    }
  })
  elbow <- inject(rbind(!!!elbow))
  elbow <- elbow + lines[[2]]
  colnames(elbow) <- c('x', 'y')
  list(lines[[1]], elbow, lines[[2]])
}
with_borderline <- function(xmin, xmax, lines) {
  new_start <- lines[[1]]
  new_start[, 1] <- ifelse(new_start[, 1] == xmin, xmax, xmin)
  c(list(new_start), lines)
}
end_cap <- function(lines, cap) {
  from <- lines[[length(lines) - 1]]
  to <- lines[[length(lines)]]
  d <- to - from
  l <- sqrt(rowSums((d)^2))
  to <- from + d * (l - cap) / l
  lines[[length(lines)]] <- to
  lines
}
zip_points <- function(points) {
  n_lines <- nrow(points[[1]])
  n_joints <- length(points)
  points <- as.data.frame(inject(rbind(!!!points)))
  points$id <- rep(seq_len(n_lines), n_joints)
  points[order(points$id), ]
}
get_end_points <- function(xmin, xmax, ymin, ymax, x, y) {
  xmin_tmp <- xmin - x
  xmax_tmp <- xmax - x
  ymin_tmp <- ymin - y
  ymax_tmp <- ymax - y
  pos <- ifelse(
    xmin_tmp < 0,
    ifelse(ymin_tmp < 0, 'bottomleft', 'topleft'),
    ifelse(ymin_tmp < 0, 'bottomright', 'topright')
  )
  pos <- ifelse(
    ymin_tmp < 0 & ymax_tmp > 0,
    ifelse(xmin_tmp < 0, 'left', 'right'),
    ifelse(
      xmin_tmp < 0 & xmax_tmp > 0,
      ifelse(ymin_tmp < 0, 'bottom', 'top'),
      pos
    )
  )
  x_new <- vswitch(
    pos,
    left = xmax,
    bottomleft = xmax,
    topleft = xmax,
    right = xmin,
    bottomright = xmin,
    topright = xmin,
    top = ifelse(abs(xmin_tmp) < abs(xmax_tmp), xmin, xmax),
    bottom = ifelse(abs(xmin_tmp) < abs(xmax_tmp), xmin, xmax)
  )
  y_new <- vswitch(
    pos,
    bottom = ymax,
    bottomleft = ymax,
    bottomright = ymax,
    top = ymin,
    topleft = ymin,
    topright = ymin,
    left = ifelse(abs(ymin_tmp) < abs(ymax_tmp), ymin, ymax),
    right = ifelse(abs(ymin_tmp) < abs(ymax_tmp), ymin, ymax)
  )
  data_frame0(x = x_new, y = y_new)
}
vswitch <- function(x, ...) {
  cases <- cbind(...)
  cases[cbind(seq_along(x), match(x, colnames(cases)))]
}

font_descent <- function(fontfamily, fontface, fontsize, cex) {
  italic <- fontface >= 3
  bold <- fontface == 2 | fontface == 4
  info <- systemfonts::font_info(fontfamily, italic, bold, fontsize * (cex %||% 1), res = 300)
  as_mm(abs(info$max_descend)*72/300, 'pt', FALSE)
}
