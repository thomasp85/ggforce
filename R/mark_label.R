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
  if (!is.null(ghosts)) {
    keep <- !is.na(ghosts$x) & !is.na(ghosts$y)
    if (sum(keep) > 0) {
      ghosts$x <- convertX(ghosts$x[keep], 'mm', TRUE)
      ghosts$y <- convertY(ghosts$y[keep], 'mm', TRUE)
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
    polygons[[length(polygons) + 1]] <- rect
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
    lab$x <- unit(pos[1], 'mm')
    lab$y <- unit(pos[2], 'mm')
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

# Applies the deprecated `label.fill`/`label.colour` arguments on top of the
# `style` aesthetic, resolving the `"inherit"` keyword family against the
# mark's own fill/colour (#331)
apply_label_style <- function(label, label.fill, label.colour) {
  if (is.null(label) || is.null(label$style) ||
      (is.null(label.fill) && is.null(label.colour))) {
    return(label)
  }
  styles <- lapply(seq_len(nrow(label)), function(i) {
    style <- label$style[i]
    if (!is.null(label.fill)) {
      style <- marquee::modify_style(
        style, "body",
        background = resolve_label_inherit(label.fill, label$fill[i], label$colour[i], 'fill')
      )
    }
    if (!is.null(label.colour)) {
      style <- marquee::modify_style(
        style, "body",
        color = resolve_label_inherit(label.colour, label$fill[i], label$colour[i], 'colour')
      )
    }
    style
  })
  label$style <- do.call(c, styles)
  label
}
resolve_label_inherit <- function(value, fill, colour, field) {
  if (!is.character(value)) return(value)
  if (value == 'inherit') {
    return(if (field == 'fill') fill else colour)
  }
  if (value == 'inherit_fill') return(fill)
  if (grepl('inherit_col', value)) return(colour)
  value
}

#' A marquee styleset for geom_mark_*()
#'
#' This function constructs stylesets with defaults appropriate for the text
#' boxes in the geom_mark_*() family of geoms.
#'
#' @param base_size The base size for the text (depending on the style, some
#' text may be larger, e.g. headings)
#' @param background The background color of the textbox
#' @param color The text color in the textbox
#' @param header_color The text color for headings in the textbox
#' @param lineheight The base lineheight for the text in the textbox
#' @param margin The margin around the text in the textbox
#' @param ... Further arguments to pass on to [marquee::classic_style()]
#'
#' @return A marquee style_set object with a length corresponding to the length
#' of the input
#'
#' @export
mark_style <- function(
  base_size = 10,
  background = "white",
  color = "black",
  header_color = color,
  lineheight = 1,
  margin = marquee::trbl(grid::unit(2, "mm")),
  ...) {
  mark_style <- marquee::modify_style(
    marquee::classic_style(base_size = base_size, color = color, margin = marquee::trbl(0), lineheight = lineheight, ...),
    "body",
    lineheight = NULL,
    padding = marquee::skip_inherit(margin),
    background = background
  )
  for (tag in paste0("h", 1:6)) {
    mark_style <- marquee::modify_style(
      mark_style,
      tag,
      lineheight = NULL,
      color = header_color,
      margin = marquee::trbl(0, 0, marquee::em(0.2))
    )
  }
  mark_style
}

