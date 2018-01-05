repel_labels <- function(boxes, points, shapes, dist, xlim, ylim) {
    if (!is.list(shapes)) shapes <- list(shapes)
    shape_points <- lapply(shapes, sample_poly_points, n = 100)
    points <- do.call(rbind, c(shape_points, list(points)))
    ggrepel:::repel_boxes(points, dist, dist, boxes, xlim, ylim, force = 5)
}
#' @importFrom mgcv in.out
sample_poly_points <- function(polygon, n) {
    x_range <- range(polygon[,1])
    y_range <- range(polygon[,2])
    points <- cbind(runif(n*5, min = x_range[1], max = x_range[2]),
                    runif(n*5, min = y_range[1], max = y_range[2]))
    within <- mgcv::in.out(polygon, points)
    if (sum(within) < n) {
        rel <- ceiling(n/sum(within))
        points <- cbind(runif(n*5*rel, min = x_range[1], max = x_range[2]),
                        runif(n*5*rel, min = y_range[1], max = y_range[2]))
        within <- mgcv::in.out(polygon, points)
    }
    points <- points[within, ]
    points[sample(nrow(points), n, nrow(points) < n), ]
}
#' @importFrom ggplot2 margin
#' @importFrom grid valid.just textGrob nullGrob viewport grobWidth grobHeight rectGrob gpar grid.layout unit gTree gList
labelGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                      description = NULL, width = NULL, min_width = 50,
                      default.units = 'mm', just = c(0, 0.5), pad = margin(2,2,2,2,'mm'),
                      gp = gpar(), vp = NULL) {
    gps <- split_label_gp(gp)
    width <- as_mm(width, default.units)
    min_width <- as_mm(min_width, default.units)
    pad <- as_mm(pad, default.units)
    pad[c(1, 3)] <- as_mm(pad[c(1,3)], default.units, FALSE)
    just <- valid.just(just)
    if (!is.null(label) && !is.na(label)) {
        if (!is.null(width)) {
            label <- wrap_text(label, gps$lab, width - pad[2] - pad[4])
        }
        lab_grob <- textGrob(label, x = just[1], y = just[2], just = just, gp = gps$lab)
    } else {
        lab_grob <- nullGrob()
    }
    if (!is.null(width)) {
        width <- max(width, min_width) - pad[2] - pad[4]
    } else {
        width <- max(as_mm(grobWidth(lab_grob)), min_width) - pad[2] - pad[4]
    }
    if (!is.null(description) && !is.na(description)) {
        description <- wrap_text(description, gps$desc, width)
        desc_grob <- textGrob(description, x = just[1], y = just[2], just = just, gp = gps$desc)
    } else {
        desc_grob <- nullGrob()
    }
    bg_grob <- rectGrob(gp = gps$rect)
    lab_height <- as_mm(grobHeight(lab_grob), width = FALSE)
    desc_height <- as_mm(grobHeight(desc_grob), width = FALSE)
    sep_height <- if (lab_height > 0 && lab_height > 0) as_mm(unit(1, 'lines'), width = FALSE)/2 else 0
    vp <- viewport(
        x = x,
        y = y,
        width = unit(width + pad[2] + pad[4], 'mm'),
        height = unit(pad[1] + pad[3] + lab_height + desc_height + sep_height, 'mm'),
        layout = grid.layout(
            5, 3,
            widths = unit(c(pad[2], width, pad[4]), 'mm'),
            heights = unit(c(pad[1], lab_height, sep_height, desc_height, pad[3]), 'mm')
        )
    )
    lab_grob$vp <- viewport(layout.pos.col = 2, layout.pos.row = 2)
    desc_grob$vp <- viewport(layout.pos.col = 2, layout.pos.row = 4)
    gTree(children = gList(bg_grob, lab_grob, desc_grob), vp = vp, cl = 'mark_label')
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
    if (width) convertWidth(x, 'mm', TRUE)
    else convertHeight(x, 'mm', TRUE)
}
#' @importFrom grid gpar
split_label_gp <- function(gp) {
    rect_gp <- gpar(col = NA)
    lab_gp <- gpar()
    desc_gp <- gpar()
    if (!is.null(gp$fill)) rect_gp$fill <- gp$fill
    if (!is.null(gp$col)) {
        col <- rep(gp$col, length.out = 2)
        lab_gp$col <- col[1]
        desc_gp$col <- col[2]
    }
    if (!is.null(gp$fontsize)) {
        fontsize <- rep(gp$fontsize, length.out = 2)
        lab_gp$fontsize <- fontsize[1]
        desc_gp$fontsize <- fontsize[2]
    }
    if (!is.null(gp$fontfamily)) {
        fontfamily <- rep(gp$fontfamily, length.out = 2)
        lab_gp$fontfamily <- fontfamily[1]
        desc_gp$fontfamily <- fontfamily[2]
    }
    if (!is.null(gp$fontface)) {
        fontface <- rep(gp$fontface, length.out = 2)
        lab_gp$fontface <- fontface[1]
        desc_gp$fontface <- fontface[2]
    }
    if (!is.null(gp$lineheight)) {
        lineheight <- rep(gp$lineheight, length.out = 2)
        lab_gp$lineheight <- lineheight[1]
        desc_gp$lineheight <- lineheight[2]
    }
    list(rect = rect_gp, lab = lab_gp, desc = desc_gp)
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
    end_angle <- atan2(end_pos[, 2], end_pos[, 1]) %% (2*pi)
    angle_bin <- end_angle %/% (pi/4)
    angle_lower <- end_angle %% (pi/4) < 0.5
    elbow <- do.call(rbind, lapply(seq_along(angle_bin), function(i) {
        a_bin <- angle_bin[i]
        a_lower <- angle_lower[i]
        if (a_bin == 0 || a_bin == 4) {
            if (a_lower) c(end_pos[i,1] - end_pos[i,2], 0)
            else c(end_pos[i,2], end_pos[i,2])
        } else if (a_bin == 1 || a_bin == 5) {
            if (a_lower) c(end_pos[i,1], end_pos[i,1])
            else c(0, end_pos[i,2] - end_pos[i,1])
        } else if (a_bin == 2 || a_bin == 6) {
            if (a_lower) c(0, end_pos[i,2] + end_pos[i,1])
            else c(end_pos[i,1], -end_pos[i,1])
        } else if (a_bin == 3 || a_bin == 7) {
            if (a_lower) c(-end_pos[i,2], end_pos[i,2])
            else c(end_pos[i,1] + end_pos[i,2], 0)
        }
    }))
    elbow <- elbow + lines[[2]]
    colnames(elbow) <- c('x', 'y')
    list(lines[[1]], elbow, lines[[2]])
}
with_borderline <- function(xmin, xmax, lines) {
    new_start <- lines[[1]]
    new_start[, 1] <- ifelse(new_start[,1] == xmin, xmax, xmin)
    c(list(new_start), lines)
}
zip_points <- function(points) {
    n_lines <- nrow(points[[1]])
    n_joints <- length(points)
    points <- as.data.frame(do.call(rbind, points))
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
    data.frame(x = x_new, y = y_new)
}
vswitch <- function(x, ...) {
    cases <- cbind(...)
    cases[cbind(seq_along(x), match(x, colnames(cases)))]
}
