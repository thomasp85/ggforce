#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomPolygon
#' @export
GeomShape <- ggproto('GeomShape', GeomPolygon,
    draw_panel = function(data, panel_params, coord, expand = 0, radius = 0) {
        data <- coord$transform(data, panel_params)

        # For gpar(), there is one entry per polygon (not one entry per point).
        # We'll pull the first value from each group, and assume all these values
        # are the same within each group.
        first_idx <- !duplicated(data$group)
        first_rows <- data[first_idx, ]

        shapeGrob(data$x, data$y, default.units = "native",
                  id = data$group, expand = expand, radius = radius,
                  gp = gpar(
                      col = first_rows$colour,
                      fill = alpha(first_rows$fill, first_rows$alpha),
                      lwd = first_rows$size * .pt,
                      lty = first_rows$linetype
                  )
                )
    }
)
#' @export
geom_shape <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", expand = 0, radius = 0, ...,
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomShape,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            expand = expand,
            radius = radius,
            ...
        )
    )
}

#' @importFrom grid is.unit grob
shapeGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                      id.lengths = NULL, expand = 0, radius = 0,
                      default.units = "npc", name = NULL, gp = gpar(), vp = NULL) {
    if (as.numeric(expand) == 0 && as.numeric(radius) == 0) {
        grob <- polygonGrob(x = x, y = y, id = id, id.lengths = id.lengths,
                    default.units = default.units, name = name, gp = gp, vp = vp)
        return(grob)
    }
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (!is.unit(expand)) {
        expand <- unit(expand, default.units)
    }
    if (!is.unit(radius)) {
        radius <- unit(radius, default.units)
    }
    if (as.numeric(radius) < 0) {
        stop('radius must be positive', call. = FALSE)
    }
    if (is.null(id)) {
        if (is.null(id.lengths)) {
            id <- rep(1, length(x))
        } else {
            id <- rep(seq_along(id.lengths), id.lengths)
            if (length(id) != length(x)) {
                stop('id.lengths must sum up to the number of points', call. = FALSE)
            }
        }
    }
    x <- x[order(id)]
    y <- y[order(id)]
    grob(x = x, y = y, id = id, expand = expand, radius = radius, name = name,
         gp = gp, vp = vp, cl = "shape")
}
#' @importFrom grid convertX convertY convertWidth
#' @importFrom polyclip polyoffset polylineoffset
#' @export
makeContent.shape <- function(x) {
    id.length <- lengths(split(seq_along(x$id), x$id))
    type <- ifelse(id.length == 1, 'point', ifelse(id.length == 2, 'line', 'polygon'))
    x_new <- convertX(x$x, 'mm', TRUE)
    x_new <- split(x_new, x$id)
    y_new <- convertY(x$y, 'mm', TRUE)
    y_new <- split(y_new, x$id)
    polygons <- Map(list, x = x_new, y = y_new)
    poly <- split(polygons, type)
    expand <- convertWidth(x$expand, 'mm', TRUE)
    radius <- convertWidth(x$radius, 'mm', TRUE)
    expand <- expand - radius
    if (expand != 0) {
        if (!is.null(poly$polygon)) {
            poly$polygon <- unlist(lapply(poly$polygon, polyoffset, delta = expand, jointype = 'miter', miterlim = 1000), recursive = FALSE)
        }
        if (expand > 0) {
            if (!is.null(poly$line)) {
                poly$line <- unlist(lapply(poly$line, polylineoffset, delta = expand, jointype = 'square', endtype = 'opensquare'), recursive = FALSE)
            }
            poly$point <- pointoffset(poly$point, expand, type = 'square')
        }
    }
    if (radius != 0) {
        if (!is.null(poly$polygon)) {
            poly$polygon <- unlist(lapply(poly$polygon, polyoffset, delta = radius, jointype = 'round'), recursive = FALSE)
        }
        if (expand > 0) {
            if (!is.null(poly$line)) {
                poly$line <- unlist(lapply(poly$line, polyoffset, delta = radius, jointype = 'round'), recursive = FALSE)
            }
            poly$point <- polyoffset(poly$point, radius, jointype = 'round')
        } else {
            if (!is.null(poly$line)) {
                poly$line <- unlist(lapply(poly$line, polylineoffset, delta = radius, jointype = 'round', endtype = 'openround'), recursive = FALSE)
            }
            poly$point <- pointoffset(poly$point, radius, type = 'circle')
        }
    }
    polygons[type == 'polygon'] <- poly$polygon
    polygons[type == 'line'] <- poly$line
    polygons[type == 'point'] <- poly$point
    x$id <- rep(seq_along(polygons), sapply(polygons, function(p) length(p$x)))
    x_new <- unlist(lapply(polygons, `[[`, 'x'))
    y_new <- unlist(lapply(polygons, `[[`, 'y'))
    x$x <- unit(x_new, 'mm')
    x$y <- unit(y_new, 'mm')
    x$cl <- 'polygon'
    class(x)[1] <- 'polygon'
    x
}
pointoffset <- function(A, delta, type) {
    if (length(A) == 0) return(A)
    switch(
        type,
        square = {
            square <- list(x = c(-delta, -delta, delta, delta), y = c(-delta, delta, delta, -delta))
            x <- split(rep(sapply(A, `[[`, 'x'), each = 4) + square$x, rep(seq_along(A), each = 4))
            y <- split(rep(sapply(A, `[[`, 'y'), each = 4) + square$y, rep(seq_along(A), each = 4))
            Map(list, x = x, y = y)
        },
        circle = {
            detail <- 100
            radi <- seq(0, 2*pi, length.out = detail + 1)[-(detail + 1)]
            circle <- list(x = cos(radi) * delta, y = sin(radi) * delta)
            x <- split(rep(sapply(A, `[[`, 'x'), each = detail) + circle$x, rep(seq_along(A), each = detail))
            y <- split(rep(sapply(A, `[[`, 'y'), each = detail) + circle$y, rep(seq_along(A), each = detail))
            Map(list, x = x, y = y)
        }
    )
}
