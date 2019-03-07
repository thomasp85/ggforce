#' Draw polygons with expansion/contraction and/or rounded corners
#'
#' This geom is a cousin of [ggplot2::geom_polygon()] with the added
#' possibility of expanding or contracting the polygon by an absolute amount
#' (e.g. 1 cm). Furthermore, it is possible to round the corners of the polygon,
#' again by an absolute amount. The resulting geom reacts to resizing of the
#' plot, so the expansion/contraction and corner radius will not get distorted.
#' If no expansion/contraction or corner radius is specified, the geom falls
#' back to `geom_polygon` so there is no performance penality in using this
#' instead of `geom_polygon`.
#'
#' @note Some settings can result in the dissappearance of polygons,
#' specifically when contracting or rounding corners with a relatively large
#' amount. Also note that x and y scale limits does not take expansion into
#' account and the resulting polygon might thus not fit into the plot.
#'
#' @section Aesthetics:
#' geom_shape understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x**
#' - **y**
#' - color
#' - fill
#' - group
#' - size
#' - linetype
#' - alpha
#'
#' @inheritParams ggplot2::geom_polygon
#'
#' @param expand A numeric or unit vector of length one, specifying the
#' expansion amount. Negative values will result in contraction instead. If the
#' value is given as a numeric it will be understood as a proportion of the
#' plot area width.
#'
#' @param radius As `expand` but specifying the corner radius.
#'
#' @author Thomas Lin Pedersen
#'
#' @name geom_shape
#' @rdname geom_shape
#'
#' @examples
#' shape <- data.frame(
#'   x = c(0.5, 1, 0.75, 0.25, 0),
#'   y = c(0, 0.5, 1, 0.75, 0.25)
#' )
#' # Expand and round
#' ggplot(shape, aes(x = x, y = y)) +
#'   geom_shape(expand = unit(1, 'cm'), radius = unit(0.5, 'cm')) +
#'   geom_polygon(fill = 'red')
#'
#' # Contract
#' ggplot(shape, aes(x = x, y = y)) +
#'   geom_polygon(fill = 'red') +
#'   geom_shape(expand = unit(-1, 'cm'))
#'
#' # Only round corners
#' ggplot(shape, aes(x = x, y = y)) +
#'   geom_polygon(fill = 'red') +
#'   geom_shape(radius = unit(1, 'cm'))
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomShape <- ggproto('GeomShape', GeomPolygon,
  draw_panel = function(data, panel_params, coord, expand = 0, radius = 0) {
    n <- nrow(data)
    if (n == 1 && expand == 0) {
      return(zeroGrob())
    }
    munched <- coord_munch(coord, data, panel_params)
    munched <- munched[order(munched$group), ]
    if (!is.integer(munched$group)) {
      munched$group <- match(munched$group, unique(munched$group))
    }

    # For gpar(), there is one entry per polygon (not one entry per point).
    # We'll pull the first value from each group, and assume all these values
    # are the same within each group.
    first_idx <- !duplicated(munched$group)
    first_rows <- munched[first_idx, ]

    shapeGrob(munched$x, munched$y,
      default.units = 'native',
      id = munched$group, expand = expand, radius = radius,
      gp = gpar(
        col = first_rows$colour,
        fill = alpha(first_rows$fill, first_rows$alpha),
        lwd = first_rows$size * .pt,
        lty = first_rows$linetype
      )
    )
  },
  extra_params = c('expand', 'radius')
)

#' @rdname geom_shape
#' @export
geom_shape <- function(mapping = NULL, data = NULL, stat = 'identity',
                       position = 'identity', expand = 0, radius = 0, ...,
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
                      default.units = 'npc', name = NULL, gp = gpar(),
                      vp = NULL) {
  if (as.numeric(expand) == 0 && as.numeric(radius) == 0) {
    grob <- polygonGrob(
      x = x, y = y, id = id, id.lengths = id.lengths,
      default.units = default.units, name = name, gp = gp, vp = vp
    )
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
  grob(
    x = x, y = y, id = id, expand = expand, radius = radius, name = name,
    gp = gp, vp = vp, cl = 'shape'
  )
}
#' @importFrom grid convertX convertY convertWidth
#' @importFrom polyclip polyoffset polylineoffset
#' @export
makeContent.shape <- function(x) {
  id.length <- lengths(split(seq_along(x$id), x$id))
  type <- ifelse(id.length == 1, 'point',
                 ifelse(id.length == 2, 'line', 'polygon'))
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
      poly$polygon <- lapply(poly$polygon, polyoffset, delta = expand,
                             jointype = 'miter', miterlim = 1000)
    }
    if (expand > 0) {
      if (!is.null(poly$line)) {
        poly$line <- lapply(poly$line, polylineoffset, delta = expand,
                            jointype = 'square', endtype = 'opensquare')
      }
      poly$point <- pointoffset(poly$point, expand, type = 'square')
    }
  }
  if (radius != 0) {
    if (!is.null(poly$polygon)) {
      not_empty <- lengths(poly$polygon) != 0
      poly$polygon[not_empty] <- lapply(poly$polygon[not_empty], polyoffset,
                                        delta = radius, jointype = 'round')
    }
    if (expand > 0) {
      if (!is.null(poly$line)) {
        not_empty <- lengths(poly$line) != 0
        poly$line[not_empty] <- lapply(poly$line[not_empty], polyoffset,
                                       delta = radius, jointype = 'round')
      }
      if (!is.null(poly$point)) {
        not_empty <- lengths(poly$point) != 0
        poly$point[not_empty] <- lapply(poly$point[not_empty], polyoffset,
                                        delta = radius, jointype = 'round')
      }
    } else {
      if (!is.null(poly$line)) {
        poly$line <- lapply(poly$line, polylineoffset, delta = radius,
                            jointype = 'round', endtype = 'openround')
      }
      poly$point <- pointoffset(poly$point, radius, type = 'circle')
    }
  }
  polygons[type == 'polygon'] <- lapply(poly$polygon, function(d) if (length(d) == 0) list() else d[[1]])
  polygons[type == 'line'] <- lapply(poly$line, function(d) if (length(d) == 0) list() else d[[1]])
  polygons[type == 'point'] <- lapply(poly$point, function(d) if (length(d) == 0) list() else d[[1]])
  x$id <- rep(seq_along(polygons), sapply(polygons, function(p) length(p$x)))
  x_new <- unlist(lapply(polygons, `[[`, 'x'))
  y_new <- unlist(lapply(polygons, `[[`, 'y'))
  if (length(x_new) == 0) return(nullGrob())
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
      square <- list(x = c(-delta, -delta, delta, delta),
                     y = c(-delta, delta, delta, -delta))
      x <- split(rep(sapply(A, `[[`, 'x'), each = 4) + square$x,
                 rep(seq_along(A), each = 4))
      y <- split(rep(sapply(A, `[[`, 'y'), each = 4) + square$y,
                 rep(seq_along(A), each = 4))
      lapply(Map(list, x = x, y = y), list)
    },
    circle = {
      detail <- 100
      radi <- seq(0, 2 * pi, length.out = detail + 1)[-(detail + 1)]
      circle <- list(x = cos(radi) * delta, y = sin(radi) * delta)
      x <- split(rep(sapply(A, `[[`, 'x'), each = detail) + circle$x,
                 rep(seq_along(A), each = detail))
      y <- split(rep(sapply(A, `[[`, 'y'), each = detail) + circle$y,
                 rep(seq_along(A), each = detail))
      lapply(Map(list, x = x, y = y), list)
    }
  )
}
