#' B-splines based on control points
#'
#' This set of stats and geoms makes it possible to draw b-splines based on a
#' set of control points. As with [geom_bezier()] there exists several
#' versions each having there own strengths. The base version calculates the
#' b-spline as a number of points along the spline and connects these with a
#' path. The *2 version does the same but in addition interpolates aesthetics
#' between each control point. This makes the *2 version considerably slower
#' so it shouldn't be used unless needed. The *0 version uses
#' [grid::xsplineGrob()] with `shape = 1` to approximate a b-spline.
#'
#' @section Aesthetics:
#' geom_bspline understand the following aesthetics (required aesthetics are in
#' bold):
#'
#'  - **x**
#'  - **y**
#'  - color
#'  - linewidth
#'  - linetype
#'  - alpha
#'  - lineend
#'
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The coordinates for the path describing the spline}
#'  \item{index}{The progression along the interpolation mapped between 0 and 1}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param n The number of points generated for each spline
#' @param type Either `'clamped'` (default) or `'open'`. The former creates a
#' knot sequence that ensures the splines starts and ends at the terminal
#' control points.
#'
#' @author Thomas Lin Pedersen. The C++ code for De Boor's algorithm has been
#' adapted from
#' \href{https://chi3x10.wordpress.com/2009/10/18/de-boor-algorithm-in-c/}{Jason Yu-Tseh Chi implementation}
#'
#' @name geom_bspline
#' @rdname geom_bspline
#'
#' @examples
#' # Define some control points
#' cp <- data.frame(
#'   x = c(
#'     0, -5, -5, 5, 5, 2.5, 5, 7.5, 5, 2.5, 5, 7.5, 5, -2.5, -5, -7.5, -5,
#'     -2.5, -5, -7.5, -5
#'   ),
#'   y = c(
#'     0, -5, 5, -5, 5, 5, 7.5, 5, 2.5, -5, -7.5, -5, -2.5, 5, 7.5, 5, 2.5,
#'     -5, -7.5, -5, -2.5
#'   ),
#'   class = sample(letters[1:3], 21, replace = TRUE)
#' )
#'
#' # Now create some paths between them
#' paths <- data.frame(
#'   ind = c(
#'     7, 5, 8, 8, 5, 9, 9, 5, 6, 6, 5, 7, 7, 5, 1, 3, 15, 8, 5, 1, 3, 17, 9, 5,
#'     1, 2, 19, 6, 5, 1, 4, 12, 7, 5, 1, 4, 10, 6, 5, 1, 2, 20
#'   ),
#'   group = c(
#'     1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7,
#'     7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10
#'   )
#' )
#' paths$x <- cp$x[paths$ind]
#' paths$y <- cp$y[paths$ind]
#' paths$class <- cp$class[paths$ind]
#'
#' ggplot(paths) +
#'   geom_bspline(aes(x = x, y = y, group = group, colour = after_stat(index))) +
#'   geom_point(aes(x = x, y = y), data = cp, color = 'steelblue')
#'
#' ggplot(paths) +
#'   geom_bspline2(aes(x = x, y = y, group = group, colour = class)) +
#'   geom_point(aes(x = x, y = y), data = cp, color = 'steelblue')
#'
#' ggplot(paths) +
#'   geom_bspline0(aes(x = x, y = y, group = group)) +
#'   geom_point(aes(x = x, y = y), data = cp, color = 'steelblue')
#'
NULL


#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatBspline <- ggproto('StatBspline', Stat,
  compute_layer = function(self, data, params, panels) {
    if (empty_data(data)) return(data)
    data <- data[order(data$group), ]
    groups <- unique0(data$group)
    paths <- getSplines(data$x, data$y, match(data$group, groups), params$n,
                        params$type %||% 'clamped')
    paths <- data_frame0(
      x = paths$paths[, 1], y = paths$paths[, 2],
      group = groups[paths$pathID]
    )
    paths$index <- rep(
      seq(0, 1, length.out = params$n),
      length(unique0(data$group))
    )
    dataIndex <- rep(match(unique0(data$group), data$group), each = params$n)
    cbind(
      paths,
      data[dataIndex, !names(data) %in% c('x', 'y', 'group'), drop = FALSE]
    )
  },
  required_aes = c('x', 'y'),
  extra_params = c('na.rm', 'n', 'type')
)
#' @rdname geom_bspline
#' @export
stat_bspline <- function(mapping = NULL, data = NULL, geom = 'path',
                         position = 'identity', na.rm = FALSE, n = 100,
                         type = 'clamped', show.legend = NA, inherit.aes = TRUE,
                         ...) {
  layer(
    stat = StatBspline, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, type = type, ...)
  )
}
#' @rdname geom_bspline
#' @export
geom_bspline <- function(mapping = NULL, data = NULL, stat = 'bspline',
                         position = 'identity', arrow = NULL, n = 100,
                         type = 'clamped', lineend = 'butt', na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPath,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, lineend = lineend, na.rm = na.rm, n = n,
      type = type, ...
    )
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatBspline2 <- ggproto('StatBspline2', Stat,
  compute_layer = function(self, data, params, panels) {
    if (empty_data(data)) return(data)
    data <- data[order(data$group), ]
    nControls <- table(data$group)
    groups <- unique0(data$group)
    paths <- getSplines(data$x, data$y, match(data$group, groups), params$n,
                        params$type %||% 'clamped')
    paths <- data_frame0(
      x = paths$paths[, 1], y = paths$paths[, 2],
      group = groups[paths$pathID]
    )
    paths$index <- rep(
      seq(0, 1, length.out = params$n),
      length(unique0(data$group))
    )
    dataIndex <- rep(match(unique0(data$group), data$group), each = params$n)
    paths <- cbind(paths, data[dataIndex, 'PANEL', drop = FALSE])
    extraCols <- !names(data) %in% c('x', 'y', 'group', 'PANEL')
    pathIndex <- match(unique0(data$group), paths$group)
    pathIndex <- unlist(Map(seq, from = pathIndex, length.out = nControls))
    paths$.interp <- TRUE
    paths$.interp[pathIndex] <- FALSE
    if (any(extraCols)) {
      for (i in names(data)[extraCols]) {
        paths[[i]] <- data[[i]][1]
        paths[[i]][pathIndex] <- data[, i]
      }
    }
    paths
  },
  required_aes = c('x', 'y'),
  extra_params = c('na.rm', 'n', 'type')
)
#' @rdname geom_bspline
#' @export
stat_bspline2 <- function(mapping = NULL, data = NULL, geom = 'path_interpolate',
                          position = 'identity', na.rm = FALSE, n = 100,
                          type = 'clamped', show.legend = NA, inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatBspline2, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, type = type, ...)
  )
}
#' @rdname geom_bspline
#' @export
geom_bspline2 <- function(mapping = NULL, data = NULL, stat = 'bspline2',
                          position = 'identity', arrow = NULL, n = 100,
                          type = 'clamped', lineend = 'butt', na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPathInterpolate,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, lineend = lineend, na.rm = na.rm, n = n,
      type = type, ...
    )
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid xsplineGrob gpar
#' @export
GeomBspline0 <- ggproto('GeomBspline0', GeomPath,
  draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          type = 'clamped', lineend = 'butt', linejoin = 'round',
                          linemitre = 1, na.rm = FALSE) {
    coords <- coord$transform(data, panel_scales)
    if (!is.integer(coords$group)) {
      coords$group <- match(coords$group, unique0(coords$group))
    }
    startPoint <- match(unique0(coords$group), coords$group)
    xsplineGrob(coords$x, coords$y,
      id = coords$group, default.units = 'native',
      shape = 1, arrow = arrow, repEnds = type == 'clamped',
      gp = gpar(
        col = alpha(coords$colour[startPoint], coords$alpha[startPoint]),
        lwd = (coords$linewidth[startPoint] %||% coords$size[startPoint]) * .pt,
        lty = coords$linetype[startPoint], lineend = lineend,
        linejoin = linejoin, linemitre = linemitre
      )
    )
  }
)
#' @rdname geom_bspline
#' @export
stat_bspline0 <- function(mapping = NULL, data = NULL, geom = 'bspline0',
                          position = 'identity', na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, type = 'clamped', ...) {
  layer(
    stat = StatIdentity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, type = type, ...)
  )
}
#' @rdname geom_bspline
#' @export
geom_bspline0 <- function(mapping = NULL, data = NULL, stat = 'identity',
                          position = 'identity', arrow = NULL, lineend = 'butt',
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                          type = 'clamped', ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomBspline0,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, lineend = lineend, na.rm = na.rm, type = type,
      ...
    )
  )
}
