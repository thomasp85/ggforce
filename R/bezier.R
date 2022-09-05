#' Create quadratic or cubic bezier curves
#'
#' This set of geoms makes it possible to connect points creating either
#' quadratic or cubic beziers. bezier and bezier2 both work by calculating
#' points along the bezier and connecting these to draw the curve. bezier0
#' directly draws the bezier using bezierGrob. In line with the [geom_link()] and
#' [geom_link2()] differences geom_bezier creates the points, assign
#' an index to each interpolated point and repeat the aesthetics for the start
#' point, while geom_bezier2 interpolates the aesthetics between the start and
#' end points.
#'
#' @details
#' Input data is understood as a sequence of data points the first being the
#' start point, then followed by one or two control points and then the end
#' point. More than 4 and less than 3 points per group will throw an error.
#' [grid::bezierGrob()] only takes cubic beziers so if three points are
#' supplied the middle one as duplicated. This, along with the fact that
#' [grid::bezierGrob()] estimates the curve using an x-spline means
#' that the curves produced by geom_bezier and geom_bezier2 deviates from those
#' produced by geom_bezier0. If you want true bezier paths use geom_bezier or
#' geom_bezier2.
#'
#' @section Aesthetics:
#' geom_bezier, geom_bezier2 and geom_bezier0 understand the following aesthetics
#' (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - color
#' - linewidth
#' - linetype
#' - alpha
#' - lineend
#'
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The interpolated point coordinates}
#'  \item{index}{The progression along the interpolation mapped between 0 and 1}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param n The number of points to create for each segment
#'
#' @name geom_bezier
#' @rdname geom_bezier
#'
#' @examples
#' beziers <- data.frame(
#'   x = c(1, 2, 3, 4, 4, 6, 6),
#'   y = c(0, 2, 0, 0, 2, 2, 0),
#'   type = rep(c('cubic', 'quadratic'), c(3, 4)),
#'   point = c('end', 'control', 'end', 'end', 'control', 'control', 'end'),
#'   colour = letters[1:7]
#' )
#' help_lines <- data.frame(
#'   x = c(1, 3, 4, 6),
#'   xend = c(2, 2, 4, 6),
#'   y = 0,
#'   yend = 2
#' )
#'
#' # See how control points affect the bezier
#' ggplot() +
#'   geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
#'                data = help_lines,
#'                arrow = arrow(length = unit(c(0, 0, 0.5, 0.5), 'cm')),
#'                colour = 'grey') +
#'   geom_bezier(aes(x = x, y = y, group = type, linetype = type),
#'               data = beziers) +
#'   geom_point(aes(x = x, y = y, colour = point),
#'              data = beziers)
#'
#' # geom_bezier0 is less exact
#' ggplot() +
#'   geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
#'                data = help_lines,
#'                arrow = arrow(length = unit(c(0, 0, 0.5, 0.5), 'cm')),
#'                colour = 'grey') +
#'   geom_bezier0(aes(x = x, y = y, group = type, linetype = type),
#'                data = beziers) +
#'   geom_point(aes(x = x, y = y, colour = point),
#'              data = beziers)
#'
#' # Use geom_bezier2 to interpolate between endpoint aesthetics
#' ggplot(beziers) +
#'   geom_bezier2(aes(x = x, y = y, group = type, colour = colour))
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatBezier <- ggproto('StatBezier', Stat,
  compute_panel = function(data, scales, n = 100) {
    if (empty_data(data)) return(data)
    nControls <- table(data$group)
    controlRange <- range(nControls)
    if (min(controlRange) < 3 || max(controlRange) > 4) {
      cli::cli_abort(c(
        'Only support for quadratic and cubic beziers',
        i = 'Make sure each group consists of 3 or 4 rows'
      ))
    }
    data <- data[order(data$group), ]
    groups <- unique0(data$group)
    paths <- getBeziers(data$x, data$y, match(data$group, groups), n)
    paths <- data_frame0(
      x = paths$paths[, 1], y = paths$paths[, 2],
      group = groups[paths$pathID]
    )
    paths$index <- rep(seq(0, 1, length.out = n), length(nControls))
    dataIndex <- rep(match(unique0(data$group), data$group), each = n)
    cbind(
      paths,
      data[dataIndex, !names(data) %in% c('x', 'y', 'group'), drop = FALSE]
    )
  },
  required_aes = c('x', 'y'),
  extra_params = c('na.rm', 'n')
)
#' @rdname geom_bezier
#' @export
stat_bezier <- function(mapping = NULL, data = NULL, geom = 'path',
                        position = 'identity', na.rm = FALSE, show.legend = NA,
                        n = 100, inherit.aes = TRUE, ...) {
  layer(
    stat = StatBezier, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, ...)
  )
}
#' @rdname geom_bezier
#' @export
geom_bezier <- function(mapping = NULL, data = NULL, stat = 'bezier',
                        position = 'identity', arrow = NULL, lineend = 'butt',
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                        n = 100, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPath,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, lineend = lineend, na.rm = na.rm, n = n,
      ...
    )
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatBezier2 <- ggproto('StatBezier2', Stat,
  compute_layer = function(self, data, params, panels) {
    if (empty_data(data)) return(data)
    data <- data[order(data$group), ]
    nControls <- table(data$group)
    controlRange <- range(nControls)
    if (min(controlRange) < 3 || max(controlRange) > 4) {
      cli::cli_abort(c(
        'Only support for quadratic and cubic beziers',
        i = 'Make sure each group consists of 3 or 4 rows'
      ))
    }
    groups <- unique0(data$group)
    paths <- getBeziers(data$x, data$y, match(data$group, groups), params$n)
    paths <- data_frame0(
      x = paths$paths[, 1], y = paths$paths[, 2],
      group = groups[paths$pathID]
    )
    paths$index <- rep(seq(0, 1, length.out = params$n), length(nControls))
    dataIndex <- rep(match(unique0(data$group), data$group), each = params$n)
    paths <- cbind(paths, data[dataIndex, 'PANEL', drop = FALSE])
    extraCols <- !names(data) %in% c('x', 'y', 'group', 'PANEL')
    startIndex <- c(1, cumsum(nControls) + 1)[-(length(nControls) + 1)]
    endIndex <- c(startIndex[-1] - 1, nrow(data))
    dataIndex <- c(startIndex, endIndex)
    pathIndex <- match(unique0(data$group), paths$group)
    pathIndex <- c(pathIndex, pathIndex + 1)
    paths$.interp <- TRUE
    paths$.interp[pathIndex] <- FALSE
    if (any(extraCols)) {
      for (i in names(data)[extraCols]) {
        paths[[i]] <- data[[i]][1]
        paths[[i]][pathIndex] <- data[dataIndex, i]
      }
    }
    paths
  },
  required_aes = c('x', 'y'),
  extra_params = c('na.rm', 'n')
)
#' @rdname geom_bezier
#' @export
stat_bezier2 <- function(mapping = NULL, data = NULL, geom = 'path_interpolate',
                         position = 'identity', na.rm = FALSE, show.legend = NA,
                         n = 100, inherit.aes = TRUE, ...) {
  layer(
    stat = StatBezier2, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, ...)
  )
}
#' @rdname geom_bezier
#' @export
geom_bezier2 <- function(mapping = NULL, data = NULL, stat = 'bezier2',
                         position = 'identity', arrow = NULL, lineend = 'butt',
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         n = 100, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPathInterpolate,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, lineend = lineend, na.rm = na.rm, n = n,
      ...
    )
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatBezier0 <- ggproto('StatBezier0', Stat,
  compute_panel = function(data, scales) {
    if (empty_data(data)) return(data)
    data <- data[order(data$group), ]
    nControls <- table(data$group)
    controlRange <- range(nControls)
    if (min(controlRange) < 3 || max(controlRange) > 4) {
      cli::cli_abort(c(
        'Only support for quadratic and cubic beziers',
        i = 'Make sure each group consists of 3 or 4 rows'
      ))
    }
    quadratic <- nControls == 3
    if (any(quadratic)) {
      controlIndex <- c(1, cumsum(nControls) + 1)[-(length(nControls) + 1)]
      extraRows <- controlIndex[quadratic] + 1
      extraRows <- sort(c(seq_len(nrow(data)), extraRows))
      data <- data[extraRows, ]
    }
    data
  },
  required_aes = c('x', 'y')
)
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid bezierGrob gpar
#' @export
GeomBezier0 <- ggproto('GeomBezier0', GeomPath,
  draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          lineend = 'butt', linejoin = 'round', linemitre = 1,
                          na.rm = FALSE) {
    coords <- coord$transform(data, panel_scales)
    if (!is.integer(coords$group)) {
      coords$group <- match(coords$group, unique0(coords$group))
    }
    startPoint <- match(unique0(coords$group), coords$group)
    bezierGrob(coords$x, coords$y,
      id = coords$group, default.units = 'native',
      arrow = arrow,
      gp = gpar(
        col = alpha(coords$colour[startPoint], coords$alpha[startPoint]),
        lwd = (coords$linewidth[startPoint] %||% coords$size[startPoint]) * .pt,
        lty = coords$linetype[startPoint], lineend = lineend,
        linejoin = linejoin, linemitre = linemitre
      )
    )
  }
)
#' @rdname geom_bezier
#' @export
stat_bezier0 <- function(mapping = NULL, data = NULL, geom = 'bezier0',
                         position = 'identity', na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  layer(
    stat = StatBezier0, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
#' @rdname geom_bezier
#' @export
geom_bezier0 <- function(mapping = NULL, data = NULL, stat = 'bezier0',
                         position = 'identity', arrow = NULL, lineend = 'butt',
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomBezier0,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
  )
}
