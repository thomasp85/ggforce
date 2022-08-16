#' Draw horizontal diagonals
#'
#' A diagonal is a bezier curve where the control points are moved
#' perpendicularly towards the center in either the x or y direction a fixed
#' amount. The versions provided here calculates horizontal diagonals meaning
#' that the x coordinate is moved to achieve the control point. The
#' `geom_diagonal()` and `stat_diagonal()` functions are simply helpers that
#' takes care of calculating the position of the control points and then
#' forwards the actual bezier calculations to [geom_bezier()].
#'
#' @section Aesthetics:
#' geom_diagonal and geom_diagonal0 understand the following aesthetics
#' (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - color
#' - size
#' - linetype
#' - alpha
#' - lineend
#'
#' geom_diagonal2 understand the following aesthetics
#' (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - **group**
#' - color
#' - size
#' - linetype
#' - alpha
#' - lineend
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
#' @param strength The proportion to move the control point along the x-axis
#' towards the other end of the bezier curve
#'
#' @name geom_diagonal
#' @rdname geom_diagonal
#'
#' @examples
#' data <- data.frame(
#'   x = rep(0, 10),
#'   y = 1:10,
#'   xend = 1:10,
#'   yend = 2:11
#' )
#'
#' ggplot(data) +
#'   geom_diagonal(aes(x, y, xend = xend, yend = yend))
#'
#' # The standard version provides an index to create gradients
#' ggplot(data) +
#'   geom_diagonal(aes(x, y, xend = xend, yend = yend, alpha = stat(index)))
#'
#' # The 0 version uses bezierGrob under the hood for an approximation
#' ggplot(data) +
#'   geom_diagonal0(aes(x, y, xend = xend, yend = yend))
#'
#' # The 2 version allows you to interpolate between endpoint aesthetics
#' data2 <- data.frame(
#'   x = c(data$x, data$xend),
#'   y = c(data$y, data$yend),
#'   group = rep(1:10, 2),
#'   colour = sample(letters[1:5], 20, TRUE)
#' )
#' ggplot(data2) +
#'   geom_diagonal2(aes(x, y, group = group, colour = colour))
#'
#' # Use strength to control the steepness of the central region
#' ggplot(data, aes(x, y, xend = xend, yend = yend)) +
#'   geom_diagonal(strength = 0.75, colour = 'red') +
#'   geom_diagonal(strength = 0.25, colour = 'blue')
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatDiagonal <- ggproto('StatDiagonal', Stat,
  setup_data = function(data, params) {
    data
  },
  compute_panel = function(data, scales, n = 100, strength = 0.5) {
    if (is.null(data)) return(data)
    data$group <- make_unique(as.character(data$group))
    end <- data
    end$x <- end$xend
    end$y <- end$yend
    data <- rbind(data, end)
    data$xend <- NULL
    data$yend <- NULL
    data <- data[order(data$group), ]
    data <- add_controls(data, strength)
    StatBezier$compute_panel(data, scales, n)
  },
  required_aes = c('x', 'y', 'xend', 'yend'),
  extra_params = c('na.rm', 'n', 'strength')
)
#' @rdname geom_diagonal
#' @export
stat_diagonal <- function(mapping = NULL, data = NULL, geom = 'path',
                          position = 'identity', n = 100, strength = 0.5,
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatDiagonal, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, strength = strength, ...)
  )
}
#' @rdname geom_diagonal
#' @export
geom_diagonal <- function(mapping = NULL, data = NULL, stat = 'diagonal',
                          position = 'identity', n = 100,
                          na.rm = FALSE, strength = 0.5,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPath,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, strength = strength, ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatDiagonal2 <- ggproto('StatDiagonal2', Stat,
  compute_layer = function(self, data, params, panels) {
    if (is.null(data)) return(data)
    data <- data[order(data$group), ]
    data <- add_controls(data, params$strength)
    StatBezier2$compute_layer(data, params, panels)
  },
  required_aes = c('x', 'y'),
  extra_params = c('na.rm', 'n', 'strength')
)
#' @rdname geom_diagonal
#' @export
stat_diagonal2 <- function(mapping = NULL, data = NULL,
                           geom = 'path_interpolate', position = 'identity',
                           na.rm = FALSE, show.legend = NA, n = 100,
                           strength = 0.5, inherit.aes = TRUE, ...) {
  layer(
    stat = StatDiagonal2, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, strength = strength, ...)
  )
}
#' @rdname geom_diagonal
#' @export
geom_diagonal2 <- function(mapping = NULL, data = NULL, stat = 'diagonal2',
                           position = 'identity', arrow = NULL, lineend = 'butt',
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           n = 100, strength = 0.5, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPathInterpolate,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, lineend = lineend, na.rm = na.rm, n = n,
      strength = strength, ...
    )
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatDiagonal0 <- ggproto('StatDiagonal0', Stat,
  compute_panel = function(data, scales, strength = 0.5) {
    if (is.null(data)) return(data)
    data$group <- make_unique(as.character(data$group))
    end <- data
    end$x <- end$xend
    end$y <- end$yend
    data <- rbind(data, end)
    data$xend <- NULL
    data$yend <- NULL
    data <- data[order(data$group), ]
    data <- add_controls(data, strength)
    StatBezier0$compute_panel(data, scales)
  },
  required_aes = c('x', 'y', 'xend', 'yend'),
  extra_params = c('na.rm', 'strength')
)
#' @rdname geom_diagonal
#' @export
stat_diagonal0 <- function(mapping = NULL, data = NULL, geom = 'bezier0',
                           position = 'identity', na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, strength = 0.5,
                           ...) {
  layer(
    stat = StatDiagonal0, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, strength = strength, ...)
  )
}
#' @rdname geom_diagonal
#' @export
geom_diagonal0 <- function(mapping = NULL, data = NULL, stat = 'diagonal0',
                           position = 'identity', arrow = NULL, lineend = 'butt',
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           strength = 0.5, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomBezier0,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow, lineend = lineend, na.rm = na.rm,
      strength = strength, ...
    )
  )
}


add_controls <- function(data, strength) {
  start <- data[c(TRUE, FALSE), ]
  end <- data[c(FALSE, TRUE), ]
  x_diff <- (end$x - start$x) * strength
  mid1 <- start
  mid1$x <- mid1$x + x_diff
  mid2 <- end
  mid2$x <- mid2$x - x_diff
  rbind(start, mid1, mid2, end)
}
