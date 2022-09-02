#' @include arc_bar.R
#' @include shape.R
NULL

#' Circles based on center and radius
#'
#' This set of stats and geoms makes it possible to draw circles based on a
#' center point and a radius. In contrast to using
#' [ggplot2::geom_point()], the size of the circles are related to the
#' coordinate system and not to a separate scale. These functions are intended
#' for cartesian coordinate systems and will only produce a true circle if
#' [ggplot2::coord_fixed()] is used.
#'
#' @note If the intend is to draw a bubble chart then use
#' [ggplot2::geom_point()] and map a variable to the size scale
#'
#' @section Aesthetics:
#' geom_circle understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **r**
#' - color
#' - fill
#' - linewidth
#' - linetype
#' - alpha
#' - lineend
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The start coordinates for the segment}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param n The number of points on the generated path per full circle.
#'
#' @name geom_circle
#' @rdname geom_circle
#' @seealso [geom_arc_bar()] for drawing arcs with fill
#'
#' @examples
#' # Lets make some data
#' circles <- data.frame(
#'   x0 = rep(1:3, 3),
#'   y0 = rep(1:3, each = 3),
#'   r = seq(0.1, 1, length.out = 9)
#' )
#'
#' # Behold some circles
#' ggplot() +
#'   geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles)
#'
#' # Use coord_fixed to ensure true circularity
#' ggplot() +
#'   geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles) +
#'   coord_fixed()
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid arcCurvature
#' @export
StatCircle <- ggproto('StatCircle', Stat,
  compute_panel = function(data, scales, n = 360) {
    # Avoid some weird interaction if x and y are mapped at the global level
    data$x <- NULL
    data$y <- NULL
    data$start <- 0
    data$end <- 2 * pi
    arcPaths(data, n + 1)
  },

  required_aes = c('x0', 'y0', 'r')
)
#' @rdname geom_circle
#' @export
stat_circle <- function(mapping = NULL, data = NULL, geom = 'circle',
                        position = 'identity', n = 360, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatCircle, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomCircle <- ggproto('GeomCircle', GeomShape,
  default_aes = combine_aes(GeomShape$default_aes, aes(colour = 'black', fill = NA))
)
#' @rdname geom_circle
#' @inheritParams geom_shape
#' @export
geom_circle <- function(mapping = NULL, data = NULL, stat = 'circle',
                        position = 'identity', n = 360, expand = 0, radius = 0,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                        ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomCircle,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}
