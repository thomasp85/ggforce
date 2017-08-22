#' Draw ellipses based on the coordinate system scale
#'
#' This is a generalisation of \code{\link{geom_circle}} that allows you to draw
#' ellipses at a specified angle and center relative to the coordinate system.
#'
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#' \itemize{
#'  \item{\strong{x0}}
#'  \item{\strong{y0}}
#'  \item{\strong{a}}
#'  \item{\strong{b}}
#'  \item{\strong{angle}}
#'  \item{color}
#'  \item{fill}
#'  \item{size}
#'  \item{linetype}
#'  \item{alpha}
#'  \item{lineend}
#' }
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The coordinates for the points along the ellipse}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param n The number of points to sample along the ellipse.
#'
#' @author Thomas Lin Pedersen
#'
#' @name geom_ellipse
#' @rdname geom_ellipse
#'
#' @examples
#' # Basic usage
#' ggplot() +
#'   geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = 0)) +
#'   coord_fixed()
#'
#' # Rotation
#' # Note that it expects radians and rotates the ellipse counter-clockwise
#' ggplot() +
#'   geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = pi/4)) +
#'   coord_fixed()
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatEllipse<- ggproto('StatEllipse', Stat,
    compute_layer = function(self, data, params, layout) {
        n_ellipses <- nrow(data)
        data <- data[rep(seq_len(n_ellipses), each = params$n), ]
        points <- rep(seq(0, 2*pi, length.out = params$n + 1)[seq_len(params$n)], n_ellipses)
        data$x <- data$x0 + data$a*cos(points)*cos(data$angle) - data$b*sin(points)*sin(data$angle)
        data$y <- data$y0 + data$a*cos(points)*sin(data$angle) + data$b*sin(points)*cos(data$angle)
        data
    },

    required_aes = c('x0', 'y0', 'a', 'b', 'angle'),
    extra_params = c('n', 'na.rm')
)
#' @rdname geom_ellipse
#' @importFrom ggplot2 layer
#' @export
stat_ellipse  <- function(mapping = NULL, data = NULL, geom = "circle",
                         position = "identity", n = 360, na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
    layer(
        stat = StatCircle, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, ...)
    )
}

#' @rdname geom_ellipse
#' @importFrom ggplot2 layer
#' @export
geom_ellipse <- function(mapping = NULL, data = NULL, stat = "ellipse",
                        position = "identity", n = 360, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomCircle,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(n = n, na.rm = na.rm, ...))
}
