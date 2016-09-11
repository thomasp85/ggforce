#' @include arc_bar.R
NULL

#' Circles based on center and radius
#'
#' This set of stats and geoms makes it possible to draw circles based on a
#' center point and a radius. In contrast to using
#' \code{\link[ggplot2]{geom_point}}, the size of the circles are related to the
#' coordinate system and not to a separate scale. These functions are intended
#' for cartesian coordinate systems and will only produce a true circle if
#' \code{\link[ggplot2]{coord_fixed}} is used.
#'
#' @note If the intend is to draw a bubble chart then use
#' \code{\link[ggplot2]{geom_point}} and map a variable to the size scale
#'
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#' \itemize{
#'  \item{\strong{x0}}
#'  \item{\strong{y0}}
#'  \item{\strong{r}}
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
#'  \item{x, y}{The start coordinates for the segment}
#' }
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE}
#' (the default), is combined with the default mapping at the top level of the
#' plot. You only need to supply mapping if there isn't a mapping defined for
#' the plot.
#'
#' @param data A data frame. If specified, overrides the default data frame
#' defined at the top level of the plot.
#'
#' @param stat The statistical transformation to use on the data for this layer,
#' as a string.
#'
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#'
#' @param n The number of points on the generated path per circle.
#'
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There
#' are three types of arguments you can use here:
#' \itemize{
#'  \item{Aesthetics: to set an aesthetic to a fixed value, like
#'  \code{color = "red"} or \code{size = 3.}}
#'  \item{Other arguments to the layer, for example you override the default
#'  \code{stat} associated with the layer.}
#'  \item{Other arguments passed on to the stat.}
#' }
#'
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#' warning. If \code{TRUE} silently removes missing values.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#' \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#' never includes, and \code{TRUE} always includes.
#'
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#' than combining with them. This is most useful for helper functions that
#' define both data and aesthetics and shouldn't inherit behaviour from the
#' default plot specification, e.g. borders.
#'
#' @param geom, stat Override the default connection between \code{geom_arc} and
#' \code{stat_arc}.
#'
#' @author Thomas Lin Pedersen
#'
#' @name geom_circle
#' @rdname geom_circle
#'
#' @examples
#' # Lets make some data
#' circles <- data.frame(
#'   x0 = rep(1:3, 3),
#'   y0 =  rep(1:3, each=3),
#'   r = seq(0.1, 1, length.out = 9)
#' )
#'
#' # Behold the some circles
#' ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r, fill=r), data=circles)
#'
#' # Use coord_fixed to ensure true circularity
#' ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r, fill=r), data=circles) +
#'   coord_fixed()
#'
#'
#' @seealso \code{\link{geom_arc_bar}} for drawing arcs with fill
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @importFrom grid arcCurvature
#' @export
StatCircle <- ggproto('StatCircle', Stat,
    compute_panel = function(data, scales, n = 360) {
        data$start <- 0
        data$end <- 2*pi
        arcPaths(data, n + 1)
    },

    required_aes = c('x0', 'y0', 'r')
)
#' @rdname geom_circle
#' @importFrom ggplot2 layer
#' @export
stat_circle  <- function(mapping = NULL, data = NULL, geom = "circle",
                      position = "identity", n = 360, na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
    layer(
        stat = StatCircle, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, ...)
    )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomPolygon
#' @export
GeomCircle <- ggproto('GeomCircle', GeomPolygon,
    default_aes = list(colour = 'black', fill = NA, size = 0.5, linetype = 1, alpha = NA)
)
#' @rdname geom_circle
#' @importFrom ggplot2 layer
#' @export
geom_circle <- function(mapping = NULL, data = NULL, stat = "circle",
                        position = "identity", n = 360, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomCircle,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(n = n, na.rm = na.rm, ...))
}
