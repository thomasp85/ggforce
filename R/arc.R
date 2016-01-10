#' Arcs based on radius and radians
#'
#' This set of stats and geoms makes it possible to draw circle segments based
#' on a centre point, a radius and a start and end angle (in radians). These
#' functions are intended for cartesian coordinate systems and makes it possible
#' to create circular plot types without using the
#' \code{\link[ggplot2]{coord_polar}} coordinate system.
#'
#' @details An arc is a segment of a line describing a circle. It is the
#' fundamental visual element in donut charts where the length of the segment
#' (and conversely the angular span of the segment) describes the proportion of
#' an entety.
#'
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#' \itemize{
#'  \item{\strong{x0}}
#'  \item{\strong{y0}}
#'  \item{\strong{r}}
#'  \item{\strong{start}}
#'  \item{\strong{end}}
#'  \item{color}
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
#'  \item{xend, yend}{The end coordinates for the segment}
#'  \item{curvature}{The curvature of the curveGrob to match a circle}
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
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#'
#' @param arrow specification for arrow heads, as created by arrow()
#'
#' @param lineend Line end style (round, butt, square)
#'
#' @param ncp The number of control points used to draw the curve. More control
#' points creates a smoother curve.
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
#' @name geom_arc
#' @rdname geom_arc
#'
#' @examples
#' # Lets make some data
#' arcs <- data.frame(
#'   start = seq(0, 2*pi, length.out=11)[-11],
#'   end = seq(0, 2*pi, length.out=11)[-1],
#'   r = rep(1:2, 5)
#' )
#'
#' # Behold the arcs
#' ggplot() + geom_arc(aes(x0=0, y0=0, r=r, start=start, end=end,
#'                         linetype=factor(r)),
#'                     data=arcs)
#'
#' @seealso \code{\link{geom_arc_bar}} for drawing arcs with fill
#'
NULL

#' @importFrom ggplot2 ggproto Stat
#' @importFrom grid arcCurvature
#' @export
StatArc <- ggproto('StatArc', Stat,
                   compute_panel = function(data, scales) {
                       data$x <- data$x0 + data$r*sin(data$start)
                       data$y <- data$y0 + data$r*cos(data$start)
                       data$xend <- data$x0 + data$r*sin(data$end)
                       data$yend <- data$y0 + data$r*cos(data$end)
                       deltaA <- (data$start - data$end)*180/pi
                       data$curvature <- sign(deltaA)*sapply(abs(deltaA), arcCurvature)
                       data
                   },

                   required_aes = c('x0', 'y0', 'r', 'start', 'end')
)
#' @rdname geom_arc
#' @importFrom ggplot2 layer
#' @export
stat_arc  <- function(mapping = NULL, data = NULL, geom = "arc",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
    layer(
        stat = StatArc, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
#' @importFrom ggplot2 ggproto Geom draw_key_path .pt alpha
#' @importFrom grid curveGrob  gList gpar
#' @export
GeomArc <- ggproto('GeomArc', Geom,
                   required_aes = c('x0', 'y0', 'r', 'start', 'end'),
                   default_aes = list(colour = 'black', size = 0.5, linetype = 1, alpha = 1, lineend = 'butt'),
                   draw_key = draw_key_path,

                   draw_panel = function(data, panel_scales, coord, ncp = 5, arrow = NULL,
                                         lineend = "butt", na.rm = FALSE) {
                       if (!coord$is_linear()) {
                           warning("geom_arc is not implemented for non-linear coordinates",
                                   call. = FALSE)
                       }
                       trans <- coord$transform(data, panel_scales)
                       do.call(gList, lapply(seq_len(nrow(trans)), function(i) {
                           curveGrob(trans$x[i], trans$y[i], trans$xend[i], trans$yend[i], default.units = "native",
                                     curvature = data$curvature[i], angle = 90, ncp = ncp, square = FALSE,
                                     squareShape = 1, inflect = FALSE, open = TRUE, gp = gpar(col = alpha(trans$colour[i],
                                                                                                          trans$alpha[i]), lwd = trans$size[i] * .pt, lty = trans$linetype[i],
                                                                                              lineend = trans$lineend[i]), arrow = arrow[i])
                       }))
                   }
)
#' @rdname geom_arc
#' @importFrom ggplot2 layer
#' @export
geom_arc <- function(mapping = NULL, data = NULL, stat = "arc",
                     position = "identity", ncp = 5, arrow = NULL,
                     lineend = "butt", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomArc,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, ncp = ncp, lineend = lineend, na.rm = na.rm, ...))
}
