#' Edge bundles based on control points and tension
#'
#' This set of stats and geoms implements the edge bundling idea proposed by
#' Danny Holten (see refs). Based on a set of control points and a tension a
#' relaxed b-spline is drawn. Often the control points comes from a hierarchical
#' layout and connects edges, but this is not a requirement.
#'
#' @details geom_edge_bundle uses geom_path for the drawing rather than
#' implementing a native xsplineGrob approach. While the latter might be more
#' efficient, it does not allow the user to draw a gradient over the spline.
#' Direct access to the xsplineGrob might be added as a separate geom.
#'
#' @section Aesthetics:
#' geom_edge_bundle understand the following aesthetics (required aesthetics are in
#' bold):
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
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
#'  \item{x, y}{The coordinates for the path describing the spline}
#'  \item{index}{The index of each coordinate useful for drawing a gradient over
#'  the spline}
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
#' @param n The number of points generated for each spline
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
#' @author Thomas Lin Pedersen. The C++ code for De Boor's algorithm has been
#' adapted from
#' \href{https://chi3x10.wordpress.com/2009/10/18/de-boor-algorithm-in-c/}{Jason Yu-Tseh Chi implementation}
#'
#' @references Holten, D. (2006). \emph{Hierarchical edge bundles: visualization
#' of adjacency relations in hierarchical data.} IEEE Transactions on
#' Visualization and Computer Graphics, \strong{12}(5), 741–748.
#' http://doi.org/10.1109/TVCG.2006.147
#'
#' @name geom_edge_bundle
#' @rdname geom_edge_bundle
NULL


#' @importFrom ggplot2 ggproto Stat
#' @importFrom dplyr %>% group_by_ do
#' @export
StatEdgeBundle <- ggproto('StatEdgeBundle', Stat,
    compute_group = function(data, scales, params, tension = 0.8, n = 100) {
        splits <- names(data)[!names(data) %in% c('x', 'y')]
        data %>% group_by_(.dots=splits) %>%
            do({
                .$x <- relax(.$x, tension)
                .$y <- relax(.$y, tension)
                bundle <- getSplines(.$x, .$y, rep(1, nrow(.)), n)$paths
                dat <- as.data.frame(.)[rep(1, n),]
                dat$x <- bundle[, 1]
                dat$y <- bundle[, 2]
                dat$index <- seq_len(n)
                dat
            })
    },
    required_aes = c('x', 'y')
)
#' @rdname geom_edge_bundle
#' @importFrom ggplot2 layer
#' @export
stat_edge_bundle <- function(mapping = NULL, data = NULL, geom = "edge_bundle",
                             position = "identity", na.rm = FALSE, n = 100,
                             show.legend = NA, inherit.aes = TRUE, ...) {
    layer(
        stat = StatEdgeBundle, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n=n, ...)
    )
}
#' @importFrom ggplot2 ggproto GeomPath
#' @export
GeomEdgeBundle <- ggproto('GeomEdgeBundle', GeomPath)
#' @rdname geom_edge_bundle
#' @importFrom ggplot2 layer
#' @export
geom_edge_bundle <- function(mapping = NULL, data = NULL, stat = "edge_bundle",
                     position = "identity", arrow = NULL, n = 100,
                     lineend = "butt", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomEdgeBundle,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n=n, ...))
}
relax <- function(x, strength) {
    strength*x + (1 - strength)*(x[1] + (seq_along(x) - 1)/(length(x) - 1) * (tail(x, 1) - x[1]))
}
