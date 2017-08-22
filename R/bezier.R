#' Create quadratic or cubic bezier curves
#'
#' This set of geoms makes it possible to connect points creating either
#' quadratic or cubic beziers. bezier and bezier2 both work by calculating
#' points along the bezier and connecting these to draw the curve. bezier0
#' directly draws the bezier using bezierGrob and is thus probably more
#' performant. In line with the \code{\link{geom_link}} and
#' \code{\link{geom_link2}} differences geom_bezier creates the points, assign
#' an index to each interpolated point and repeat the aesthetics for the start
#' point, while geom_bezier2 interpolates the aesthetics between the start and
#' end points.
#'
#' @details
#' Input data is understood as a sequence of data points the first being the
#' start point, then followed by one or two control points and then the end
#' point. More than 4 and less than 3 points per group will throw an error.
#' \code{\link[grid]{bezierGrob}} only takes cubic beziers so if three points are
#' supplied the middle one as duplicated. This, along with the fact that
#' \code{\link[grid]{bezierGrob}} estimates the curve using an x-spline means
#' that the curves produced by geom_bezier and geom_bezier2 deviates from those
#' produced by geom_bezier0. If you want true bezier paths use geom_bezier or
#' geom_bezier2.
#'
#' @section Aesthetics:
#' geom_link, geom_link2 and geom_lin0 understand the following aesthetics
#' (required aesthetics are in bold):
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
#'  \item{x, y}{The interpolated point coordinates}
#'  \item{index}{The progression along the interpolation mapped between 0 and 1}
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
#' @param arrow specification for arrow heads, as created by arrow()
#'
#' @param lineend Line end style (round, butt, square)
#'
#' @param n The number of points to create for each segment
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
#' @param geom,stat Override the default connection between \code{geom_arc} and
#' \code{stat_arc}.
#'
#' @author Thomas Lin Pedersen
#'
#' @name geom_bezier
#' @rdname geom_bezier
#'
#' @examples
#' beziers <- data.frame(
#'     x = c(1, 2, 3, 4, 4, 6, 6),
#'     y = c(0, 2, 0, 0, 2, 2, 0),
#'     type = rep(c('cubic', 'quadratic'), c(3, 4)),
#'     point = c('end', 'control', 'end', 'end', 'control', 'control', 'end')
#' )
#' help_lines <- data.frame(
#'     x = c(1, 3, 4, 6),
#'     xend = c(2, 2, 4, 6),
#'     y = 0,
#'     yend = 2
#' )
#' ggplot() + geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
#'                         data = help_lines,
#'                         arrow = arrow(length = unit(c(0, 0, 0.5, 0.5), 'cm')),
#'                         colour = 'grey') +
#'     geom_bezier(aes(x= x, y = y, group = type, linetype = type),
#'                 data = beziers) +
#'     geom_point(aes(x = x, y = y, colour = point), data = beziers)
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatBezier <- ggproto('StatBezier', Stat,
    compute_layer = function(self, data, params, panels) {
        if (is.null(data)) return(data)
        nControls <- table(data$group)
        controlRange <- range(nControls)
        if (min(controlRange) < 3 || max(controlRange) > 4) {
            stop('Only support for quadratic and cubic beziers')
        }
        data <- data[order(data$group),]
        paths <- getBeziers(data$x, data$y, data$group, params$n)
        paths <- data.frame(x = paths$paths[,1], y = paths$paths[,2], group = paths$pathID)
        paths$index <- rep(seq(0, 1, length.out = params$n), length(nControls))
        dataIndex <- rep(match(unique(data$group), data$group), each = params$n)
        cbind(paths, data[dataIndex, !names(data) %in% c('x', 'y', 'group'), drop = FALSE])
    },
    required_aes = c('x', 'y'),
    extra_params = c('na.rm', 'n')
)
#' @rdname geom_bezier
#' @importFrom ggplot2 layer
#' @export
stat_bezier <- function(mapping = NULL, data = NULL, geom = "path",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        n = 100, inherit.aes = TRUE, ...) {
    layer(
        stat = StatBezier, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, ...)
    )
}
#' @rdname geom_bezier
#' @importFrom ggplot2 layer
#' @export
geom_bezier <- function(mapping = NULL, data = NULL, stat = "bezier",
                        position = "identity", arrow = NULL, lineend = "butt",
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                        n = 100, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n,
                        ...))
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatBezier2 <- ggproto('StatBezier2', Stat,
    compute_layer = function(self, data, params, panels) {
        if (is.null(data)) return(data)
        data <- data[order(data$group),]
        nControls <- table(data$group)
        controlRange <- range(nControls)
        if (min(controlRange) < 3 || max(controlRange) > 4) {
            stop('Only support for quadratic and cubic beziers')
        }
        paths <- getBeziers(data$x, data$y, data$group, params$n)
        paths <- data.frame(x = paths$paths[,1], y = paths$paths[,2], group = paths$pathID)
        paths$index <- rep(seq(0, 1, length.out = params$n), length(nControls))
        dataIndex <- rep(match(unique(data$group), data$group), each = params$n)
        paths <- cbind(paths, data[dataIndex, 'PANEL', drop = FALSE])
        extraCols <- !names(data) %in% c('x', 'y', 'group', 'PANEL')
        startIndex <- c(1, cumsum(nControls) + 1)[-(length(nControls)+1)]
        endIndex <- c(startIndex[-1] - 1, nrow(data))
        dataIndex <- c(startIndex, endIndex)
        pathIndex <- match(unique(data$group), paths$group)
        pathIndex <- c(pathIndex, pathIndex + 1)
        paths$.interp <- TRUE
        paths$.interp[pathIndex] <- FALSE
        if (any(extraCols)) {
            for (i in names(data)[extraCols]) {
                paths[[i]] <- NA
                if (is.factor(data[[i]])) {
                    paths[[i]] <- as.factor(paths[[i]])
                    levels(paths[[i]]) <- levels(data[[i]])
                }
                paths[[i]][pathIndex] <- data[dataIndex, i]
            }
        }
        paths
    },
    required_aes = c('x', 'y'),
    extra_params = c('na.rm', 'n')
)
#' @rdname geom_bezier
#' @importFrom ggplot2 layer
#' @export
stat_bezier2  <- function(mapping = NULL, data = NULL, geom = "path_interpolate",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        n = 100, inherit.aes = TRUE, ...) {
    layer(
        stat = StatBezier2, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, ...)
    )
}
#' @rdname geom_bezier
#' @importFrom ggplot2 layer
#' @export
geom_bezier2 <- function(mapping = NULL, data = NULL, stat = "bezier2",
                       position = "identity", arrow = NULL, lineend = "butt",
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                       n = 100, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPathInterpolate,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n,
                        ...))
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatBezier0 <- ggproto('StatBezier0', Stat,
    compute_layer = function(self, data, params, panels) {
        if (is.null(data)) return(data)
        data <- data[order(data$group),]
        nControls <- table(data$group)
        controlRange <- range(nControls)
        if (min(controlRange) < 3 || max(controlRange) > 4) {
            stop('Only support for quadratic and cubic beziers')
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
#' @importFrom ggplot2 ggproto GeomPath alpha
#' @export
GeomBezier0 <- ggproto('GeomBezier0', GeomPath,
    draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          lineend = "butt", linejoin = "round", linemitre = 1,
                          na.rm = FALSE) {
        coords <- coord$transform(data, panel_scales)
        startPoint <- match(unique(coords$group), coords$group)
        bezierGrob(coords$x, coords$y, id = coords$group, default.units = "native",
                   arrow = arrow,
                   gp = gpar(col = alpha(coords$colour[startPoint], coords$alpha[startPoint]),
                             lwd = coords$size[startPoint] * .pt,
                             lty = coords$linetype[startPoint], lineend = lineend,
                             linejoin = linejoin, linemitre = linemitre))
    }
)
#' @rdname geom_bezier
#' @importFrom ggplot2 layer
#' @export
stat_bezier0  <- function(mapping = NULL, data = NULL, geom = "bezier0",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
    layer(
        stat = StatBezier0, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
#' @rdname geom_bezier
#' @importFrom ggplot2 layer
#' @export
geom_bezier0 <- function(mapping = NULL, data = NULL, stat = "bezier0",
                         position = "identity", arrow = NULL, lineend = "butt",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomBezier0,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, ...))
}
