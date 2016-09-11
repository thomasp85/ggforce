#' Arcs and wedges as polygons
#'
#' This set of stats and geoms makes it possible to draw arcs and wedges as
#' known from pie and donut charts as well as more specialized plottypes such as
#' sunburst plots.
#'
#' @details An arc bar is the thick version of an arc; that is, a circle segment
#' drawn as a polygon in the same way as a rectangle is a thick version of a
#' line. A wedge is a special case of an arc where the inner radius is 0. As
#' opposed to applying coord_polar to a stacked bar chart, these layers are
#' drawn in cartesian space, which allows for transformations not possible with
#' the native ggplot2 approach. Most notable of these are the option to explode
#' arcs and wedgets away from their center point, thus detaching it from the
#' main pie/donut.
#'
#' @section Aesthetics:
#' geom_arc_bar understand the following aesthetics (required aesthetics are in
#' bold):
#' \itemize{
#'  \item{\strong{x0}}
#'  \item{\strong{y0}}
#'  \item{\strong{r0}}
#'  \item{\strong{r}}
#'  \item{\strong{start} - when using stat_arc_bar}
#'  \item{\strong{end} - when using stat_arc_bar}
#'  \item{\strong{amount} - when using stat_pie}
#'  \item{explode}
#'  \item{color}
#'  \item{fill}
#'  \item{size}
#'  \item{linetype}
#'  \item{alpha}
#' }
#'
#' @section Computed variables:
#' \describe{
#'  \item{x, y}{x and y coordinates for the polygon}
#' }
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
#' @param n The number of points used to draw a full circle. The number of
#' points on each arc will then be calculated as n / span-of-arc
#'
#' @param sep The separation between arcs in pie/donut charts
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
#' @param geom, stat Override the default connection between \code{geom_arc_bar}
#' and \code{stat_arc_bar}.
#'
#' @author Thomas Lin Pedersen
#'
#' @name geom_arc_bar
#' @rdname geom_arc_bar
#'
#' @examples
#' # If you know the angle spans to plot it is easy
#' arcs <- data.frame(
#'   start = seq(0, 2*pi, length.out=11)[-11],
#'   end = seq(0, 2*pi, length.out=11)[-1],
#'   r = rep(1:2, 5)
#' )
#'
#' # Behold the arcs
#' ggplot() + geom_arc_bar(aes(x0=0, y0=0, r0=r-1, r=r, start=start, end=end,
#'                         fill = r),
#'                     data=arcs)
#'
#' # If you got values for a pie chart, use stat_pie
#' states <- c('eaten', "eaten but said you didn't", 'cat took it', 'for tonight',
#'             'will decompose slowly')
#' pie <- data.frame(
#'   state = factor(rep(states, 2), levels = states),
#'   type = rep(c('Pie', 'Donut'), each = 5),
#'   r0 = rep(c(0, 0.8), each = 5),
#'   focus=rep(c(0.2, 0, 0, 0, 0), 2),
#'   amount = c(4,3, 1, 1.5, 6, 6, 1, 2, 3, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Look at the cakes
#' ggplot() + geom_arc_bar(aes(x0=0, y0=0, r0=r0, r=1, amount=amount,
#'                             fill=state, explode=focus),
#'                         data=pie, stat='pie') +
#'   facet_wrap(~type, ncol=1) +
#'   coord_fixed() +
#'   theme_no_axes() +
#'   scale_fill_brewer('', type='qual')
#'
#' @seealso \code{\link{geom_arc}} for drawing arcs as lines
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatArcBar <- ggproto('StatArcBar', Stat,
    compute_panel = function(data, scales, n = 360) {
        arcPaths(data, n)
    },

    required_aes = c('x0', 'y0', 'r0','r', 'start', 'end')
)
#' @rdname geom_arc_bar
#' @importFrom ggplot2 layer
#' @export
stat_arc_bar  <- function(mapping = NULL, data = NULL, geom = "arc_bar",
                          position = "identity", n = 360, na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
    layer(
        stat = StatArcBar, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, ...)
    )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @importFrom dplyr group_by_ do
#' @export
StatPie <- ggproto('StatPie', Stat,
    compute_panel = function(data, scales, n = 360, sep = 0) {
        data <- data %>% group_by_(~x0, ~y0) %>%
            do({
                angles <- cumsum(.$amount)
                seps <- cumsum(sep * seq_along(angles))
                if (max(seps) >= 2*pi) {
                    stop('Total separation exceeds circle circumference. Try lowering "sep"')
                }
                angles <- angles/max(angles) * (2*pi - max(seps))
                data.frame(
                    as.data.frame(.),
                    start = c(0, angles[-length(angles)]) + c(0, seps[-length(seps)]) + sep/2,
                    end = angles + seps -sep/2,
                    stringsAsFactors = FALSE
                )
            })
        arcPaths(as.data.frame(data), n)
    },

    required_aes = c('x0', 'y0', 'r0','r', 'amount')
)
#' @rdname geom_arc_bar
#' @importFrom ggplot2 layer
#' @export
stat_pie  <- function(mapping = NULL, data = NULL, geom = "arc_bar",
                      position = "identity", n = 360, sep = 0, na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE, ...) {
    layer(
        stat = StatPie, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, sep = sep, ...)
    )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomPolygon
#' @export
GeomArcBar <- ggproto('GeomArcBar', GeomPolygon,
    default_aes = list(colour = 'black', fill = NA, size = 0.5, linetype = 1, alpha = NA)
)
#' @rdname geom_arc_bar
#' @importFrom ggplot2 layer
#' @export
geom_arc_bar <- function(mapping = NULL, data = NULL, stat = "arc_bar",
                     position = "identity", n = 360, na.rm = FALSE,
                     show.legend = NA, inherit.aes = TRUE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomArcBar,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, n = n, ...))
}

arcPaths <- function(data, n) {
    trans <- radial_trans(c(0, 1), c(0, 2*pi), pad = 0)
    data <- data[data$start != data$end, ]
    data$nControl <- ceiling(n/(2*pi) * abs(data$end - data$start))
    data$nControl[data$nControl < 3] <- 3
    extraData <- !names(data) %in% c('r0', 'r', 'start', 'end')
    paths <- lapply(seq_len(nrow(data)), function(i) {
        path <- data.frame(
            a = seq(data$start[i], data$end[i], length.out = data$nControl[i]),
            r = data$r[i]
        )
        if ('r0' %in% names(data)) {
            if (data$r0[i] != 0) {
                path <- rbind(
                    path,
                    data.frame(a = rev(path$a), r = data$r0[i])
                )
            } else {
                path <- rbind(
                    path,
                    data.frame(a = data$start[i], r = 0)
                )
            }
        }
        path$group <- i
        path$index <- seq(0, 1, length.out = nrow(path))
        path <- cbind(path, data[rep(i, nrow(path)), extraData])
    })
    paths <- do.call(rbind, paths)
    paths <- cbind(paths[, !names(paths) %in% c('r', 'a')],
                   trans$transform(paths$r, paths$a))
    paths$x <- paths$x + paths$x0
    paths$y <- paths$y + paths$y0
    if ('explode' %in% names(data)) {
        exploded <- data$explode != 0
        if (any(exploded)) {
            exploder <- trans$transform(
                data$explode[exploded],
                data$start[exploded] + (data$end[exploded] - data$start[exploded])/2
            )
            explodedPaths <- paths$group %in% which(exploded)
            exploderInd <- as.integer(factor(paths$group[explodedPaths]))
            paths$x[explodedPaths] <-
                paths$x[explodedPaths] + exploder$x[exploderInd]
            paths$y[explodedPaths] <-
                paths$y[explodedPaths] + exploder$y[exploderInd]
        }
    }
    paths[, !names(paths) %in% c('x0', 'y0', 'exploded')]
}
arcPaths2 <- function(data, n) {
    trans <- radial_trans(c(0, 1), c(0, 2*pi), pad = 0)
    fullCirc <- n/(2*pi)
    extraData <- setdiff(names(data), c('r', 'x0', 'y0', 'end', 'group', 'PANEL'))
    hasExtra <- length(extraData) != 0
    extraTemplate <-  data[NA, extraData, drop = FALSE][1, , drop = FALSE]
    paths <- lapply(split(seq_len(nrow(data)), data$group), function(i) {
        if (length(i) != 2) {
            stop('Arcs must be defined by two end points', call. = FALSE)
        }
        if (data$r[i[1]] != data$r[i[2]] ||
            data$x0[i[1]] != data$x0[i[2]] ||
            data$y0[i[1]] != data$y0[i[2]]) {
            stop('Both end points must be at same radius and with same center', call. = FALSE)
        }
        if (data$end[i[1]] == data$end[i[2]]) return()
        nControl <- ceiling(fullCirc * abs(diff(data$end[i])))
        if (nControl < 3) nControl <- 3
        path <- data.frame(
            a = seq(data$end[i[1]], data$end[i[2]], length.out = nControl),
            r = data$r[i[1]],
            x0 = data$x0[i[1]],
            y0 = data$y0[i[1]],
            group = data$group[i[1]],
            index = seq(0, 1, length.out = nControl),
            .interp = c(FALSE, rep(TRUE, nControl -2), FALSE),
            PANEL = data$PANEL[i[1]]
        )
        if (hasExtra) {
            path <- cbind(path, extraTemplate[rep(1, nControl), , drop = FALSE])
            path[1, extraData] <- data[i[1], extraData]
            path[nControl, extraData] <- data[i[2], extraData]
        }
        path
    })
    paths <- do.call(rbind, paths)
    paths <- cbind(paths[, !names(paths) %in% c('r', 'a')],
                   trans$transform(paths$r, paths$a))
    paths$x <- paths$x + paths$x0
    paths$y <- paths$y + paths$y0
    paths[, !names(paths) %in% c('x0', 'y0')]
}
