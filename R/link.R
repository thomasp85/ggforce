#' Link points with paths
#'
#' This set of geoms makes it possible to connect points using straight lines.
#' Before you think \code{\link[ggplot2]{geom_segment}} and
#' \code{\link[ggplot2]{geom_path}}, these functions have some additional tricks
#' up their sleeves. geom_link connects two points in the same way as
#' \code{\link[ggplot2]{geom_segment}} but does so by interpolating multiple
#' points between the two. An additional column called index is added to the
#' data with a sequential progression of the interpolated points. This can be
#' used to map color or size to the direction of the link. geom_link2 uses the
#' same syntax as \code{\link[ggplot2]{geom_path}} but interpolates between the
#' aesthetics given by each row in the data.
#'
#' @section Aesthetics:
#' geom_link understand the following aesthetics (required aesthetics are in
#' bold):
#' \itemize{
#'  \item{\strong{x}}
#'  \item{\strong{y}}
#'  \item{\strong{xend}}
#'  \item{\strong{yend}}
#'  \item{color}
#'  \item{size}
#'  \item{linetype}
#'  \item{alpha}
#'  \item{lineend}
#' }
#' geom_link2 understand the following aesthetics (required aesthetics are in
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
#' @param geom, stat Override the default connection between \code{geom_arc} and
#' \code{stat_arc}.
#'
#' @author Thomas Lin Pedersen
#'
#' @name geom_link
#' @rdname geom_link
#'
#' @examples
#' # Lets make some data
#' lines <- data.frame(
#'   x = c(5, 12, 15, 9, 6),
#'   y = c(17, 20, 4, 15, 5),
#'   xend = c(19, 17, 2, 9, 5),
#'   yend = c(10, 18, 7, 12, 1),
#'   width = c(1, 10, 6, 2, 3),
#'   colour = letters[1:5]
#' )
#'
#' ggplot() + geom_link(aes(x = x, y = y, xend = xend, yend = yend,
#'                          colour = colour, alpha = ..index..,
#'                          size = ..index..),
#'                      data = lines)
#'
#' ggplot() + geom_link2(aes(x = x, y = y, colour = colour, size = width,
#'                           group = 1),
#'                       data = lines, lineend = 'round', n = 500)
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatLink <- ggproto('StatLink', Stat,
    compute_panel = function(data, scales, n = 100) {
        extraCols <- !names(data) %in% c('x', 'y', 'xend', 'yend')
        data <- lapply(seq_len(nrow(data)), function(i) {
            path <- data.frame(
                x = seq(data$x[i], data$xend[i], length.out = n),
                y = seq(data$y[i], data$yend[i], length.out = n),
                index = seq(0, 1, length.out = n),
                group = i
            )
            cbind(path, data[rep(i, n), extraCols])
        })
        do.call(rbind, data)
    },
    required_aes = c('x', 'y', 'xend', 'yend')
)
#' @rdname geom_link
#' @importFrom ggplot2 layer
#' @export
stat_link  <- function(mapping = NULL, data = NULL, geom = "path",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      n = 100, inherit.aes = TRUE, ...) {
    layer(
        stat = StatLink, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, ...)
    )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @importFrom tweenr tween_states
#' @export
StatLink2 <- ggproto('StatLink2', Stat,
    compute_panel = function(data, scales, n = 100) {
        extraCols <- !names(data) %in% c('x', 'y', 'group', 'PANEL')
        data <- data %>% group_by_(~group) %>%
            do({
                interp <- tween_states(split(.[c('x', 'y')], seq_len(nrow(.))),
                                       tweenlength = 1, statelength = 0,
                                       ease = 'linear', nframes = n)
                interp$.frame <- NULL
                interp <- cbind(interp,
                                index = seq(0, 1, length.out = n),
                                group = .$group[1],
                                PANEL = .$PANEL[1])
                nIndex <- seq_len(nrow(interp))
                if (any(extraCols)) {
                    cbind(interp, .[nIndex, extraCols], .interp = nIndex > nrow(.))
                } else {
                    cbind(interp, .interp = nIndex > nrow(.))
                }
            }) %>%
            ungroup()
        as.data.frame(data)
    },
    required_aes = c('x', 'y')
)
#' @rdname geom_link
#' @importFrom ggplot2 layer
#' @export
stat_link2 <- function(mapping = NULL, data = NULL, geom = "path_interpolate",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       n = 100, inherit.aes = TRUE, ...) {
    layer(
        stat = StatLink2, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, ...)
    )
}
#' @rdname geom_link
#' @importFrom ggplot2 layer
#' @export
geom_link <- function(mapping = NULL, data = NULL, stat = "link",
                      position = "identity", arrow = NULL, lineend = "butt",
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                      n = 100, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n,
                        ...))
}
#' @rdname geom_link
#' @importFrom ggplot2 layer
#' @export
geom_link2 <- function(mapping = NULL, data = NULL, stat = "link2",
                      position = "identity", arrow = NULL, lineend = "butt",
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                      n = 100, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPathInterpolate,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n,
                        ...))
}
#' @rdname geom_link
#' @importFrom ggplot2 geom_segment
#' @export
geom_link0 <- geom_segment
