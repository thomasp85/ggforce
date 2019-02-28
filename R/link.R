#' Link points with paths
#'
#' This set of geoms makes it possible to connect points using straight lines.
#' Before you think [ggplot2::geom_segment()] and
#' [ggplot2::geom_path()], these functions have some additional tricks
#' up their sleeves. geom_link connects two points in the same way as
#' [ggplot2::geom_segment()] but does so by interpolating multiple
#' points between the two. An additional column called index is added to the
#' data with a sequential progression of the interpolated points. This can be
#' used to map color or size to the direction of the link. geom_link2 uses the
#' same syntax as [ggplot2::geom_path()] but interpolates between the
#' aesthetics given by each row in the data.
#'
#' @section Aesthetics:
#' geom_link understand the following aesthetics (required aesthetics are in
#' bold):
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
#' geom_link2 understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x**
#' - **y**
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
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::stat_identity
#'
#' @param n The number of points to create for each segment
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
#' ggplot(lines) +
#'   geom_link(aes(x = x, y = y, xend = xend, yend = yend, colour = colour,
#'                 alpha = stat(index), size = stat(index)))
#'
#' ggplot(lines) +
#'   geom_link2(aes(x = x, y = y, colour = colour, size = width, group = 1),
#'              lineend = 'round', n = 500)
#'
#' # geom_link0 is simply an alias for geom_segment to put the link geoms in
#' # line with the other line geoms with multiple versions. `index` is not
#' # available here
#' ggplot(lines) +
#'   geom_link0(aes(x = x, y = y, xend = xend, yend = yend, colour = colour))
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatLink <- ggproto('StatLink', Stat,
  compute_panel = function(data, scales, n = 100) {
    extraCols <- !names(data) %in% c('x', 'y', 'xend', 'yend', 'group', 'PANEL')
    data <- lapply(seq_len(nrow(data)), function(i) {
      path <- data.frame(
        x = seq(data$x[i], data$xend[i], length.out = n),
        y = seq(data$y[i], data$yend[i], length.out = n),
        index = seq(0, 1, length.out = n),
        group = i
      )
      cbind(path, data[rep(i, n), extraCols, drop = FALSE])
    })
    do.call(rbind, data)
  },
  required_aes = c('x', 'y', 'xend', 'yend')
)
#' @rdname geom_link
#' @export
stat_link <- function(mapping = NULL, data = NULL, geom = 'path',
                      position = 'identity', na.rm = FALSE, show.legend = NA,
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
#' @importFrom tweenr tween_t
#' @export
StatLink2 <- ggproto('StatLink2', Stat,
  compute_panel = function(data, scales, n = 100) {
    extraCols <- !names(data) %in% c('x', 'y', 'group', 'PANEL', 'frame')
    dapply(data, 'group', function(df) {
      n_group <- n * (nrow(df) - 1) + 1
      interp <- tween_t(list(df$x, df$y), n_group)
      interp <- data.frame(x = interp[[1]], y = interp[[2]])
      interp <- cbind(interp,
        index = seq(0, 1, length.out = n_group),
        group = df$group[1],
        PANEL = df$PANEL[1]
      )
      if ('frame' %in% names(df)) interp$frame <- df$frame[1]
      nIndex <- seq_len(nrow(interp))
      if (any(extraCols)) {
        cbind(interp, df[nIndex, extraCols], .interp = nIndex > nrow(df))
      } else {
        cbind(interp, .interp = nIndex > nrow(df))
      }
    })
  },
  required_aes = c('x', 'y')
)
#' @rdname geom_link
#' @export
stat_link2 <- function(mapping = NULL, data = NULL, geom = 'path_interpolate',
                       position = 'identity', na.rm = FALSE, show.legend = NA,
                       n = 100, inherit.aes = TRUE, ...) {
  layer(
    stat = StatLink2, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, ...)
  )
}
#' @rdname geom_link
#' @export
geom_link <- function(mapping = NULL, data = NULL, stat = 'link',
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
#' @rdname geom_link
#' @export
geom_link2 <- function(mapping = NULL, data = NULL, stat = 'link2',
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
#' @rdname geom_link
#' @export
geom_link0 <- geom_segment
