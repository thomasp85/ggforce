#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatErr <- ggproto(
  "StatErr",
  Stat,
  required_aes = c('xmin', 'x', 'xmax', 'ymin', 'y', 'ymax'),
  compute_group = function(data, scales) {
    data_frame0(
      x    = c(data$xmin, data$x),
      xend = c(data$xmax, data$x),
      y    = c(data$y,    data$ymin),
      yend = c(data$y,    data$ymax)
    )[c(matrix(seq_len(2 * nrow(data)), nrow = 2, byrow = TRUE)), ]
  }
)

#' Intervals in vertical and horizontal directions
#'
#' `stat_err` draws intervals of points (`x`, `y`) in vertical (`ymin`, `ymax`)
#' and horizontal (`xmin`, `xmax`) directions.
#'
#' @section Aesthetics:
#' `stat_err()` understands the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x**
#' - **xmin**
#' - **xmax**
#' - **y**
#' - **ymin**
#' - **ymax**
#' - alpha
#' - color
#' - group
#' - linetype
#' - linewidth
#'
#' @examples
#' library(ggplot2)
#'
#' x <- 1:3
#' xmin <- x - 2.5
#' xmax <- x + 2.5
#' d <- data.frame(
#'   x = x, y = x, xmin = xmin, ymin = xmin, xmax = xmax, ymax = xmax,
#'   color = as.factor(x)
#' )
#' ggplot(
#'   d,
#'   aes(x = x, y = y, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = color)
#' ) +
#'   stat_err(size = 2)
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_errorbar
#' @inheritParams ggplot2::stat_identity
#'
#' @importFrom ggplot2 layer
#'
#' @export
stat_err <- function(
  mapping = NULL, data = NULL, geom = "segment", position = "identity",
  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...
) {
  layer(
    stat = StatErr, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, ...)
  )
}
