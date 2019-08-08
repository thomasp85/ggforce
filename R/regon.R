#' Draw regular polygons by specifying number of sides
#'
#' This geom makes it easy to construct regular polygons (polygons where all
#' sides and angles are equal) by specifying the number of sides, position, and
#' size. The polygons are always rotated so that they "rest" on a flat side, but
#' this can be changed with the angle aesthetic. The size is based on the radius
#' of their circumcircle and is thus not proportional to their area.
#'
#' @section Aesthetics:
#' geom_regon understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **sides**
#' - **r**
#' - **angle**
#' - color
#' - fill
#' - size
#' - linetype
#' - alpha
#' - lineend
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The coordinates for the corners of the polygon}
#' }
#'
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::stat_identity
#'
#' @name geom_regon
#' @rdname geom_regon
#'
#' @examples
#' ggplot() +
#'   geom_regon(aes(x0 = runif(8), y0 = runif(8), sides = sample(3:10, 8),
#'                  angle = 0, r = runif(8) / 10)) +
#'   coord_fixed()
#'
#' # The polygons are drawn with geom_shape, so can be manipulated as such
#' ggplot() +
#'   geom_regon(aes(x0 = runif(8), y0 = runif(8), sides = sample(3:10, 8),
#'                  angle = 0, r = runif(8) / 10),
#'              expand = unit(1, 'cm'), radius = unit(1, 'cm')) +
#'   coord_fixed()
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatRegon <- ggproto('StatRegon', Stat,
  compute_layer = function(self, data, params, panels) {
    if (is.null(data)) return(data)
    pos <- unlist(lapply(data$sides, function(n) {
      p <- (seq_len(n) - 1) / n
      if (n %% 2 == 0) p <- p + p[2] / 2
      p * 2 * pi
    }))
    data$group <- paste0(data$group, '_', seq_len(nrow(data)))
    data <- data[rep(seq_len(nrow(data)), data$sides), ]
    x_tmp <- sin(pos) * data$r
    y_tmp <- cos(pos) * data$r
    data$x <- data$x0 + x_tmp * cos(data$angle) - y_tmp * sin(data$angle)
    data$y <- data$y0 + x_tmp * sin(data$angle) + y_tmp * cos(data$angle)
    data
  },
  required_aes = c('x0', 'y0', 'sides', 'angle', 'r'),
  extra_params = c('na.rm')
)

#' @rdname geom_regon
#' @export
stat_regon <- function(mapping = NULL, data = NULL, geom = 'shape',
                       position = 'identity', na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatRegon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
#' @rdname geom_regon
#' @export
geom_regon <- function(mapping = NULL, data = NULL, stat = 'regon',
                       position = 'identity', na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomShape,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
