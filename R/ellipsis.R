
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @importFrom grid arcCurvature
#' @export
StatCircle <- ggproto('StatCircle', Stat,
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
