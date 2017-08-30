#' Draw shapes defined by the superformula
#'
#' The superformula is a generalisation of the formula defining a superellipse
#' (and, by extension, the formula defining ellipses and circles). By varying
#' the 7 parameters it allows for description of many shapes found in nature.
#'
#' @references Gielis, J. *A generic geometric transformation that unifies a wide range of natural and abstract shapes.* American Journal of Botany, 90, 333 - 338, (2003).
#'
#' @section Aesthetics:
#' geom_regon understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - angle
#' - a
#' - b
#' - m1
#' - m2
#' - n1
#' - n2
#' - n3
#' - revolutions
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
#' @author Thomas Lin Pedersen
#'
#' @name geom_superformula
#' @rdname geom_superformula
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatSuperformula <- ggproto('StatSuperformula', Stat,
    setup_data = function(data, params) {
        if (is.null(data$a)) data$a <- 1
        if (is.null(data$b)) data$b <- data$a
        if (is.null(data$angle)) data$angle <- 0
        if (is.null(data$n1)) data$n1 <- 1
        if (is.null(data$n2)) data$n2 <- data$n1
        if (is.null(data$n3)) data$n3 <- data$n2
        if (is.null(data$m1)) data$m1 <- 0
        if (is.null(data$m2)) data$m2 <- data$m1
        if (is.null(data$revolutions)) data$revolutions <- 1
        data
    },
    compute_layer = function(self, data, params, panels) {
        if (is.null(data)) return(data)
        data$group <- seq_len(nrow(data))
        n_shapes <- nrow(data)
        data <- data[rep(seq_len(n_shapes), each = params$n), ]
        points <- rep(2*pi*(seq_len(params$n) - 1)/params$n, n_shapes) * data$revolutions
        raux <- (abs(cos(data$m1 * points/4)/data$a)^data$n2 + abs(sin(data$m2 * points/4)/data$b)^data$n3)^(-1/data$n1)
        x_tmp <- raux * cos(points)
        y_tmp <- raux * sin(points)
        data$x <- data$x0 + x_tmp*cos(data$angle) - y_tmp*sin(data$angle)
        data$y <- data$y0 + x_tmp*sin(data$angle) + y_tmp*cos(data$angle)
        data
    },
    required_aes = c('x0', 'y0'),
    default_aes = aes(angle = 0, a = 1, b = NA, m1 = 0, m2 = NA, n1 = 1, n2 = NA, n3 = NA, revolutions = 1),
    extra_params = c('na.rm', 'n')
)

#' @rdname geom_superformula
#' @importFrom ggplot2 layer
#' @export
stat_superformula <- function(mapping = NULL, data = NULL, geom = "shape",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, n = 100, ...) {
    layer(
        stat = StatSuperformula, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n = n, ...)
    )
}
#' @rdname geom_superformula
#' @importFrom ggplot2 layer
#' @export
geom_superformula <- function(mapping = NULL, data = NULL, stat = "superformula",
                       position = "identity", na.rm = FALSE, n = 100,
                       show.legend = NA, inherit.aes = TRUE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomShape,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, n = n, ...))
}
