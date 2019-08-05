#' Draw spirograms based on the radii of the different "wheels" involved
#'
#' This, rather pointless, geom allows you to draw spirograms, as known from the
#' popular drawing toy where lines were traced by inserting a pencil into a hole
#' in a small gear that would then trace around inside another gear. The
#' potential practicality of this geom is slim and it excists mainly for fun and
#' art.
#'
#' @section Aesthetics:
#' stat_spiro and geom_spiro understand the following aesthetics (required
#' aesthetics are in bold):
#'
#' - **R**
#' - **r**
#' - **d**
#' - x0
#' - y0
#' - outer
#' - color
#' - size
#' - linetype
#' - alpha
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The coordinates for the path describing the spirogram}
#'  \item{index}{The progression along the spirogram mapped between 0 and 1}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param n The number of points that should be used to draw a fully closed
#' spirogram. If `revolutions < 1` the actual number of points will be less
#' than this.
#'
#' @param revolutions The number of times the inner gear should revolve around
#' inside the outer gear. If `NULL` the number of revolutions to reach the
#' starting position is calculated and used.
#'
#' @name geom_spiro
#' @rdname geom_spiro
#'
#' @examples
#' # Basic usage
#' ggplot() +
#'   geom_spiro(aes(R = 10, r = 3, d = 5))
#'
#' # Only draw a portion
#' ggplot() +
#'   geom_spiro(aes(R = 10, r = 3, d = 5), revolutions = 1.2)
#'
#' # Let the inner gear circle the outside of the outer gear
#' ggplot() +
#'   geom_spiro(aes(R = 10, r = 3, d = 5, outer = TRUE))
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom MASS fractions
#' @export
StatSpiro <- ggproto('StatSpiro', Stat,
  compute_layer = function(self, data, params, panels) {
    if (is.null(data)) return(data)
    if (is.null(data$outer)) data$outer <- FALSE
    if (is.null(data$x0)) data$x0 <- 0
    if (is.null(data$y0)) data$y0 <- 0
    n_spiro <- nrow(data)
    data$group <- paste0(data$group, '_', seq_len(n_spiro))
    if (is.null(params$revolutions)) {
      revo <- attr(fractions(data$r / data$R), 'fracs')
      revo <- as.numeric(sub('/.*$', '', revo))
    } else {
      revo <- params$revolutions
    }
    data <- data[rep(seq_len(n_spiro), params$n * revo), ]
    data$rho <- unlist(lapply(revo, function(r) {
      seq(0, 2 * pi * r, length.out = params$n * r)
    }))
    data$index <- unlist(lapply(revo, function(r) {
      seq(0, 1, length.out = params$n * r)
    }))
    data$x <- data$x0 + ifelse(
      data$outer,
      (data$R + data$r) * cos(data$rho) - data$d * cos(data$rho * (data$R + data$r) / data$r),
      (data$R - data$r) * cos(data$rho) + data$d * cos(data$rho * (data$R - data$r) / data$r)
    )
    data$y <- data$y0 + ifelse(
      data$outer,
      (data$R + data$r) * sin(data$rho) - data$d * sin(data$rho * (data$R + data$r) / data$r),
      (data$R - data$r) * sin(data$rho) - data$d * sin(data$rho * (data$R - data$r) / data$r)
    )
    data
  },
  required_aes = c('R', 'r', 'd'),
  default_aes = aes(outer = FALSE, x0 = 0, y0 = 0),
  extra_params = c('na.rm', 'n', 'revolutions')
)

#' @rdname geom_spiro
#' @export
stat_spiro <- function(mapping = NULL, data = NULL, geom = 'path',
                       position = 'identity', na.rm = FALSE, n = 500,
                       revolutions = NULL, show.legend = NA, inherit.aes = TRUE,
                       ...) {
  layer(
    stat = StatSpiro, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n = n, revolutions = revolutions, ...)
  )
}

#' @rdname geom_spiro
#' @export
geom_spiro <- function(mapping = NULL, data = NULL, stat = 'spiro',
                       position = 'identity', arrow = NULL, n = 500,
                       lineend = 'butt', na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPath,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n = n, ...)
  )
}
