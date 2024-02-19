#' Create closed b-spline shapes
#'
#' This geom creates closed b-spline curves and draws them as shapes. The
#' closed b-spline is achieved by wrapping the control points rather than the
#' knots. The *0 version uses the [grid::xsplineGrob()] function with
#' `open = FALSE` and can thus not be manipulated as a shape geom in the same
#' way as the base version (expand, contract, etc).
#'
#' @section Aesthetics:
#' geom_bspline_closed understand the following aesthetics (required aesthetics
#' are in bold):
#'
#'  - **x**
#'  - **y**
#'  - color
#'  - fill
#'  - linewidth
#'  - linetype
#'  - alpha
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The coordinates for the path describing the spline}
#'  \item{index}{The progression along the interpolation mapped between 0 and 1}
#' }
#'
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::stat_identity
#'
#' @param n The number of points generated for each spline
#'
#' @author Thomas Lin Pedersen. The C++ code for De Boor's algorithm has been
#' adapted from
#' \href{https://chi3x10.wordpress.com/2009/10/18/de-boor-algorithm-in-c/}{Jason Yu-Tseh Chi implementation}
#'
#' @name geom_bspline_closed
#' @rdname geom_bspline_closed
#'
#' @examples
#' # Create 6 random control points
#' controls <- data.frame(
#'   x = runif(6),
#'   y = runif(6)
#' )
#'
#' ggplot(controls, aes(x, y)) +
#'   geom_polygon(fill = NA, colour = 'grey') +
#'   geom_point(colour = 'red') +
#'   geom_bspline_closed(alpha = 0.5)
#'
#' # The 0 version approximates the correct shape
#' ggplot(controls, aes(x, y)) +
#'   geom_polygon(fill = NA, colour = 'grey') +
#'   geom_point(colour = 'red') +
#'   geom_bspline_closed0(alpha = 0.5)
#'
#' # But only the standard version supports geom_shape operations
#' # Be aware of self-intersections though
#' ggplot(controls, aes(x, y)) +
#'   geom_polygon(fill = NA, colour = 'grey') +
#'   geom_point(colour = 'red') +
#'   geom_bspline_closed(alpha = 0.5, expand = unit(2, 'cm'))
NULL

#' @rdname geom_bspline_closed
#' @export
stat_bspline_closed <- function(mapping = NULL, data = NULL, geom = 'shape',
                                position = 'identity', na.rm = FALSE, n = 100,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatBspline, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, n = n, ...)
  )
}
#' @rdname geom_bspline_closed
#' @export
geom_bspline_closed <- function(mapping = NULL, data = NULL, stat = 'bspline',
                                position = 'identity', n = 100, na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomShape,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, n = n, type = 'closed', ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid xsplineGrob gpar
#' @export
GeomBsplineClosed0 <- ggproto('GeomBspline0', GeomPolygon,
  draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
    coords <- coord$transform(data, panel_scales)
    if (!is.integer(coords$group)) {
      coords$group <- match(coords$group, unique0(coords$group))
    }
    startPoint <- match(unique0(coords$group), coords$group)
    xsplineGrob(coords$x, coords$y,
      id = coords$group, default.units = 'native',
      shape = 1, open = FALSE,
      gp = gpar(
        col = coords$colour[startPoint],
        fill = alpha(coords$fill[startPoint], coords$alpha[startPoint]),
        lwd = (coords$linewidth[startPoint] %||% coords$size[startPoint]) * .pt,
        lty = coords$linetype[startPoint]
      )
    )
  }
)

#' @rdname geom_bspline_closed
#' @export
geom_bspline_closed0 <- function(mapping = NULL, data = NULL, stat = 'identity',
                                 position = 'identity', na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomBsplineClosed0,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, ...)
  )
}
