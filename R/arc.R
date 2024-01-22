#' @include arc_bar.R
NULL

#' Arcs based on radius and radians
#'
#' This set of stats and geoms makes it possible to draw circle segments based
#' on a center point, a radius and a start and end angle (in radians). These
#' functions are intended for cartesian coordinate systems and makes it possible
#' to create circular plot types without using the
#' [ggplot2::coord_polar()] coordinate system.
#'
#' @details An arc is a segment of a line describing a circle. It is the
#' fundamental visual element in donut charts where the length of the segment
#' (and conversely the angular span of the segment) describes the proportion of
#' an entety.
#'
#' @section Aesthetics:
#' geom_arc understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **r**
#' - **start**
#' - **end**
#' - color
#' - linewidth
#' - linetype
#' - alpha
#' - lineend
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The start coordinates for the segment}
#'  \item{xend, yend}{The end coordinates for the segment}
#'  \item{curvature}{The curvature of the curveGrob to match a circle}
#' }
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param n the smoothness of the arc. Sets the number of points to use if the
#' arc would cover a full circle
#'
#' @param ncp the number of control points used to draw the arc with curveGrob.
#' Determines how well the arc approximates a circle section
#'
#' @name geom_arc
#' @rdname geom_arc
#' @seealso [geom_arc_bar()] for drawing arcs with fill
#'
#' @examples
#' # Lets make some data
#' arcs <- data.frame(
#'   start = seq(0, 2 * pi, length.out = 11)[-11],
#'   end = seq(0, 2 * pi, length.out = 11)[-1],
#'   r = rep(1:2, 5)
#' )
#'
#' # Behold the arcs
#' ggplot(arcs) +
#'   geom_arc(aes(x0 = 0, y0 = 0, r = r, start = start, end = end,
#'                linetype = factor(r)))
#'
#' # Use the calculated index to map values to position on the arc
#' ggplot(arcs) +
#'   geom_arc(aes(x0 = 0, y0 = 0, r = r, start = start, end = end,
#'                size = after_stat(index)), lineend = 'round')
#'
#' # The 0 version maps directly to curveGrob instead of calculating the points
#' # itself
#' ggplot(arcs) +
#'   geom_arc0(aes(x0 = 0, y0 = 0, r = r, start = start, end = end,
#'                 linetype = factor(r)))
#'
#' # The 2 version allows interpolation of aesthetics between the start and end
#' # points
#' arcs2 <- data.frame(
#'   angle = c(arcs$start, arcs$end),
#'   r = rep(arcs$r, 2),
#'   group = rep(1:10, 2),
#'   colour = sample(letters[1:5], 20, TRUE)
#' )
#'
#' ggplot(arcs2) +
#'   geom_arc2(aes(x0 = 0, y0 = 0, r = r, end = angle, group = group,
#'                 colour = colour), size = 2)
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatArc <- ggproto('StatArc', Stat,
  compute_panel = function(data, scales, n = 360) {
    arcPaths(data, n)
  },

  required_aes = c('x0', 'y0', 'r', 'start', 'end')
)
#' @rdname geom_arc
#' @export
stat_arc <- function(mapping = NULL, data = NULL, geom = 'arc',
                     position = 'identity', na.rm = FALSE, show.legend = NA,
                     n = 360, inherit.aes = TRUE, ...) {
  layer(
    stat = StatArc, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, n = n, ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid curveGrob gList gpar
#' @export
GeomArc <- ggproto('GeomArc', GeomPath)
#' @rdname geom_arc
#' @export
geom_arc <- function(mapping = NULL, data = NULL, stat = 'arc',
                     position = 'identity', n = 360, arrow = NULL,
                     lineend = 'butt', na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomArc,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(arrow = arrow, n = n, lineend = lineend, na.rm = na.rm, ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatArc2 <- ggproto('StatArc2', Stat,
  compute_panel = function(data, scales, n = 360) {
    arcPaths2(data, n)
  },

  required_aes = c('x0', 'y0', 'r', 'group', 'end')
)
#' @rdname geom_arc
#' @export
stat_arc2 <- function(mapping = NULL, data = NULL, geom = 'path_interpolate',
                      position = 'identity', na.rm = FALSE, show.legend = NA,
                      n = 360, inherit.aes = TRUE, ...) {
  layer(
    stat = StatArc2, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, n = n, ...)
  )
}
#' @rdname geom_arc
#' @export
geom_arc2 <- function(mapping = NULL, data = NULL, stat = 'arc2',
                      position = 'identity', n = 360, arrow = NULL,
                      lineend = 'butt', na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPathInterpolate,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(arrow = arrow, n = n, lineend = lineend, na.rm = na.rm, ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid arcCurvature
#' @export
StatArc0 <- ggproto('StatArc0', Stat,
  compute_panel = function(data, scales) {
    data$x <- data$x0 + data$r * sin(data$start)
    data$y <- data$y0 + data$r * cos(data$start)
    data$xend <- data$x0 + data$r * sin(data$end)
    data$yend <- data$y0 + data$r * cos(data$end)
    deltaA <- (data$start - data$end) * 180 / pi
    data$curvature <- sign(deltaA) * sapply(abs(deltaA), arcCurvature)
    data
  },

  required_aes = c('x0', 'y0', 'r', 'start', 'end')
)
#' @rdname geom_arc
#' @export
stat_arc0 <- function(mapping = NULL, data = NULL, geom = 'arc0',
                      position = 'identity', na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatArc0, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid curveGrob  gList gpar
#' @export
GeomArc0 <- ggproto('GeomArc0', Geom,
  required_aes = c('x0', 'y0', 'r', 'start', 'end'),
  default_aes = aes(colour = 'black', linewidth = 0.5, linetype = 1, alpha = 1),
  draw_key = draw_key_path,

  draw_panel = function(self, data, panel_scales, coord, ncp = 5, arrow = NULL,
                          lineend = 'butt', na.rm = FALSE) {
    if (!coord$is_linear()) {
      cli::cli_abort('{.fn {snake_class(self)}} is not implemented for non-linear coordinates')
    }
    trans <- coord$transform(data, panel_scales)
    grobs <- lapply(seq_len(nrow(trans)), function(i) {
      curveGrob(trans$x[i], trans$y[i], trans$xend[i], trans$yend[i],
        default.units = 'native',
        curvature = data$curvature[i], angle = 90, ncp = ncp, square = FALSE,
        squareShape = 1, inflect = FALSE, open = TRUE, gp = gpar(
          col = alpha(
            trans$colour[i],
            trans$alpha[i]
          ), lwd = (trans$linewidth[i] %||% trans$size[i]) * .pt, lty = trans$linetype[i],
          lineend = trans$lineend[i]
        ), arrow = arrow[i]
      )
    })
    inject(gList(!!!grobs))
  },
  rename_size = TRUE,
  non_missing_aes = "size"
)
#' @rdname geom_arc
#' @export
geom_arc0 <- function(mapping = NULL, data = NULL, stat = 'arc0',
                      position = 'identity', ncp = 5, arrow = NULL,
                      lineend = 'butt', na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomArc0,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(
      arrow = arrow, ncp = ncp, lineend = lineend, na.rm = na.rm,
      ...
    )
  )
}
