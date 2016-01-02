#' @importFrom ggplot2 ggproto Stat
#' @importFrom grid arcCurvature
StatArc <- ggproto('StatArc', Stat,
                   compute_panel = function(data, scales) {
                       data$x <- data$x0 + data$r*sin(data$start)
                       data$y <- data$y0 + data$r*cos(data$start)
                       data$xend <- data$x0 + data$r*sin(data$end)
                       data$yend <- data$y0 + data$r*cos(data$end)
                       deltaA <- (data$start - data$end)*180/pi
                       data$curvature <- sign(deltaA)*sapply(abs(deltaA), arcCurvature)
                       data
                   },

                   required_aes = c('x0', 'y0', 'r', 'start', 'end')
)
#' @importFrom ggplot2 layer
stat_arc  <- function(mapping = NULL, data = NULL, geom = "arc",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
    layer(
        stat = StatArc, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
#' @importFrom ggplot2 ggproto Geom draw_key_path .pt
#' @importFrom grid curveGrob  gList gpar
GeomArc <- ggproto('GeomArc', Geom,
                   required_aes = c('x0', 'y0', 'r', 'start', 'end'),
                   default_aes = list(colour = 'black', size = 0.5, linetype = 1, alpha = 1, lineend = 'butt'),
                   draw_key = draw_key_path,

                   draw_panel = function(data, panel_scales, coord, ncp = 5, arrow = NULL,
                                         lineend = "butt", na.rm = FALSE) {
                       if (!coord$is_linear()) {
                           warning("geom_arc is not implemented for non-linear coordinates",
                                   call. = FALSE)
                       }
                       trans <- coord$transform(data, panel_scales)
                       do.call(gList, lapply(seq_len(nrow(trans)), function(i) {
                           curveGrob(trans$x[i], trans$y[i], trans$xend[i], trans$yend[i], default.units = "native",
                                     curvature = data$curvature[i], angle = 90, ncp = ncp, square = FALSE,
                                     squareShape = 1, inflect = FALSE, open = TRUE, gp = gpar(col = alpha(trans$colour[i],
                                                                                                          trans$alpha[i]), lwd = trans$size[i] * .pt, lty = trans$linetype[i],
                                                                                              lineend = trans$lineend[i]), arrow = arrow[i])
                       }))
                   }
)
#' @importFrom ggplot2 layer
geom_arc <- function(mapping = NULL, data = NULL, stat = "arc",
                     position = "identity", ncp = 5, arrow = NULL,
                     lineend = "butt", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomArc,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, ncp = ncp, lineend = lineend, na.rm = na.rm, ...))
}
