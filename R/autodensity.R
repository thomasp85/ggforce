#' @rdname geom_autohistogram
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_density
#' @export
geom_autodensity <- function(mapping = NULL, data = NULL,
                          stat = "autodensity", position = "floatstack",
                          ...,
                          bw = "nrd0",
                          adjust = 1,
                          kernel = "gaussian",
                          n = 512,
                          trim = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          outline.type = "upper") {
  extra_mapping <- aes(x = .panel_x, y = .panel_y)
  if (is.null(mapping$x)) mapping$x <- extra_mapping$x
  if (is.null(mapping$y)) mapping$y <- extra_mapping$y
  class(mapping) <- 'uneval'

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAutoarea,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      trim = trim,
      na.rm = na.rm,
      ...,
      outline.type = outline.type
    )
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatAutodensity <- ggproto('StatAutodensity', StatDensity,
  setup_params = function(data, params) {
    params$panel_range <- lapply(split(data$y, data$PANEL), function(y) {
      if (length(y) == 0) return()
      range(y, na.rm=TRUE)
    })
    params$panel_count <- lapply(split(data$y, data$PANEL), function(y)length(y[is.finite(y)]))

    params
  },
  compute_group = function(self, data, scales, bw = "nrd0", adjust = 1, kernel = "gaussian",
                           n = 512, trim = FALSE, na.rm = FALSE, panel_range = list(), panel_count = list()) {
    if (scales$x$is_discrete()) {
      bins <- split(data, factor(data$x, levels = seq_len(scales$x$range_c$range[2])))
      binned <- rbind_dfs(lapply(as.integer(names(bins)), function(x) {
        count <- nrow(bins[[x]])
        pad <- if (count == 0) 0.5 else 0.3
        pad <- pad * c(-1, 1)
        new_data_frame(list(
          x = x + pad,
          density = count / nrow(data)
        ))
      }))
      binned$scaled <- binned$density / max(binned$density)
      binned$ndensity <- binned$density / max(binned$density)
      binned$count <- binned$density * nrow(data)
      binned$n <- nrow(data)
    } else {
      binned <- ggproto_parent(StatDensity, self)$compute_group(
        data, scales, bw = bw, adjust = adjust, kernel = kernel,
        n = n, trim = trim, na.rm = na.rm
      )
    }
    panel_range <- panel_range[[data$PANEL[1]]]
    panel_count <- panel_count[[data$PANEL[1]]]
    ymin <- panel_range[1]
    binned$y <- ymin + binned$ndensity * (panel_range[2] - panel_range[1]) * nrow(data) / panel_count

    binned$ymin <- ymin
    binned$ymax <- binned$y
    binned
  },

  default_aes = aes(weight = 1),
  required_aes = c("x", "y")
)
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomAutoarea <- ggproto('GeomAutoarea', GeomArea,
  setup_data = function(data, params) {
    data[order(data$PANEL, data$group, data$x), ]
  },
  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE, ...) {
    y_range <- coord$range(panel_params)$y
    y_span <- y_range[2] - y_range[1]
    panel_min <- min(data$ymin)
    panel_span <- max(data$ymax) - panel_min
    data$ymin <- ((data$ymin - panel_min) / panel_span) * y_span * 0.9 + y_range[1]
    data$ymax <- ((data$ymax - panel_min) / panel_span) * y_span * 0.9 + y_range[1]
    ggproto_parent(GeomArea, self)$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      na.rm = na.rm,
      ...
    )
  }
)
