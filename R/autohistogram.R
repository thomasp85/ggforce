#' A distribution geoms that fills the panel and works with discrete and continuous data
#'
#' These versions of the histogram and density geoms have been designed
#' specifically for diagonal plotting with [facet_matrix()]. They differ from
#' [ggplot2::geom_histogram()] and [ggplot2::geom_density()] in that they
#' defaults to mapping `x` and `y` to `.panel_x` and `.panel_y` respectively,
#' they ignore the y scale of the panel and fills it out, and they work for both
#' continuous and discrete x scales.
#'
#' @inheritParams ggplot2::geom_histogram
#'
#' @seealso [facet_matrix] for creating matrix grids
#'
#' @export
#'
#' @examples
#' # A matrix plot with a mix of discrete and continuous variables
#' p <- ggplot(mpg) +
#'   geom_autopoint() +
#'   facet_matrix(vars(drv:fl), layer.diag = 2, grid.y.diag = FALSE)
#' p
#'
#' # Diagonal histograms
#' p + geom_autohistogram()
#'
#' # Diagonal density distributions
#' p + geom_autodensity()
#'
#' # You can use them like regular layers with groupings etc
#' p + geom_autodensity(aes(colour = drv, fill = drv),
#'                      alpha = 0.4)
geom_autohistogram <- function(mapping = NULL, data = NULL,
                               stat = "autobin", position = "floatstack",
                               ...,
                               bins = NULL,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  extra_mapping <- aes(x = .panel_x, y = .panel_y)
  if (is.null(mapping$x)) mapping$x <- extra_mapping$x
  if (is.null(mapping$y)) mapping$y <- extra_mapping$y
  class(mapping) <- 'uneval'

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAutorect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      na.rm = na.rm,
      ...
    )
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatAutobin <- ggproto('StatAutobin', StatBin,
  setup_params = function(data, params) {
    if (is.null(params$bins)) params$bins <- 30
    params$panel_range <- lapply(split(data$y, data$PANEL), function(y) {
      if (length(y) == 0) return()
      range(y, na.rm=TRUE)
    })
    params$panel_count <- lapply(split(data$y, data$PANEL), function(y)length(y[is.finite(y)]))

    params
  },
  compute_group = function(self, data, scales, binwidth = NULL, bins = NULL,
                           center = NULL, boundary = NULL,
                           closed = c("right", "left"), pad = FALSE,
                           breaks = NULL, panel_range = list(), panel_count = list(),
                           # The following arguments are not used, but must
                           # be listed so parameters are computed correctly
                           origin = NULL, right = NULL, drop = NULL,
                           width = NULL) {
    if (scales$x$is_discrete()) {
      binned <- rbind_dfs(lapply(split(data, data$x), function(d) {
        new_data_frame(list(
          count = nrow(d),
          x = d$x[1],
          xmin = d$x[1] - 0.5,
          xmax = d$x[1] + 0.5,
          width = 1
        ))
      }))
      binned$density <- binned$count / sum(binned$count)
      binned$ncount <- binned$count / max(binned$count)
      binned$ndensity <- binned$density / max(binned$density)
    } else {
      binned <- ggproto_parent(StatBin, self)$compute_group(
        data, scales, binwidth = binwidth, bins = bins, center = center,
        boundary = boundary, closed = closed, pad = pad, breaks = breaks,
        origin = origin, right = right, drop = drop, width = width
      )
    }

    panel_range <- panel_range[[data$PANEL[1]]]
    panel_count <- panel_count[[data$PANEL[1]]]
    binned$ymin <- panel_range[1]
    binned$ymax <- binned$ymin + binned$ncount * (panel_range[2] - panel_range[1]) * nrow(data) / panel_count
    binned$y <- (binned$ymin + binned$ymax) / 2
    binned
  },

  default_aes = aes(weight = 1),
  required_aes = c("x", "y")
)
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomAutorect <- ggproto('PositionAutorect', GeomRect,
  draw_panel = function(self, data, panel_params, coord, ...) {
    y_range <- coord$range(panel_params)$y
    y_span <- y_range[2] - y_range[1]
    panel_min <- min(data$ymin)
    panel_span <- max(data$ymax) - panel_min
    data$ymin <- ((data$ymin - panel_min) / panel_span) * y_span * 0.9 + y_range[1]
    data$ymax <- ((data$ymax - panel_min) / panel_span) * y_span * 0.9 + y_range[1]
    ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord, ...)
  },
  extra_params = c('na.rm', 'lineend', 'linejoin')
)
