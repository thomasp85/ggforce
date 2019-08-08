# Only for use with autohistogram and autodensity

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
PositionFloatstack <- ggproto('PositionFloatstack', PositionStack,
  setup_params = function(self, data) {
    list(
      var = 'ymax',
      fill = self$fill,
      vjust = self$vjust,
      reverse = self$reverse
    )
  },
  setup_data = function(data, params) {
    data
  },
  compute_panel = function(self, data, params, scales) {
    panel_min <- data$ymin[1]

    data$y <- data$y - panel_min
    data$ymin <- data$ymin - panel_min
    data$ymax <- data$ymax - panel_min

    data <- ggproto_parent(PositionStack, self)$compute_panel(data, params, scales)

    data$y <- data$y + panel_min
    data$ymin <- data$ymin + panel_min
    data$ymax <- data$ymax + panel_min

    data
  }
)
