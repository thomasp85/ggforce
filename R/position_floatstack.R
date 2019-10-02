# Only for use with autohistogram and autodensity

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
PositionFloatstack <- ggproto('PositionFloatstack', PositionStack,
  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    list(
      var = self$var %||% if (flipped_aes) 'xmax' else 'ymax',
      fill = self$fill,
      vjust = self$vjust,
      reverse = self$reverse,
      flipped_aes = flipped_aes
    )
  },
  compute_panel = function(self, data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    panel_min <- data$ymin[1]

    data$y <- data$y - panel_min
    data$ymin <- data$ymin - panel_min
    data$ymax <- data$ymax - panel_min

    data <- flip_data(data, params$flipped_aes)
    data <- ggproto_parent(PositionStack, self)$compute_panel(data, params, scales)
    data <- flip_data(data, params$flipped_aes)

    data$y <- data$y + panel_min
    data$ymin <- data$ymin + panel_min
    data$ymax <- data$ymax + panel_min

    flip_data(data, params$flipped_aes)
  }
)
