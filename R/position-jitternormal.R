#' Jitter points with normally distributed random noise
#'
#' [ggplot2::geom_jitter()] adds random noise to points using a uniform
#' distribution. When many points are plotted, they appear in a rectangle. This
#' position jitters points using a normal distribution instead, resulting in
#' more circular clusters.
#'
#' @family position adjustments
#' @param sd_x,sd_y Standard deviation to add along the x and y axes. The
#'   function uses [stats::rnorm()] with `mean = 0` behind the scenes.
#'
#'   If omitted, defaults to 0.15. As with [ggplot2::geom_jitter()], categorical
#'   data is aligned on the integers, so a standard deviation of more than 0.2
#'   will spread the data so it's not possible to see the distinction between
#'   the categories.
#' @inheritParams ggplot2::position_jitter
#' @export
#' @examples
#' # Example data
#' df <- data.frame(
#'   x = sample(1:3, 1500, TRUE),
#'   y = sample(1:3, 1500, TRUE)
#' )
#'
#' # position_jitter results in rectangular clusters
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_point(position = position_jitter())
#'
#' # geom_jitternormal results in more circular clusters
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_point(position = position_jitternormal())
#'
#' # You can adjust the standard deviations along both axes
#' # Tighter circles
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_point(position = position_jitternormal(sd_x = 0.08, sd_y = 0.08))
#'
#' # Oblong shapes
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_point(position = position_jitternormal(sd_x = 0.2, sd_y = 0.08))
#'
#' # Only add random noise to one dimension
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_point(
#'     position = position_jitternormal(sd_x = 0.15, sd_y = 0),
#'     alpha = 0.1
#'   )
position_jitternormal <- function(sd_x = NULL, sd_y = NULL, seed = NA) {
  ggproto(NULL, PositionJitterNormal,
    sd_x = sd_x,
    sd_y = sd_y,
    seed = seed
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
PositionJitterNormal <- ggproto('PositionJitterNormal',
  Position,
  seed = NA,
  required_aes = c('x', 'y'),

  setup_params = function(self, data) {
    if (!is.null(self$seed) && is.na(self$seed)) {
      seed <- sample.int(.Machine$integer.max, 1L)
    } else {
      seed <- self$seed
    }
    list(
      sd_x = self$sd_x %||% 0.15,
      sd_y = self$sd_y %||% 0.15,
      seed = seed
    )
  },

  compute_layer = function(data, params, panel) {
    trans_x <- if (params$sd_x > 0) {
      function(x) x + rnorm(length(x), sd = params$sd_x)
    }
    trans_y <- if (params$sd_y > 0) {
      function(x) x + rnorm(length(x), sd = params$sd_y)
    }

    # Make sure x and y jitter is only calculated once for all position aesthetics
    # Takes aesthetic names from ggplot_global
    x_aes <- intersect(c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final",
                         "xlower", "xmiddle", "xupper", "x0"), names(data))
    x <- if (length(x_aes) == 0) 0 else data[[x_aes[1]]]
    y_aes <- intersect(c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final",
                         "lower", "middle", "upper", "y0"), names(data))
    y <- if (length(y_aes) == 0) 0 else data[[y_aes[1]]]
    dummy_data <- data_frame0(x = x, y = y, .size = nrow(data))
    fixed_jitter <- with_seed_null(params$seed, transform_position(dummy_data, trans_x, trans_y))
    x_jit <- fixed_jitter$x - x
    y_jit <- fixed_jitter$y - y
    # Avoid nan values, if x or y has Inf values
    x_jit[is.infinite(x)] <- 0
    y_jit[is.infinite(y)] <- 0

    # Apply jitter
    transform_position(data, function(x) x + x_jit, function(x) x + y_jit)
  }
)
