#' Jitter based on scale types
#'
#' This position adjustment is able to select a meaningful jitter of the data
#' based on the combination of positional scale types. IT behaves differently
#' depending on if none, one, or both the x and y scales are discrete. If both
#' are discrete it will jitter the datapoints evenly inside a disc, if one of
#' them is discrete it will jitter the discrete dimension to follow the density
#' along the other dimension (like a sina plot). If neither are discrete it will
#' not do any jittering.
#'
#' @param jitter.width The maximal width of the jitter
#' @param bw The smoothing bandwidth to use in the case of sina jittering. See
#' the `bw` argument in [stats::density]
#' @param scale Should the width of jittering be scaled based on the number of
#' points in the group
#' @param seed A seed to supply to make the jittering reproducible across layers
#'
#' @seealso [geom_autopoint] for a point geom that uses auto-position by default
#'
#' @export
#'
#' @examples
#' # Continuous vs continuous: No jitter
#' ggplot(mpg) + geom_point(aes(cty, hwy), position = 'auto')
#'
#' # Continuous vs discrete: sina jitter
#' ggplot(mpg) + geom_point(aes(cty, drv), position = 'auto')
#'
#' # Discrete vs discrete: disc-jitter
#' ggplot(mpg) + geom_point(aes(fl, drv), position = 'auto')
#'
#' # Don't scale the jitter based on group size
#' ggplot(mpg) + geom_point(aes(cty, drv), position = position_auto(scale = FALSE))
#' ggplot(mpg) + geom_point(aes(fl, drv), position = position_auto(scale = FALSE))
#'
position_auto <- function(jitter.width = 0.75, bw = 'nrd0', scale = TRUE, seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionAuto,
          jitter.width = jitter.width,
          seed = seed, bw = bw, scale = scale
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
PositionAuto <- ggproto('PositionAuto', Position,
  jitter.width = 0.75,
  seed = NULL,
  bw = 'nrd0',
  scale = TRUE,
  setup_params = function(self, data) {
    list(jitter.width = self$jitter.width, bw = self$bw, seed = self$seed, scale = self$scale)
  },
  compute_panel = function(data, params, scales) {
    discrete_x <- scales$x$is_discrete()
    discrete_y <- scales$y$is_discrete()
    if (!discrete_x && !discrete_y) {
      return(data)
    }
    if (discrete_x && discrete_y) {
      comb <- table(data$x, data$y)
      max_n <- max(comb)
      if (params$scale) {
        weight <- sqrt(comb[cbind(as.character(data$x), as.character(data$y))] / max_n) * (params$jitter.width / 2)
      } else {
        weight <- params$jitter.width / 2
      }
      if (is.null(params$seed)) {
        adj <- sample_disc(length(data$x), weight)
      } else {
        adj <- withr::with_seed(params$seed, sample_disc(length(data$x), weight))
      }
      data$x <- data$x + adj$x
      data$y <- data$y + adj$y
      data
    } else {
      trans_x <- trans_y <- identity
      if (discrete_x) {
        trans_x <- function(x) x + sina_trans(x, data$y, params$jitter.width / 2, params$bw, params$scale)
      } else {
        trans_y <- function(x) x + sina_trans(x, data$x, params$jitter.width / 2, params$bw, params$scale)
      }
      if (is.null(params$seed)) {
        transform_position(data, trans_x, trans_y)
      } else {
        withr::with_seed(params$seed, transform_position(data, trans_x, trans_y))
      }
    }
  }
)

sina_trans <- function(x, val, max_width, bw = 'nrd0', scale = TRUE) {
  max_size <- max(table(x))
  by_ind <- split(seq_along(x), x)
  x_new <- unlist(lapply(by_ind, function(i) {
    val_x <- val[i]
    if (length(unique(val_x)) < 2) {
      return(stats::runif(length(val_x), min = -max_width, max = max_width))
    }
    if (length(val_x) < 3) {
      return(0)
    }
    range <- range(val_x, na.rm = TRUE)
    bw <- calc_bw(val_x, bw)
    dens <- stats::density(val_x, bw = bw, from = range[1], to = range[2])
    densf <- stats::approxfun(dens$x, dens$y, rule = 2)
    x_mod <- densf(val_x)
    x_mod <- x_mod / max(x_mod)
    if (scale) x_mod <- x_mod * length(val_x) / max_size
    stats::runif(length(val_x), min = -1, max = 1) * max_width * x_mod
  }))
  x_new[match(seq_along(x), unlist(by_ind))]
}
sample_disc <- function(n, r_disc = 1) {
  r = sqrt(stats::runif(n, 0, 1))
  theta = stats::runif(n, 0, 2*pi)
  x <- r * cos(theta) * r_disc
  y <- r * sin(theta) * r_disc
  list(x = x, y = y)
}
