#' @include shape.R
NULL

#' Arcs and wedges as polygons
#'
#' This set of stats and geoms makes it possible to draw arcs and wedges as
#' known from pie and donut charts as well as more specialized plottypes such as
#' sunburst plots.
#'
#' @details An arc bar is the thick version of an arc; that is, a circle segment
#' drawn as a polygon in the same way as a rectangle is a thick version of a
#' line. A wedge is a special case of an arc where the inner radius is 0. As
#' opposed to applying coord_polar to a stacked bar chart, these layers are
#' drawn in cartesian space, which allows for transformations not possible with
#' the native ggplot2 approach. Most notable of these are the option to explode
#' arcs and wedgets away from their center point, thus detaching it from the
#' main pie/donut.
#'
#' @section Aesthetics:
#' geom_arc_bar understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **r0**
#' - **r**
#' - **start** - when using stat_arc_bar
#' - **end** - when using stat_arc_bar
#' - **amount** - when using stat_pie
#' - explode
#' - color
#' - fill
#' - linewidth
#' - linetype
#' - alpha
#'
#'
#' @section Computed variables:
#' \describe{
#'  \item{x, y}{x and y coordinates for the polygon}
#' }
#'
#' \describe{
#'  \item{x, y}{The start coordinates for the segment}
#' }
#'
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::stat_identity
#'
#' @param n The number of points used to draw a full circle. The number of
#' points on each arc will then be calculated as n / span-of-arc
#'
#' @param sep The separation between arcs in pie/donut charts
#'
#' @name geom_arc_bar
#' @rdname geom_arc_bar
#' @seealso [geom_arc()] for drawing arcs as lines
#'
#' @examples
#' # If you know the angle spans to plot it is easy
#' arcs <- data.frame(
#'   start = seq(0, 2 * pi, length.out = 11)[-11],
#'   end = seq(0, 2 * pi, length.out = 11)[-1],
#'   r = rep(1:2, 5)
#' )
#'
#' # Behold the arcs
#' ggplot(arcs) +
#'   geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = r - 1, r = r, start = start,
#'                    end = end, fill = r))
#'
#' # geom_arc_bar uses geom_shape to draw the arcs, so you have all the
#' # possibilities of that as well, e.g. rounding of corners
#' ggplot(arcs) +
#'   geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = r - 1, r = r, start = start,
#'                    end = end, fill = r), radius = unit(4, 'mm'))
#'
#' # If you got values for a pie chart, use stat_pie
#' states <- c(
#'   'eaten', "eaten but said you didn\'t", 'cat took it', 'for tonight',
#'   'will decompose slowly'
#' )
#' pie <- data.frame(
#'   state = factor(rep(states, 2), levels = states),
#'   type = rep(c('Pie', 'Donut'), each = 5),
#'   r0 = rep(c(0, 0.8), each = 5),
#'   focus = rep(c(0.2, 0, 0, 0, 0), 2),
#'   amount = c(4, 3, 1, 1.5, 6, 6, 1, 2, 3, 2)
#' )
#'
#' # Look at the cakes
#' ggplot() + geom_arc_bar(aes(
#'   x0 = 0, y0 = 0, r0 = r0, r = 1, amount = amount,
#'   fill = state, explode = focus
#' ),
#' data = pie, stat = 'pie'
#' ) +
#'   facet_wrap(~type, ncol = 1) +
#'   coord_fixed() +
#'   theme_no_axes() +
#'   scale_fill_brewer('', type = 'qual')
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatArcBar <- ggproto('StatArcBar', Stat,
  compute_panel = function(data, scales, n = 360) {
    arcPaths(data, n)
  },

  required_aes = c('x0', 'y0', 'r0', 'r', 'start', 'end')
)
#' @rdname geom_arc_bar
#' @export
stat_arc_bar <- function(mapping = NULL, data = NULL, geom = 'arc_bar',
                         position = 'identity', n = 360, na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatArcBar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, n = n, ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatPie <- ggproto('StatPie', Stat,
  compute_panel = function(data, scales, n = 360, sep = 0) {
    data <- dapply(data, c('x0', 'y0'), function(df) {
      angles <- cumsum(df$amount)
      seps <- cumsum(sep * seq_along(angles))
      if (max(seps) >= 2 * pi) {
        cli::cli_abort(c(
          'Total separation exceeds circle circumference',
          i = 'Try lowering {.arg sep}.'
        ))
      }
      angles <- angles / max(angles) * (2 * pi - max(seps))
      data_frame0(
        df,
        start = c(0, angles[-length(angles)]) + c(0, seps[-length(seps)]) + sep / 2,
        end = angles + seps - sep / 2
      )
    })
    arcPaths(as.data.frame(data), n)
  },

  required_aes = c('x0', 'y0', 'r0', 'r', 'amount'),
  default_aes = aes(explode = NULL)
)
#' @rdname geom_arc_bar
#' @export
stat_pie <- function(mapping = NULL, data = NULL, geom = 'arc_bar',
                     position = 'identity', n = 360, sep = 0, na.rm = FALSE,
                     show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatPie, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, n = n, sep = sep, ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomArcBar <- ggproto('GeomArcBar', GeomShape,
  default_aes = combine_aes(GeomShape$default_aes, aes(colour = 'black', fill = NA))
)
#' @rdname geom_arc_bar
#' @inheritParams geom_shape
#' @export
geom_arc_bar <- function(mapping = NULL, data = NULL, stat = 'arc_bar',
                         position = 'identity', n = 360, expand = 0, radius = 0,
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomArcBar,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm, n = n, expand = expand, radius = radius, ...)
  )
}

arcPaths <- function(data, n) {
  trans <- radial_trans(c(0, 1), c(0, 2 * pi), pad = 0)
  data <- data[data$start != data$end, ]
  data$nControl <- ceiling(n / (2 * pi) * abs(data$end - data$start))
  data$nControl[data$nControl < 3] <- 3
  extraData <- !names(data) %in% c('r0', 'r', 'start', 'end', 'group')
  data$group <- make_unique(data$group)
  paths <- lapply(seq_len(nrow(data)), function(i) {
    path <- data_frame0(
      a = seq(data$start[i], data$end[i], length.out = data$nControl[i]),
      r = data$r[i]
    )
    if ('r0' %in% names(data)) {
      if (data$r0[i] != 0) {
        path <- vec_rbind(
          path,
          data_frame0(a = rev(path$a), r = data$r0[i])
        )
      } else {
        path <- vec_rbind(
          path,
          data_frame0(a = data$start[i], r = 0)
        )
      }
    }
    path$group <- data$group[i]
    path$index <- seq(0, 1, length.out = nrow(path))
    path <- cbind(path, data[rep(i, nrow(path)), extraData, drop = FALSE])
  })
  paths <- vec_rbind(!!!paths)
  paths <- cbind(
    paths[, !names(paths) %in% c('r', 'a')],
    trans$transform(paths$r, paths$a)
  )
  paths$x <- paths$x + paths$x0
  paths$y <- paths$y + paths$y0
  if ('explode' %in% names(data)) {
    exploded <- data$explode != 0
    if (any(exploded)) {
      exploder <- trans$transform(
        data$explode[exploded],
        data$start[exploded] + (data$end[exploded] - data$start[exploded]) / 2
      )
      explodedPaths <- paths$group %in% which(exploded)
      exploderInd <- as.integer(factor(paths$group[explodedPaths]))
      paths$x[explodedPaths] <-
        paths$x[explodedPaths] + exploder$x[exploderInd]
      paths$y[explodedPaths] <-
        paths$y[explodedPaths] + exploder$y[exploderInd]
    }
  }
  paths[, !names(paths) %in% c('x0', 'y0', 'exploded')]
}
arcPaths2 <- function(data, n) {
  trans <- radial_trans(c(0, 1), c(0, 2 * pi), pad = 0)
  fullCirc <- n / (2 * pi)
  extraData <- setdiff(names(data), c('r', 'x0', 'y0', 'end', 'group', 'PANEL'))
  hasExtra <- length(extraData) != 0
  extraTemplate <- data[1, extraData, drop = FALSE]
  paths <- lapply(split(seq_len(nrow(data)), data$group), function(i) {
    if (length(i) != 2) {
      cli::cli_abort(c(
        'Arcs must be defined by two end points',
        i = 'Make sure each group consists of two rows'
      ))
    }
    if (data$r[i[1]] != data$r[i[2]] ||
      data$x0[i[1]] != data$x0[i[2]] ||
      data$y0[i[1]] != data$y0[i[2]]) {
      cli::cli_abort(
        'Both end points in each arc must be at same radius ({.arg r}) and with same center ({.arg {c("x0", "y0")}})'
      )
    }
    if (data$end[i[1]] == data$end[i[2]]) return()
    nControl <- ceiling(fullCirc * abs(diff(data$end[i])))
    if (nControl < 3) nControl <- 3
    path <- data_frame0(
      a = seq(data$end[i[1]], data$end[i[2]], length.out = nControl),
      r = data$r[i[1]],
      x0 = data$x0[i[1]],
      y0 = data$y0[i[1]],
      group = data$group[i[1]],
      index = seq(0, 1, length.out = nControl),
      .interp = c(FALSE, rep(TRUE, nControl - 2), FALSE),
      PANEL = data$PANEL[i[1]]
    )
    if (hasExtra) {
      path <- cbind(path, extraTemplate[rep(1, nControl), , drop = FALSE])
      path[1, extraData] <- data[i[1], extraData, drop = FALSE]
      path[nControl, extraData] <- data[i[2], extraData, drop = FALSE]
    }
    path
  })
  paths <- vec_rbind(!!!paths)
  paths <- cbind(
    paths[, !names(paths) %in% c('r', 'a')],
    trans$transform(paths$r, paths$a)
  )
  paths$x <- paths$x + paths$x0
  paths$y <- paths$y + paths$y0
  paths[, !names(paths) %in% c('x0', 'y0')]
}
