#' Create Parallel Sets diagrams
#'
#' A parallel sets diagram is a type of visualisation showing the interaction
#' between multiple categorical variables. If the variables has an intrinsic
#' order the representation can be thought of as a Sankey Diagram. If each
#' variable is a point in time it will resemble an alluvial diagram.
#'
#' In a parallel sets visualization each categorical variable will be assigned
#' a position on the x-axis. The size of the intersection of categories from
#' neighboring variables are then shown as thick diagonals, scaled by the sum of
#' elements shared between the two categories. The natural data representation
#' for such as plot is to have each categorical variable in a separate column
#' and then have a column giving the amount/magnitude of the combination of
#' levels in the row. This representation is unfortunately not fitting for the
#' `ggplot2` API which needs every position encoding in the same column. To make
#' it easier to work with `ggforce` provides a helper [gather_set_data()], which
#' takes care of the transformation.
#'
#' @section Aesthetics:
#' geom_parallel_sets understand the following aesthetics
#' (required aesthetics are in bold):
#'
#' - **x|y**
#' - **id**
#' - **split**
#' - **value**
#' - color
#' - fill
#' - size
#' - linetype
#' - alpha
#' - lineend
#'
#' @inheritParams geom_diagonal_wide
#' @param sep The proportional separation between categories within a variable
#' @param axis.width The width of the area around each variable axis
#' @param angle The angle of the axis label text
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#' Useful for offsetting text from the category segments.
#'
#' @inheritSection ggplot2::geom_line Orientation
#'
#' @name geom_parallel_sets
#' @rdname geom_parallel_sets
#'
#' @author Thomas Lin Pedersen
#'
#' @examples
#' data <- reshape2::melt(Titanic)
#' data <- gather_set_data(data, 1:4)
#'
#' ggplot(data, aes(x, id = id, split = y, value = value)) +
#'   geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
#'   geom_parallel_sets_axes(axis.width = 0.1) +
#'   geom_parallel_sets_labels(colour = 'white')
#'
#' # Use nudge_x to offset and hjust = 0 to left-justify label
#' ggplot(data, aes(x, id = id, split = y, value = value)) +
#'   geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
#'   geom_parallel_sets_axes(axis.width = 0.1) +
#'   geom_parallel_sets_labels(colour = 'red', angle = 0, nudge_x = 0.1, hjust = 0)
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatParallelSets <- ggproto('StatParallelSets', Stat,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)
    params
  },
  setup_data = function(data, params) {
    value_check <- lapply(split(data$value, data$id), unique0)
    if (any(lengths(value_check) != 1)) {
      cli::cli_abort('{.field value} must be kept constant across {.field id}')
    }
    data$split <- as.factor(data$split)
    data$flipped_aes <- params$flipped_aes
    data
  },
  compute_panel = function(data, scales, sep = 0.05, strength = 0.5, n = 100,
                           axis.width = 0, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    data <- remove_group(data)
    data <- complete_data(data)
    cols <- c('group', 'colour', 'color', 'fill', 'size', 'linewidth', 'alpha', 'linetype')
    data_groups <- vec_rbind(
      !!!lapply(split(data[, names(data) %in% cols, drop = FALSE], data$group),
        function(d) {
          data_frame0(!!!lapply(d, function(x) na.omit(x)[1]))
        }
      )
    )
    # Calculate axis sizes
    data_axes <- sankey_axis_data(data, sep)

    # Calculate diagonals
    diagonals <- sankey_diag_data(data, data_axes, data_groups, axis.width)

    diagonals <- flip_data(diagonals, flipped_aes)

    StatDiagonalWide$compute_panel(diagonals, scales, strength, n, flipped_aes)
  },
  required_aes = c('x|y', 'id', 'split', 'value'),
  extra_params = c('na.rm', 'n', 'sep', 'strength', 'axis.width', 'orientation')
)
#' @rdname geom_parallel_sets
#' @export
stat_parallel_sets <- function(mapping = NULL, data = NULL, geom = 'shape',
                               position = 'identity', n = 100, strength = 0.5,
                               sep = 0.05, axis.width = 0, na.rm = FALSE,
                               orientation = NA, show.legend = NA,
                               inherit.aes = TRUE, ...) {
  layer(
    stat = StatParallelSets, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, orientation = orientation, n = n, strength = strength,
      sep = sep, axis.width = axis.width, ...
    )
  )
}
#' @rdname geom_parallel_sets
#' @export
geom_parallel_sets <- function(mapping = NULL, data = NULL,
                               stat = 'parallel_sets', position = 'identity',
                               n = 100, na.rm = FALSE, orientation = NA,
                               sep = 0.05, strength = 0.5, axis.width = 0,
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomShape,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, orientation = orientation, n = n, strength = strength,
      sep = sep, axis.width = axis.width, ...
    )
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
StatParallelSetsAxes <- ggproto('StatParallelSetsAxes', Stat,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)
    params
  },
  setup_data = function(data, params) {
    value_check <- lapply(split(data$value, data$id), unique0)
    if (any(lengths(value_check) != 1)) {
      cli::cli_abort('{.field value} must be kept constant across {.field id}')
    }
    data$split <- as.factor(data$split)
    data$flipped_aes <- params$flipped_aes
    data
  },
  compute_panel = function(data, scales, sep = 0.05, axis.width = 0, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    split_levels <- levels(data$split)
    data <- remove_group(data)
    data <- complete_data(data, FALSE)
    # Calculate axis sizes
    data_axes <- sankey_axis_data(data, sep)
    data_axes <- data_axes[data_axes$split != '.ggforce_missing', ]
    cols <- c('x', 'split', 'colour', 'color', 'fill', 'size', 'linewidth',
              'alpha', 'linetype')
    aes <- data[, names(data) %in% cols]
    aes <- unique0(aes)
    if (nrow(aes) != nrow(data_axes)) {
      cli::cli_abort('Axis aesthetics must be constant in each split')
    }
    data_axes$split <- factor(as.character(data_axes$split),
                              levels = split_levels)
    aes$split <- factor(as.character(aes$split), levels = split_levels)
    data <- merge(data_axes, aes, by = c('x', 'split'), all.x = TRUE,
                  sort = FALSE)
    names(data)[names(data) == 'split'] <- 'label'
    data$y <- data$ymin + data$value / 2
    data$xmin <- data$x - axis.width / 2
    data$xmax <- data$x + axis.width / 2
    flip_data(data, flipped_aes)
  },
  required_aes = c('x|y', 'id', 'split', 'value'),
  extra_params = c('na.rm', 'sep', 'orientation')
)
#' @rdname geom_parallel_sets
#' @export
stat_parallel_sets_axes <- function(mapping = NULL, data = NULL,
                                    geom = 'parallel_sets_axes',
                                    position = 'identity', sep = 0.05,
                                    axis.width = 0, na.rm = FALSE,
                                    orientation = NA, show.legend = NA,
                                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatParallelSetsAxes, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, orientation = orientation, sep = sep,
                  axis.width = axis.width, ...)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomParallelSetsAxes <- ggproto('GeomParallelSetsAxes', GeomShape,
  setup_data = function(data, params) {
    flipped_aes <- has_flipped_aes(data, params)
    data <- flip_data(data, flipped_aes)
    data$group <- seq_len(nrow(data))
    lb <- data
    lb$x <- lb$xmin
    lb$y <- lb$ymin
    rb <- data
    rb$x <- rb$xmax
    rb$y <- rb$ymin
    lt <- data
    lt$x <- lt$xmin
    lt$y <- lt$ymax
    rt <- data
    rt$x <- rt$xmax
    rt$y <- rt$ymax
    data <- vec_rbind(lb, rb, rt, lt)
    flip_data(data[order(data$group), ], flipped_aes)
  },
  required_aes = c('xmin', 'ymin', 'xmax', 'ymax')
)
#' @rdname geom_parallel_sets
#' @export
geom_parallel_sets_axes <- function(mapping = NULL, data = NULL,
                                    stat = 'parallel_sets_axes',
                                    position = 'identity', na.rm = FALSE,
                                    orientation = NA, show.legend = NA,
                                    inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomParallelSetsAxes,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, orientation = orientation, ...)
  )
}
#' @rdname geom_parallel_sets
#' @export
geom_parallel_sets_labels <- function(mapping = NULL, data = NULL,
                                      stat = 'parallel_sets_axes', angle = -90,
				                              nudge_x = 0, nudge_y = 0,
                                      position = 'identity', na.rm = FALSE,
				                              orientation = NA, show.legend = NA,
				                              inherit.aes = TRUE, ...) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied",
        "i" = "Only use one approach to alter the position"
      ))
    }
    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomText,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, orientation = orientation, angle = angle, ...)
  )
}
#' Tidy data for use with geom_parallel_sets
#'
#' This helper function makes it easy to change tidy data into a tidy(er) format
#' that can be used by geom_parallel_sets.
#'
#' @param data A tidy dataframe with some categorical columns
#' @param x The columns to use for axes in the parallel sets diagram
#' @param id_name The name of the column that will contain the original index of
#' the row.
#'
#' @return A data.frame
#'
#' @export
#'
#' @examples
#' data <- reshape2::melt(Titanic)
#' head(gather_set_data(data, 1:4))
#' head(gather_set_data(data, c("Class","Sex","Age","Survived")))
gather_set_data <- function(data, x, id_name = 'id') {
  columns <- tidyselect::eval_select(enquo(x), data)
  data[[id_name]] <- seq_len(nrow(data))
  vec_rbind(!!!lapply(names(columns), function(n) {
    data$x <- n
    data$y <- data[[n]]
    data
  }))
}
#' @importFrom stats na.omit
complete_data <- function(data, check_id = TRUE) {
  levels(data$split) <- c(levels(data$split), '.ggforce_missing')
  all_obs <- unique0(data[, c('id', 'value')])
  data <- vec_rbind(!!!lapply(split(data, data$x), function(d) {
    if (anyDuplicated(d$id) != 0) {
      cli::cli_abort('{.field id} must be unique within axes')
    }
    x <- d$x[1]
    if (length(d$id) != nrow(all_obs)) {
      n_miss <- nrow(all_obs) - length(d$id)
      fill <- d[seq_len(n_miss), ][NA, ]
      fill$x <- x
      fill[, c('id', 'value')] <- all_obs[!d$id %in% all_obs$id, ]
      fill$split <- '.ggforce_missing'
      d <- vec_rbind(d, fill)
    }
    d
  }))

  if (check_id) {
    # Ensure id grouping
    id_groups <- lapply(split(data$group, data$id),
                        function(x) unique0(na.omit(x)))
    if (any(lengths(id_groups) != 1)) {
      cli::cli_abort('{.field id} must keep grouping across data')
    }
    id_match <- match(as.character(data$id), names(id_groups))
    data$group <- unlist(id_groups)[id_match]
  }

  data[order(data$x, data$id), ]
}

sankey_axis_data <- function(data, sep) {
  vec_rbind(!!!lapply(split(data, data$x), function(d) {
    splits <- split(d$value, as.character(d$split))
    splits <- splits[rev(order(match(names(splits), levels(d$split))))]
    d <- data_frame0(
      split = factor(names(splits)),
      value = sapply(splits, sum),
      x = d$x[1]
    )
    sep <- sum(d$value) * sep
    d$ymax <- (seq_len(nrow(d)) - 1) * sep + cumsum(d$value)
    d$ymin <- d$ymax - d$value
    d
  }))
}

sankey_diag_data <- function(data, axes_data, groups, axis.width) {
  axes <- sort(unique0(data$x))
  diagonals <- lapply(seq_len(length(axes) - 1), function(i) {
    from <- data[data$x == axes[i], , drop = FALSE]
    to <- data[data$x == axes[i + 1], , drop = FALSE]
    diagonals <- split(
      seq_len(nrow(from)),
      list(from$group, from$split, to$split)
    )
    diagonals <- diagonals[lengths(diagonals) != 0]
    diag_rep <- sapply(diagonals, `[`, 1)
    diag_from <- data_frame0(
      group = from$group[diag_rep],
      split = from$split[diag_rep],
      value = sapply(diagonals, function(ii) sum(from$value[ii])),
      x = from$x[1] + axis.width / 2
    )
    diag_to <- diag_from
    diag_to$split <- to$split[diag_rep]
    diag_to$x <- to$x[1] - axis.width / 2

    diag_from <- add_y_pos(diag_from, axes_data[axes_data$x == axes[i], ])
    diag_to <- add_y_pos(diag_to, axes_data[axes_data$x == axes[i + 1], ])
    diagonals <- vec_rbind(diag_from, diag_to)
    main_groups <- diagonals$group
    diagonals$group <- rep(seq_len(nrow(diag_from) / 2), 4)
    if (length(setdiff(names(groups), 'group')) > 0) {
      diagonals <- cbind(
        diagonals,
        groups[match(main_groups, groups$group), names(groups) != 'group', drop = FALSE]
      )
    }
    diagonals
  })
  n_groups <- sapply(diagonals, nrow) / 4
  group_offset <- c(0, cumsum(n_groups)[-length(n_groups)])
  vec_rbind(!!!Map(function(d, i) {
    d$group <- d$group + i
    d
  }, d = diagonals, i = group_offset))
}

add_y_pos <- function(data, axes_data) {
  splits <- split(seq_len(nrow(data)), as.character(data$split))
  ymin <- lapply(splits, function(i) {
    split <- as.character(data$split[i[1]])
    sizes <- data$value[i]
    ymin <- axes_data$ymax[axes_data$split == split] -
      cumsum(sizes[order(data$group[i])])
    ymin[order(data$group[i])] <- ymin
    ymin
  })
  data$y[unlist(splits)] <- unlist(ymin)
  data_tmp <- data
  data_tmp$y <- data$y + data$value
  vec_rbind(data_tmp, data)
}

remove_group <- function(data) {
  split_groups <- lapply(split(data$group, data$split), unique0)
  if (length(Reduce(intersect, split_groups)) == 0) {
    disc <- vapply(data, is.discrete, logical(1))
    disc[names(disc) %in% c('split', 'label', 'PANEL')] <- FALSE
    if (any(disc)) {
      data$group <- id(data[disc], drop = TRUE)
    } else {
      data$group <- -1
    }
  }
  data
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}
