#' Facet by different data columns
#'
#' The `facet_matrix()` facet allows you to put different data columns into
#' different rows and columns in a grid of panels. If the same data columns are
#' present in both the rows and the columns of the grid, and used together with
#' [ggplot2::geom_point()] it is also known as a scatterplot matrix, and if
#' other geoms are used it is sometimes referred to as a pairs plot.
#' `facet_matrix` is so flexible that these types are simply a subset of its
#' capabilities, as any combination of data columns can be plotted against each
#' other using any type of geom. Layers should use the `.panel_x` and `.panel_y`
#' placeholders to map aesthetics to, in order to access the row and column
#' data.
#'
#' @param rows,cols A specification of the data columns to put in the rows and
#' columns of the facet grid. They are specified using the [ggplot2::vars()]
#' function wherein you can use standard tidyselect syntax as known from e.g.
#' `dplyr::select()`. These data values will be made available to the different
#' layers through the `.panel_x` and `.panel_y` variables.
#' @inheritParams ggplot2::facet_grid
#' @param flip.rows Should the order of the rows be reversed so that, if the
#' rows and columns are equal, the diagonal goes from bottom-left to top-right
#' instead of top-left to bottom-right.
#' @param alternate.axes Should axes be drawn at alternating positions.
#' @param layer.lower,layer.diag,layer.upper Specification for where each layer
#' should appear. The default (`NULL`) will allow any layer that has not been
#' specified directly to appear at that position. Putting e.g. `layer.diag = 2`
#' will make the second layer appear on the diagonal as well as remove that
#' layer from any position that has `NULL`. Using `TRUE` will put all layers at
#' that position, and using `FALSE` will conversely remove all layers. These
#' settings will only have an effect if the grid is symmetric.
#' @param layer.continuous,layer.discrete,layer.mixed As above, but instead of
#' referencing panel positions it references the combination of position scales
#' in the panel. Continuous panels have both a continuous x and y axis, discrete
#' panels have both a discrete x and y axis, and mixed panels have one of each.
#' Unlike the position based specifications above these also have an effect in
#' non-symmetric grids.
#' @param grid.y.diag Should the y grid be removed from the diagonal? In certain
#' situations the diagonal are used to plot the distribution of the column data
#' and will thus not use the y-scale. Removing the y gridlines can indicate
#' this.
#'
#' @note Due to the special nature of this faceting it slightly breaks the
#' ggplot2 API, in that any positional scale settings are ignored. This is
#' because each row and column in the grid will potentially have very different
#' scale types and it is not currently possible to have multiple different scale
#' specifications in the same plot object.
#'
#' @seealso [geom_autopoint], [geom_autohistogram], [geom_autodensity], and
#' [position_auto] for geoms and positions that adapts to different positional
#' scale types
#'
#' @export
#'
#' @examples
#' # Standard use:
#' ggplot(mpg) +
#'   geom_point(aes(x = .panel_x, y = .panel_y)) +
#'   facet_matrix(vars(displ, cty, hwy))
#'
#' # Switch the diagonal, alternate the axes and style strips as axis labels
#' ggplot(mpg) +
#'   geom_point(aes(x = .panel_x, y = .panel_y)) +
#'   facet_matrix(vars(displ, cty, hwy), flip.rows = TRUE,
#'                alternate.axes = TRUE, switch = 'both') +
#'   theme(strip.background = element_blank(),
#'         strip.placement = 'outside',
#'         strip.text = element_text(size = 12))
#'
#' # Mix discrete and continuous columns. Use geom_autopoint for scale-based jitter
#' ggplot(mpg) +
#'   geom_autopoint() +
#'   facet_matrix(vars(drv:fl))
#'
#' # Have a special diagonal layer
#' ggplot(mpg) +
#'   geom_autopoint() +
#'   geom_autodensity() +
#'   facet_matrix(vars(drv:fl), layer.diag = 2)
#'
#' \donttest{
#' # Show continuous panels in upper triangle as contours and rest as binned
#' ggplot(mpg) +
#'   geom_autopoint() +
#'   geom_autodensity() +
#'   geom_density2d(aes(x = .panel_x, y = .panel_y)) +
#'   geom_bin2d(aes(x = .panel_x, y = .panel_y)) +
#'   facet_matrix(vars(drv:fl), layer.lower = 1, layer.diag = 2,
#'                layer.continuous = -4, layer.discrete = -3, layer.mixed = -3)
#' }
#'
#' # Make asymmetric grid
#' ggplot(mpg) +
#'   geom_boxplot(aes(x = .panel_x, y = .panel_y, group = .panel_x)) +
#'   facet_matrix(rows = vars(cty, hwy), cols = vars(drv, fl))
#'
facet_matrix <- function(rows, cols = rows, shrink = TRUE, switch = NULL,
                         flip.rows = FALSE, alternate.axes = FALSE, layer.lower = NULL,
                         layer.diag = NULL, layer.upper = NULL,
                         layer.continuous = NULL, layer.discrete = NULL,
                         layer.mixed = NULL, grid.y.diag = TRUE) {
  if (!is_quosures(rows)) rows <- quos(rows)
  if (!is_quosures(cols)) cols <- quos(cols)

  ggproto(NULL, FacetMatrix,
    shrink = shrink,
    params = list(rows = quos(row_data = row_data), cols = quos(col_data = col_data),
                  row_vars = rows, col_vars = cols, switch = switch,
                  free = list(x = TRUE, y = TRUE),
                  space_free = list(x = FALSE, y = FALSE), margins = FALSE,
                  as.table = !flip.rows, drop = TRUE, labeller = label_value,
                  alternate.axes = alternate.axes,
                  layer.lower = layer.lower, layer.diag = layer.diag,
                  layer.upper = layer.upper, layer.continuous = layer.continuous,
                  layer.discrete = layer.discrete, layer.mixed = layer.mixed,
                  grid.y.diag = grid.y.diag)
  )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom tidyselect eval_select
#' @export
FacetMatrix <- ggproto('FacetMatrix', FacetGrid,
  setup_data = function(data, params) {
    lapply(seq_along(data), function(i) {
      d <- data[[i]]
      d$.layer_index <- i - 1
      d
    })
  },
  setup_params = function(data, params) {
    rows <- lapply(data, function(d) {
      names(eval_select(quo(c(!!!params$row_vars)), d))
    })
    rows <- unique0(unlist(rows))
    cols <- lapply(data, function(d) {
      names(eval_select(quo(c(!!!params$col_vars)), d))
    })
    cols <- unique0(unlist(cols))
    if (length(rows) == 0 || length(cols) == 0) {
      cli::cli_abort('{.arg rows} and {.arg cols} must select valid data columns')
    }
    params$pairs <- all(rows == cols)
    if (!params$as.table) rows <- rev(rows)
    params$row_vars <- rows
    params$col_vars <- cols
    plot_env <- get('plot_env', caller_env(2)) # Horrible hack - don't judge
    params$row_scales <- create_pos_scales(rows, data, plot_env, 'y', params$alternate.axes)
    params$col_scales <- create_pos_scales(cols, data, plot_env, 'x', params$alternate.axes)

    check_layer_pos_params(params$pairs, params$lower, params$upper, params$diag)

    n_layers <- length(data) - 1
    params$layer_pos <- assign_layers(
      n_layers,
      lower = params$layer.lower,
      diagonal = params$layer.diag,
      upper = params$layer.upper
    )
    params$layer_type <- assign_layers(
      n_layers,
      continuous = params$layer.continuous,
      discrete = params$layer.discrete,
      mixed = params$layer.mixed
    )

    params
  },
  compute_layout = function(data, params) {
    layout <- expand.grid(col_data = params$col_vars, row_data = params$row_vars, stringsAsFactors = FALSE)
    layout$ROW <- match(layout$row_data, params$row_vars)
    layout$COL <- match(layout$col_data, params$col_vars)
    layout$PANEL <- factor(seq_len(nrow(layout)))
    layout$SCALE_X <- layout$COL
    layout$SCALE_Y <- layout$ROW
    if (params$pairs) {
      mat_ind <- matrix(seq_len(nrow(layout)), length(params$row_vars), length(params$col_vars), byrow = TRUE)
      if (!params$as.table) mat_ind <- mat_ind[rev(seq_along(params$row_vars)), ]
      layout$panel_pos <- 'lower'
      layout$panel_pos[diag(mat_ind)] <- 'diagonal'
      layout$panel_pos[mat_ind[upper.tri(mat_ind)]] <- 'upper'
    }
    layout
  },
  map_data = function(data, layout, params) {
    layer_pos <- params$layer_pos[[data$.layer_index[1]]]
    layer_type <- params$layer_type[[data$.layer_index[1]]]
    data <- lapply(seq_len(nrow(layout)), function(i) {
      row <- layout$row_data[i]
      col <- layout$col_data[i]
      col_discrete <- params$col_scales[[layout$SCALE_X[i]]]$is_discrete()
      row_discrete <- params$row_scales[[layout$SCALE_Y[i]]]$is_discrete()
      panel_type <- c('continuous', 'mixed', 'discrete')[col_discrete + row_discrete + 1]

      placeholder <- cbind(data[0, ], PANEL = layout$PANEL[0],
                           .panel_x = numeric(), .panel_y = numeric())
      if (!all(c(row, col) %in% names(data))) return(placeholder)
      if (params$pairs && !layout$panel_pos[i] %in% layer_pos) return(placeholder)
      if (!panel_type %in% layer_type) return(placeholder)

      data$PANEL <- layout$PANEL[i]
      data$.panel_x <- params$col_scales[[col]]$map(data[[col]])
      data$.panel_y <- params$row_scales[[row]]$map(data[[row]])
      data
    })
    vec_rbind(!!!data)
  },
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- list()
    if (!is.null(x_scale)) {
      scales$x <- params$col_scales
    }
    if (!is.null(y_scale)) {
      scales$y <- params$row_scales
    }
    scales
  },
  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    if (params$pairs && !params$grid.y.diag) {
      panels[layout$panel_pos == 'diagonal'] <- lapply(
        panels[layout$panel_pos == 'diagonal'],
        function(panel) {
          grill <- grep('^grill', names(panel$children))
          y_grid <- grep('^panel\\.grid\\.(major\\.y)|(minor\\.y)', names(panel$children[[grill]]$children))
          panel$children[[grill]]$children[y_grid] <- rep(list(zeroGrob()), length(y_grid))
          panel
        }
      )
    }
    ggproto_parent(FacetGrid, self)$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
  }
)

create_pos_scales <- function(vars, data, env, dim = 'x', alternate = FALSE) {
  positions <- if (dim == 'x') c('bottom', 'top') else c('left', 'right')
  scales <- lapply(seq_along(vars), function(i) {
    var <- vars[i]
    pos <- if (alternate) positions[((i - 1) %% 2) + 1] else positions[1]
    d <- lapply(data, .subset2, var)
    d <- d[lengths(d) != 0]
    type <- paste0('scale_', dim, '_', scale_type(d[[1]]))
    scales <- lapply(type, function(t) {
      if (exists(t, env, mode = 'function')) {
        scale <- get(t, env, mode = 'function')
      } else if (exists(t, asNamespace('ggplot2'), mode = 'function')) {
        scale <- get(t, asNamespace('ggplot2'), mode = 'function')
      } else if (exists(t, env, mode = 'function')) {
        scale <- get(t, env, mode = 'function')
      } else if (exists(t, asNamespace('ggforce'), mode = 'function')) {
        scale <- get(t, asNamespace('ggforce'), mode = 'function')
      } else {
        NULL
      }
    })
    scales <- scales[lengths(scales) != 0]
    if (length(scales) == 0) {
      cli::cli_abort('Unable to pick a scale for {.col {var}}')
    }
    scale <- scales[[1]](name = NULL, position = pos)
    lapply(d, scale$train)
    scale
  })
  names(scales) <- vars
  scales
}

assign_layers <- function(n_layers, ...) {
  specs <- list(...)
  layers <- seq_len(n_layers)
  specs <- lapply(specs, function(spec) {
    if (is.null(spec)) return(spec)
    if (is.logical(spec)) {
      if (spec) layers else integer()
    } else {
      layers[spec]
    }
  })

  specified_layers <- sort(unique0(unlist(specs)))
  specified_layers <- layers %in% specified_layers

  specs <- lapply(specs, function(spec) {
    if (is.null(spec)) {
      layers[!specified_layers]
    } else {
      spec
    }
  })
  split(
    unlist(lapply(names(specs), function(name) rep_along(specs[[name]], name))),
    factor(unlist(specs), levels = layers)
  )
}

check_layer_pos_params <- function(pairs = TRUE, lower = NULL, upper = NULL, diag = NULL) {
  if (pairs) return()

  if (!all(is.null(lower), is.null(upper), is.null(diag))) {
    cli::cli_warn('layer positions are ignored when the matrix is not symmetrical')
  }
}

utils::globalVariables(c('.panel_x', '.panel_y', 'col_data', 'row_data'))

