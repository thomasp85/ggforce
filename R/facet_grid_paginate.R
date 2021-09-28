#' Split facet_grid over multiple plots
#'
#' This extension to [ggplot2::facet_grid()] will allow you to split
#' a facetted plot over multiple pages. You define a number of rows and columns
#' per page as well as the page number to plot, and the function will
#' automatically only plot the correct panels. Usually this will be put in a
#' loop to render all pages one by one.
#'
#' @inheritParams ggplot2::facet_grid
#' @param ncol Number of columns per page
#' @param nrow Number of rows per page
#' @param page The page to draw
#' @param byrow Should the pages be created row-wise or column wise
#'
#' @note If either `ncol` or `nrow` is `NULL` this function will
#' fall back to the standard `facet_grid` functionality.
#'
#' @family ggforce facets
#' @seealso [n_pages()] to compute the total number of pages in a paginated
#' faceted plot
#'
#' @export
#'
#' @examples
#' # Draw a small section of the grid
#' ggplot(diamonds) +
#'   geom_point(aes(carat, price), alpha = 0.1) +
#'   facet_grid_paginate(color ~ cut:clarity, ncol = 3, nrow = 3, page = 4)
facet_grid_paginate <- function(facets, margins = FALSE, scales = 'fixed',
                                space = 'fixed', shrink = TRUE,
                                labeller = 'label_value', as.table = TRUE,
                                switch = NULL, drop = TRUE, ncol = NULL,
                                nrow = NULL, page = 1, byrow = TRUE) {
  facet <- facet_grid(facets,
    margins = margins, scales = scales,
    space = space, shrink = shrink, labeller = labeller,
    as.table = as.table, switch = switch, drop = drop
  )
  if (is.null(nrow) || is.null(ncol)) {
    facet
  } else {
    ggproto(NULL, FacetGridPaginate,
      shrink = shrink,
      params = c(
        facet$params,
        list(ncol = ncol, nrow = nrow, page = page, byrow = byrow)
      )
    )
  }
}

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @export
FacetGridPaginate <- ggproto('FacetGridPaginate', FacetGrid,
  compute_layout = function(data, params) {
    layout <- FacetGrid$compute_layout(data, params)
    row_bin <- ceiling(layout$ROW / params$nrow)
    col_bin <- ceiling(layout$COL / params$ncol)
    bin_layout <- matrix(seq_len(max(row_bin) * max(col_bin)),
      nrow = max(row_bin), byrow = params$byrow
    )
    layout$page <- bin_layout[(col_bin - 1) * nrow(bin_layout) + row_bin]
    layout
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data,
                         theme, params) {
    include <- which(layout$page == params$page)
    panels <- panels[include]
    ranges <- ranges[include]
    layout <- layout[include, , drop = FALSE]
    layout$ROW <- layout$ROW - min(layout$ROW) + 1
    layout$COL <- layout$COL - min(layout$COL) + 1
    layout$PANEL <- 1:dim(layout)[1]
    x_scale_ind <- unique(layout$SCALE_X)
    x_scales <- x_scales[x_scale_ind]
    layout$SCALE_X <- match(layout$SCALE_X, x_scale_ind)
    y_scale_ind <- unique(layout$SCALE_Y)
    y_scales <- y_scales[y_scale_ind]
    layout$SCALE_Y <- match(layout$SCALE_Y, y_scale_ind)
    table <- FacetGrid$draw_panels(panels, layout, x_scales, y_scales, ranges,
                                   coord, data, theme, params)
    if (max(layout$ROW) != params$nrow) {
      spacing <- theme$panel.spacing.y %||% theme$panel.spacing
      missing_rows <- params$nrow - max(layout$ROW)
      table <- gtable_add_rows(table, unit(missing_rows, 'null'))
      table <- gtable_add_rows(table, spacing * missing_rows)
    }
    if (max(layout$COL) != params$ncol) {
      spacing <- theme$panel.spacing.x %||% theme$panel.spacing
      missing_cols <- params$ncol - max(layout$COL)
      table <- gtable_add_cols(table, unit(missing_cols, 'null'))
      table <- gtable_add_cols(table, spacing * missing_cols)
    }
    table
  }
)
