#' Split facet_grid over multiple plots
#'
#' This extension to \code{\link[ggplot2]{facet_grid}} will allow you to split
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
#' @note If either \code{ncol} or \code{nrow} is \code{NULL} this function will
#' fall back to the standard \code{facet_grid} functionality.
#'
#' @family ggforce facets
#'
#' @export
#'
#' @examples
#' # Draw a small section of the grid
#' ggplot(diamonds) +
#'   geom_point(aes(carat, price), alpha = 0.1) +
#'   facet_grid_paginate(color~cut:clarity, ncol = 3, nrow = 3, page = 4)
#'
facet_grid_paginate <- function(facets, margins = FALSE, scales = "fixed",
    space = "fixed", shrink = TRUE, labeller = "label_value", as.table = TRUE,
    switch = NULL, drop = TRUE, ncol = NULL, nrow = NULL, page = 1,
    byrow = TRUE) {
    facet <- facet_grid(facets, margins = margins, scales = scales,
                        space = space, shrink = shrink, labeller = labeller,
                        as.table = as.table, switch = switch, drop = drop)
    if (is.null(nrow) || is.null(ncol)) {
        facet
    } else {
        ggproto(NULL, FacetGridPaginate, shrink = shrink,
                params = c(
                    facet$params,
                    list(ncol = ncol, nrow = nrow, page = page, byrow = byrow)
                ))
    }
}

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto FacetWrap
#' @export
FacetGridPaginate <- ggproto("FacetGridPaginate", FacetGrid,
    compute_layout = function(data, params) {
        layout <- FacetGrid$compute_layout(data, params)
        row_bin <- ceiling(layout$ROW / params$nrow)
        col_bin <- ceiling(layout$COL / params$ncol)
        bin_layout <- matrix(seq_len(max(row_bin) * max(col_bin)),
                             nrow = max(row_bin), byrow = params$byrow)
        layout$page <- bin_layout[(col_bin - 1) * nrow(bin_layout) + row_bin]
        layout
    },
    draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
        include <- which(layout$page == params$page)
        panels <- panels[include]
        ranges <- ranges[include]
        layout <- layout[include, , drop = FALSE]
        layout$ROW <- layout$ROW - min(layout$ROW) + 1
        layout$COL <- layout$COL - min(layout$COL) + 1
        FacetGrid$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    }
)
