#' Split facet_wrap over multiple plots
#'
#' This extension to \code{\link[ggplot2]{facet_wrap}} will allow you to split
#' a facetted plot over multiple pages. You define a number of rows and columns
#' per page as well as the page number to plot, and the function will
#' automatically only plot the correct panels. Usually this will be put in a
#' loop to render all pages one by one.
#'
#' @inheritParams ggplot2::facet_wrap
#' @param page The page to draw
#'
#' @note If either \code{ncol} or \code{nrow} is \code{NULL} this function will
#' fall back to the standard \code{facet_wrap} functionality.
#'
#' @export
#'
#' @examples
#' # Calculate the number of pages with 9 panels per page
#' n_pages <- ceiling(
#'   length(levels(diamonds$cut)) * length(levels(diamonds$clarity)) / 9
#' )
#'
#' # Draw each page
#' for (i in seq_len(n_pages)) {
#'   ggplot(diamonds) +
#'     geom_point(aes(carat, price), alpha = 0.1) +
#'     facet_wrap_paginate(~cut:clarity, ncol = 3, nrow = 3, page = i)
#' }
#'
facet_wrap_paginate <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                       shrink = TRUE, labeller = "label_value", as.table = TRUE,
                       switch = NULL, drop = TRUE, dir = "h", strip.position = 'top', page = 1) {
    facet <- facet_wrap(facets, nrow = nrow, ncol = ncol, scales = scales,
               shrink = shrink, labeller = labeller, as.table = as.table,
               switch = switch, drop = drop, dir = dir,
               strip.position = strip.position)
    if (is.null(nrow) || is.null(ncol)) {
        facet
    } else {
        ggproto(NULL, FacetWrapPaginate, shrink = shrink,
                params = c(facet$params, list(page = page)))
    }
}

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto FacetWrap
#' @export
FacetWrapPaginate <- ggproto("FacetWrapPaginate", FacetWrap,
    setup_params = function(data, params) {
        modifyList(
            params,
            list(
                max_rows = params$nrow,
                nrow = NULL
            )
        )
    },
    compute_layout = function(data, params) {
        layout <- FacetWrap$compute_layout(data, params)
        layout$page <- ceiling(layout$ROW / params$max_rows)
        layout
    },
    draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
        include <- which(layout$page == params$page)
        panels <- panels[include]
        ranges <- ranges[include]
        layout <- layout[include, , drop = FALSE]
        layout$ROW <- layout$ROW - min(layout$ROW) + 1
        FacetWrap$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    }
)
