#' One-dimensional facets
#'
#' These facets are one-dimensional versions of [ggplot2::facet_wrap()],
#' arranging the panels in either a single row or a single column. This
#' restriction makes it possible to support a `space` argument as seen in
#' [ggplot2::facet_grid()] which, if set to `"free"` will allow the panels to be
#' sized based on the relative range of their scales. Another way of thinking
#' about them are one-dimensional versions of [ggplot2::facet_grid()] (ie.
#' `. ~ {var}` or `{var} ~ .`), but with the ability to position the strip at
#' either side of the panel. However you look at it it is the best of both world
#' if you just need one dimension.
#'
#' @inheritParams ggplot2::facet_wrap
#' @param space Should the size of the panels be fixed or relative to the range
#' of the respective position scales
#'
#' @export
#'
#' @examples
#' # Standard use
#' ggplot(mtcars) +
#'   geom_point(aes(disp, mpg)) +
#'   facet_col(~gear)
#' # It retains the ability to have unique scales for each panel
#' ggplot(mtcars) +
#'   geom_point(aes(disp, mpg)) +
#'   facet_col(~gear, scales = 'free')
#'
#' # But can have free sizing along the stacking dimension
#' ggplot(mtcars) +
#'   geom_point(aes(disp, mpg)) +
#'   facet_col(~gear, scales = 'free', space = 'free')
#'
#' # And you can position the strip where-ever you like
#' ggplot(mtcars) +
#'   geom_point(aes(disp, mpg)) +
#'   facet_col(~gear, scales = 'free', space = 'free', strip.position = 'bottom')
#'
facet_row <- function(facets, scales = "fixed", space = "fixed",
                      shrink = TRUE, labeller = "label_value",
                      drop = TRUE, strip.position = 'top') {
  space <- match.arg(space, c('free', 'fixed'))
  facet <- facet_wrap(facets, nrow = 1, scales = scales, shrink = shrink, labeller = labeller, drop = drop, strip.position = strip.position)
  params <- facet$params

  if ("space" %in% fn_fmls_names(facet_wrap)) {
    params$space_free <- list(x = space == 'free', y = FALSE)
  } else {
    params$space_free <- space == 'free'
  }

  ggproto(NULL, FacetRow, shrink = shrink, params = params)
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
FacetRow <- ggproto('FacetRow', FacetWrap,
  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    combined <- ggproto_parent(FacetWrap, self)$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    if (isTRUE(params$space_free)) {
      widths <- vapply(layout$PANEL, function(i) diff(ranges[[i]]$x.range), numeric(1))
      panel_widths <- unit(widths, "null")
      combined$widths[panel_cols(combined)$l] <- panel_widths
    }
    combined
  }
)

#' @rdname facet_row
#' @export
facet_col <- function(facets, scales = "fixed", space = "fixed",
                      shrink = TRUE, labeller = "label_value",
                      drop = TRUE, strip.position = 'top') {
  space <- match.arg(space, c('free', 'fixed'))
  facet <- facet_wrap(facets, ncol = 1, scales = scales, shrink = shrink, labeller = labeller, drop = drop, strip.position = strip.position)
  params <- facet$params

  if ("space" %in% fn_fmls_names(facet_wrap)) {
    params$space_free <- list(x = FALSE, y = space == 'free')
  } else {
    params$space_free <- space == 'free'
  }
  ggproto(NULL, FacetCol, shrink = shrink, params = params)
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
FacetCol <- ggproto('FacetCol', FacetWrap,
  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    combined <- ggproto_parent(FacetWrap, self)$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    if (isTRUE(params$space_free)) {
      heights <- vapply(layout$PANEL, function(i) diff(ranges[[i]]$y.range), numeric(1))
      panel_heights <- unit(heights, "null")
      combined$heights[panel_rows(combined)$t] <- panel_heights
    }
    combined
  }
)
