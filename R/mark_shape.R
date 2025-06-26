#' Annotate areas with polygonal shapes
#'
#' This geom lets you annotate sets of points via polygonal shapes. 
#' Unlike other `geom_mark_*` functions, `geom_mark_shape` should be explicitly
#' provided with the shape coordinates. As in `geom_shape`, the polygon can be
#' expanded/contracted and corners can be rounded, which is controlled by `expand` and
#' `radius` parameters.
#'
#' @inheritSection geom_mark_circle Annotation
#' @inheritSection geom_mark_circle Filtering
#' @section Aesthetics:
#' `geom_mark_shape` understand the following aesthetics (required aesthetics are
#' in bold):
#'
#' - **x**
#' - **y**
#' - x0 *(used to anchor the label)*
#' - y0 *(used to anchor the label)*
#' - filter
#' - label
#' - description
#' - color
#' - fill
#' - group
#' - size
#' - linetype
#' - alpha
#'
#' @inheritParams geom_mark_circle
#'
#'
#' @family mark geoms
#' @name geom_mark_shape
#' @rdname geom_mark_shape
#'
#' @examples
#' set.seed(42)
#' points <- data.frame(
#'     x=runif(100, min=0, max=3),
#'     y=runif(100, min=0, max=3))
#' shape <- data.frame(
#'     x = c(0, 3, 3, 2, 2, 1, 1, 0),
#'     y = c(0, 0, 3, 3, 1, 1, 3, 3)
#' )
#' 
#' ggplot(points, aes(x=x, y=y)) +
#'     geom_point() +
#'     geom_mark_shape(data=shape, color="red")
#'
NULL

#' @rdname geom_mark_shape
#' @export
geom_mark_shape <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', expand = 0,
                           radius = 0,
                           label.margin = margin(2, 2, 2, 2, 'mm'),
                           label.width = NULL, label.minwidth = unit(50, 'mm'),
                           label.hjust = 0, label.fontsize = 12,
                           label.family = '', label.lineheight = 1,
                           label.fontface = c('bold', 'plain'),
                           label.fill = 'white', label.colour = 'black',
                           label.buffer = unit(10, 'mm'), con.colour = 'black',
                           con.size = 0.5, con.type = 'elbow', con.linetype = 1,
                           con.border = 'one', con.cap = unit(3, 'mm'),
                           con.arrow = NULL, ..., na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMarkHull,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      expand = expand,
      radius = radius,
      concavity = NA,
      label.margin = label.margin,
      label.width = label.width,
      label.minwidth = label.minwidth,
      label.fontsize = label.fontsize,
      label.family = label.family,
      label.lineheight = label.lineheight,
      label.fontface = label.fontface,
      label.hjust = label.hjust,
      label.fill = label.fill,
      label.colour = label.colour,
      label.buffer = label.buffer,
      con.colour = con.colour,
      con.size = con.size,
      con.type = con.type,
      con.linetype = con.linetype,
      con.border = con.border,
      con.cap = con.cap,
      con.arrow = con.arrow,
      ...
    )
  )
}
