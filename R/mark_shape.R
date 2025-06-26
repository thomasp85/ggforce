#' Annotate areas with hulls
#'
#' This geom lets you annotate sets of points via hulls. While convex hulls are
#' most common due to their clear definition, they can lead to large areas
#' covered that does not contain points. Due to this `geom_mark_hull` uses
#' concaveman which lets you adjust concavity of the resulting hull. The hull is
#' calculated at draw time, and can thus change as you resize the plot. In order
#' to clearly contain all points, and for aesthetic purpose the resulting hull
#' is expanded 5mm and rounded on the corners. This can be adjusted with the
#' `expand` and `radius` parameters.
#'
#' @inheritSection geom_mark_circle Annotation
#' @inheritSection geom_mark_circle Filtering
#' @section Aesthetics:
#' `geom_mark_hull` understand the following aesthetics (required aesthetics are
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
#' @param concavity A measure of the concavity of the hull. `1` is very concave
#' while it approaches convex as it grows. Defaults to `2`.
#'
#' @family mark geoms
#' @name geom_mark_shape
#' @rdname geom_mark_shape
#'
#' @examples
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, filter = Species != 'versicolor')) +
#'   geom_point()
#'
#' # Adjusting the concavity lets you change the shape of the hull
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, filter = Species != 'versicolor'),
#'     concavity = 1
#'   ) +
#'   geom_point()
#'
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, filter = Species != 'versicolor'),
#'     concavity = 10
#'   ) +
#'   geom_point()
#'
#' # Add annotation
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, label = Species)) +
#'   geom_point()
#'
#' # Long descriptions are automatically wrapped to fit into the width
#' iris$desc <- c(
#'   'A super Iris - and it knows it',
#'   'Pretty mediocre Iris, but give it a couple of years and it might surprise you',
#'   "You'll never guess what this Iris does every Sunday"
#' )[iris$Species]
#'
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, label = Species, description = desc,
#'                      filter = Species == 'setosa')) +
#'   geom_point()
#'
#' # Change the buffer size to move labels farther away (or closer) from the
#' # marks
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, label = Species),
#'                  label.buffer = unit(40, 'mm')) +
#'   geom_point()
#'
#' # The connector is capped a bit before it reaches the mark, but this can be
#' # controlled
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, label = Species),
#'                  con.cap = 0) +
#'   geom_point()
#'
#' # If you want to use the scaled colours for the labels or connectors you can
#' # use the "inherit" keyword instead
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, label = Species),
#'                  label.fill = "inherit") +
#'   geom_point()
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
