#' A point geom specialised for scatterplot matrices
#'
#' This geom is a specialisation of [ggplot2::geom_point()] with two changes. It
#' defaults to mapping `x` and `y` to `.panel_x` and `.panel_y` respectively,
#' and it defaults to using [position_auto()] to jitter the points based on the
#' combination of position scale types.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @seealso [facet_matrix] for how to lay out scatterplot matrices and
#' [position_auto] for information about the position adjustments
#'
#' @export
#'
#' @examples
#' # Continuous vs continuous: No jitter
#' ggplot(mpg) + geom_autopoint(aes(cty, hwy))
#'
#' # Continuous vs discrete: sina jitter
#' ggplot(mpg) + geom_autopoint(aes(cty, drv))
#'
#' # Discrete vs discrete: disc-jitter
#' ggplot(mpg) + geom_autopoint(aes(fl, drv))
#'
#' # Used with facet_matrix (x and y are automatically mapped)
#' ggplot(mpg) +
#'   geom_autopoint() +
#'   facet_matrix(vars(drv:fl))
#'
geom_autopoint <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "auto",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  extra_mapping <- aes(x = .panel_x, y = .panel_y)
  if (is.null(mapping$x)) mapping$x <- extra_mapping$x
  if (is.null(mapping$y)) mapping$y <- extra_mapping$y
  class(mapping) <- 'uneval'
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
