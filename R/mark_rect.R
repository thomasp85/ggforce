#' Annotate areas with rectangles
#'
#' This geom lets you annotate sets of points via rectangles. The rectangles are
#' simply scaled to the range of the data and as with the the other
#' `geom_mark_*()` geoms expanded and have rounded corners.
#'
#' @section Aesthetics:
#' geom_mark_rect understand the following aesthetics (required aesthetics are
#' in bold):
#'
#' - **x**
#' - **y**
#' - filter
#' - color
#' - fill
#' - group
#' - size
#' - linetype
#' - alpha
#'
#' @inheritParams geom_shape
#'
#' @author Thomas Lin Pedersen
#'
#' @name geom_mark_rect
#' @rdname geom_mark_rect
#'
#' @examples
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_rect(aes(fill = Species, filter = Species != 'versicolor')) +
#'   geom_point()
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto zeroGrob
#' @export
GeomMarkRect <- ggproto('GeomMarkRect', GeomShape,
    setup_data = function(data, params) {
        if (!is.null(data$filter)) data <- data[data$filter, ]
        do.call(rbind, lapply(split(data, data$group), function(d) {
            if (nrow(d) == 1) return(d)
            x_range <- range(d$x)
            y_range <- range(d$y)
            d_new <- data.frame(x = x_range[c(1, 1, 2, 2)],
                                y = y_range[c(1, 2, 2, 1)])
            d$x <- NULL
            d$y <- NULL
            cbind(d_new, d[rep(1,4), ])
        }))
    },
    default_aes = GeomMarkHull$default_aes
)
#' @rdname geom_mark_rect
#' @export
geom_mark_rect <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", expand = unit(5, 'mm'),
                           radius = unit(2.5, 'mm'), ...,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomMarkRect,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            expand = expand,
            radius = radius,
            ...
        )
    )
}
