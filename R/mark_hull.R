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
#' @section Aesthetics:
#' geom_mark_hull understand the following aesthetics (required aesthetics are in
#' bold):
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
#' @param concavity A meassure of the concavity of the hull. `1` is very concave
#' while it approaches convex as it grows. Defaults to `2`
#'
#' @author Thomas Lin Pedersen
#'
#' @name geom_mark_hull
#' @rdname geom_mark_hull
#'
#' @examples
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, filter = Species != 'versicolor')) +
#'   geom_point()
#'
#' # Adjusting the concavity lets you change the shape of the hull
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, filter = Species != 'versicolor'),
#'                  concavity = 1) +
#'   geom_point()
#'
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_hull(aes(fill = Species, filter = Species != 'versicolor'),
#'                  concavity = 10) +
#'   geom_point()
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto zeroGrob
#' @export
GeomMarkHull <- ggproto('GeomMarkHull', GeomShape,
    setup_data = function(data, params) {
        if (!is.null(data$filter)) data <- data[data$filter, ]
        data
    },
    draw_panel = function(data, panel_params, coord, expand = unit(5, 'mm'), radius = unit(2.5, 'mm'), concavity = 2) {
        if (nrow(data) == 0) return(zeroGrob())

        coords <- coord$transform(data, panel_params)

        coords <- coords[order(coords$group), ]

        # For gpar(), there is one entry per polygon (not one entry per point).
        # We'll pull the first value from each group, and assume all these values
        # are the same within each group.
        first_idx <- !duplicated(coords$group)
        first_rows <- coords[first_idx, ]

        hullGrob(coords$x, coords$y, default.units = "native",
                 id = coords$group, expand = expand, radius = radius,
                 concavity = concavity,
                 gp = gpar(
                     col = first_rows$colour,
                     fill = alpha(first_rows$fill, first_rows$alpha),
                     lwd = first_rows$size * .pt,
                     lty = first_rows$linetype
                 )
        )
    },
    default_aes = aes(fill = NA, colour = 'black', alpha = 0.3, size = 0.5, linetype = 1, filter = TRUE)
)

#' @rdname geom_mark_hull
#' @export
geom_mark_hull <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", expand = unit(5, 'mm'),
                           radius = unit(2.5, 'mm'), concavity = 2, ...,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomMarkHull,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            expand = expand,
            radius = radius,
            concavity = concavity,
            ...
        )
    )
}

# Helpers -----------------------------------------------------------------

hullGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                     id.lengths = NULL, expand = 0, radius = 0, concavity = 2,
                     default.units = "npc", name = NULL, gp = gpar(), vp = NULL) {
    grob <- shapeGrob(x = x, y = y, id = id, id.lengths = id.lengths,
                      expand = expand, radius = radius,
                      default.units = default.units, name = name, gp = gp,
                      vp = vp)
    grob$cl <- 'hull'
    class(grob)[1] <- 'hull'
    grob$concavity <- concavity
    grob
}
#' Calculate the hull of points and draw it as a shapeGrob
#'
#' This function takes care of calculating the hull (concave or convex) of sets
#' of points and forwards these to `shapeGrob()`. The calculations happens at
#' draw time and is thus sensitive to the aspect ratio of the plot in order to
#' ensure the most pleasing result. This can affect performance though.
#'
#' @param x A hull grob
#'
#' @return A shape grob
#'
#' @importFrom grid convertX convertY unit makeContent
#' @importFrom concaveman concaveman
#' @export
#' @keywords internal
#'
makeContent.hull <- function(x) {
    id.length <- lengths(split(seq_along(x$id), x$id))
    type <- ifelse(id.length == 1, 'point', ifelse(id.length == 2, 'line', 'polygon'))
    x_new <- convertX(x$x, 'mm', TRUE)
    x_new <- split(x_new, x$id)
    y_new <- convertY(x$y, 'mm', TRUE)
    y_new <- split(y_new, x$id)
    polygons <- Map(function(xx, yy, type) {
        mat <- cbind(xx, yy)
        if (type != 'polygon') return(mat)
        concaveman(mat, x$concavity)
    }, xx = x_new, yy = y_new, type = type)
    x$id <- rep(seq_along(polygons), vapply(polygons, nrow, numeric(1)))
    polygons <- do.call(rbind, polygons)
    x$x <- unit(polygons[,1], 'mm')
    x$y <- unit(polygons[,2], 'mm')
    class(x)[1] <- 'shape'
    x$cl <- 'shape'
    makeContent(x)
}
