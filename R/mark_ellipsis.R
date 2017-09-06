#' Annotate areas with ellipses
#'
#' This geom lets you annotate sets of points via ellipses. The enclosing
#' ellipses are estimated using the Khachiyan algorithm which guarantees and
#' optimal solution within the given tolerance level. As this geom is often
#' expanded it is of lesser concern that some points are slightly outside the
#' ellipsis. The Khachiyan algorithm has polynomial complexity and can thus
#' suffer from scaling issues. Still, it is only calculated on the convex hull
#' of the groups, so performance issues should be rare (it can easily handle a
#' hull consisting of 1000 points).
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
#' @param n The number of points used to draw each circle. Defaults to `100`
#' @param tol The tolerance cutoff. Lower values will result in ellipses closer
#' to the optimal solution. Defaults to `0.01`
#'
#' @author Thomas Lin Pedersen
#'
#' @name geom_mark_ellipsis
#' @rdname geom_mark_ellipsis
#'
#' @examples
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_ellipsis(aes(fill = Species, filter = Species != 'versicolor')) +
#'   geom_point()
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto zeroGrob
#' @export
GeomMarkEllipsis <- ggproto('GeomMarkEllipsis', GeomShape,
    setup_data = function(data, params) {
        if (!is.null(data$filter)) data <- data[data$filter, ]
        data
    },
    draw_panel = function(data, panel_params, coord, expand = unit(5, 'mm'), radius = 0, n = 100, tol = 0.01) {
        if (nrow(data) == 0) return(zeroGrob())

        coords <- coord$transform(data, panel_params)

        coords <- coords[order(coords$group), ]

        # For gpar(), there is one entry per polygon (not one entry per point).
        # We'll pull the first value from each group, and assume all these values
        # are the same within each group.
        first_idx <- !duplicated(coords$group)
        first_rows <- coords[first_idx, ]

        ellipEncGrob(coords$x, coords$y, default.units = "native",
                    id = coords$group, expand = expand, radius = radius, n = n,
                    tol = tol,
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

#' @rdname geom_mark_ellipsis
#' @export
geom_mark_ellipsis <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", expand = unit(5, 'mm'),
                             radius = 0, n = 100, tol = 0.01, ...,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomMarkEllipsis,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            expand = expand,
            radius = radius,
            n = n,
            tol = tol,
            ...
        )
    )
}

# Helpers -----------------------------------------------------------------

#' @importFrom grDevices chull
ellipEncGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                        id.lengths = NULL, expand = 0, radius = 0, n = 100, tol = 0.01,
                        default.units = "npc", name = NULL, gp = gpar(), vp = NULL) {
    if (is.null(id)) {
        if (is.null(id.lengths)) {
            id <- rep(1, length(x))
        } else {
            id <- rep(seq_along(id.lengths), id.lengths)
            if (length(id) != length(x)) {
                stop('id.lengths must sum up to the number of points', call. = FALSE)
            }
        }
    }
    df <- unique(data.frame(x=x, y=y, id=id))
    include <- unlist(lapply(split(df, df$id), function(d) {
        seq_len(nrow(d)) %in% chull(d$x, d$y)
    }))
    grob <- shapeGrob(x = df$x[include], y = df$y[include], id = df$id[include], id.lengths = NULL,
                      expand = expand, radius = radius,
                      default.units = default.units, name = name, gp = gp,
                      vp = vp)
    grob$cl <- 'ellip_enc'
    class(grob)[1] <- 'ellip_enc'
    grob$n <- n
    grob$tol <- tol
    grob
}
#' Calculate the enclosing circle and draw it as a shapeGrob
#'
#' This function takes care of calculating the enclosing circle at draw time and
#' draw it as a shape grob
#'
#' @param x A circ_enc grob
#'
#' @return A shape grob
#'
#' @importFrom grid convertX convertY unit makeContent
#' @export
#' @keywords internal
#'
makeContent.ellip_enc <- function(x) {
    x_new <- convertX(x$x, 'mm', TRUE)
    y_new <- convertY(x$y, 'mm', TRUE)
    ellipses <- enclose_ellip_points(round(x_new, 2), round(y_new, 2), x$id, x$tol)
    ellipses$id <- seq_len(nrow(ellipses))
    ellipses <- ellipses[rep(ellipses$id, each = x$n), ]
    points <- 2*pi*(seq_len(x$n) - 1)/x$n
    x_tmp <- cos(points)*ellipses$a
    y_tmp <- sin(points)*ellipses$b
    ellipses$x <- ellipses$x0 + x_tmp*cos(ellipses$angle) - y_tmp*sin(ellipses$angle)
    ellipses$y <- ellipses$y0 + x_tmp*sin(ellipses$angle) + y_tmp*cos(ellipses$angle)
    ellipses <- unique(ellipses)
    x$x <- unit(ellipses$x, 'mm')
    x$y <- unit(ellipses$y, 'mm')
    x$id <- ellipses$id
    class(x)[1] <- 'shape'
    x$cl <- 'shape'
    makeContent(x)
}
