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
#' - label
#' - description
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
    setup_data = function(self, data, params) {
        if (!is.null(data$filter)) {
            self$removed <- data[!data$filter, c('x', 'y', 'PANEL')]
            data <- data[data$filter, ]
        }
        data
    },
    draw_panel = function(self, data, panel_params, coord, expand = unit(5, 'mm'),
                          radius = unit(2.5, 'mm'), concavity = 2,
                          label.margin = margin(2, 2, 2, 2, 'mm'),
                          label.width = NULL, label.minwidth = unit(50, 'mm'),
                          label.hjust = 0,
                          label.fontsize = 12, label.family = '',
                          label.fontface = c('bold', 'plain'), label.fill = 'white',
                          label.colour = 'black', con.colour = 'black', con.size = 0.5,
                          con.type = 'elbow', con.linetype = 1, con.border = 'one',
                          con.cap = unit(3, 'mm')) {
        if (!requireNamespace('concaveman', quietly = TRUE)) {
            stop("The concaveman package is required to use geom_mark_hull", call. = FALSE)
        }
        if (nrow(data) == 0) return(zeroGrob())

        coords <- coord$transform(data, panel_params)

        coords <- coords[order(coords$group), ]

        # For gpar(), there is one entry per polygon (not one entry per point).
        # We'll pull the first value from each group, and assume all these values
        # are the same within each group.
        first_idx <- !duplicated(coords$group)
        first_rows <- coords[first_idx, ]

        label <- NULL
        ghosts <- NULL
        if (!is.null(coords$label) || !is.null(coords$description)) {
            label <- first_rows
            is_ghost <- which(self$removed$PANEL == coords$PANEL[1])
            if (length(is_ghost) > 0) {
                ghosts <- self$removed[is_ghost, ]
                ghosts <- coord$transform(ghosts, panel_params)
                ghosts <- list(x = ghosts$x, y = ghosts$y)
            }
        }


        hullEncGrob(coords$x, coords$y, default.units = "native",
                 id = coords$group, expand = expand, radius = radius,
                 concavity = concavity, label = label, ghosts = ghosts,
                 mark.gp = gpar(
                     col = first_rows$colour,
                     fill = alpha(first_rows$fill, first_rows$alpha),
                     lwd = first_rows$size * .pt,
                     lty = first_rows$linetype
                 ),
                 label.gp = gpar(
                     col = label.colour,
                     fill = label.fill,
                     fontface = label.fontface,
                     fontfamily = label.family,
                     fontsize = label.fontsize
                 ),
                 con.gp = gpar(
                     col = con.colour,
                     lwd = con.size * .pt,
                     lty = con.linetype
                 ),
                 label.margin = label.margin,
                 label.width = label.width,
                 label.minwidth = label.minwidth,
                 label.hjust = label.hjust,
                 con.type = con.type,
                 con.border = con.border,
                 con.cap = con.cap
        )
    },
    default_aes = GeomMarkCircle$default_aes
)

#' @rdname geom_mark_hull
#' @export
geom_mark_hull <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", expand = unit(5, 'mm'),
                           radius = unit(2.5, 'mm'), concavity = 2,
                           label.margin = margin(2, 2, 2, 2, 'mm'),
                           label.width = NULL, label.minwidth = unit(50, 'mm'),
                           label.hjust = 0, label.fontsize = 12, label.family = '',
                           label.fontface = c('bold', 'plain'), label.fill = 'white',
                           label.colour = 'black', con.colour = 'black', con.size = 0.5,
                           con.type = 'elbow', con.linetype = 1, con.border = 'one',
                           con.cap = unit(3, 'mm'), ...,
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
            label.margin = label.margin,
            label.width = label.width,
            label.minwidth = label.minwidth,
            label.fontsize = label.fontsize,
            label.family = label.family,
            label.fontface = label.fontface,
            label.hjust = label.hjust,
            label.fill = label.fill,
            label.colour = label.colour,
            con.colour = con.colour,
            con.size = con.size,
            con.type = con.type,
            con.linetype = con.linetype,
            con.border = con.border,
            con.cap = con.cap,
            ...
        )
    )
}

# Helpers -----------------------------------------------------------------

hullEncGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                     id.lengths = NULL, expand = 0, radius = 0, concavity = 2,
                     label = NULL, ghosts = NULL, default.units = "npc",
                     name = NULL, mark.gp = gpar(), label.gp = gpar(),
                     con.gp = gpar(), label.margin = margin(), label.width = NULL,
                     label.minwidth = unit(50, 'mm'), label.hjust = 0,
                     con.type = 'elbow', con.border = 'one',
                     con.cap = unit(3, 'mm'), vp = NULL) {
    mark <- shapeGrob(x = x, y = y, id = id, id.lengths = id.lengths,
                      expand = expand, radius = radius,
                      default.units = default.units, name = name, gp = mark.gp,
                      vp = vp)
    if (!is.null(label)) {
        label <- lapply(seq_len(nrow(label)), function(i) {
            grob <- labelboxGrob(label$label[i], 0, 0, label$description[i],
                                 gp = label.gp, pad = label.margin, width = label.width,
                                 min.width = label.minwidth)
            if (con.border == 'all') {
                grob$children[[1]]$gp$col = con.gp$col
                grob$children[[1]]$gp$lwd = con.gp$lwd
                grob$children[[1]]$gp$lty = con.gp$lty
            }
            grob
        })
        labeldim <- lapply(label, function(l) {
            c(convertWidth(grobWidth(l), 'mm', TRUE),
              convertHeight(grobHeight(l), 'mm', TRUE))
        })
        ghosts <- lapply(ghosts, unit, default.units)
    } else {
        labeldim <- NULL
    }
    gTree(mark = mark, concavity = concavity, label = label, labeldim = labeldim,
          ghosts = ghosts, con.gp = con.gp, con.type = con.type,
          con.cap = as_mm(con.cap, default.units), con.border = con.border,
          name = name, vp = vp, cl = 'hull_enc')
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
makeContent.hull_enc <- function(x) {
    mark <- x$mark
    id.length <- lengths(split(seq_along(mark$id), mark$id))
    type <- ifelse(id.length == 1, 'point', ifelse(id.length == 2, 'line', 'polygon'))
    x_new <- convertX(mark$x, 'mm', TRUE)
    x_new <- split(x_new, mark$id)
    y_new <- convertY(mark$y, 'mm', TRUE)
    y_new <- split(y_new, mark$id)
    polygons <- Map(function(xx, yy, type) {
        mat <- cbind(xx, yy)
        if (type != 'polygon') return(mat)
        concaveman(mat, x$concavity)
    }, xx = x_new, yy = y_new, type = type)
    mark$id <- rep(seq_along(polygons), vapply(polygons, nrow, numeric(1)))
    polygons <- do.call(rbind, polygons)
    mark$x <- unit(polygons[,1], 'mm')
    mark$y <- unit(polygons[,2], 'mm')
    mark <- makeContent(mark)
    if (!is.null(x$label)) {
        polygons <- Map(function(x, y) list(x = x, y = y),
                        x = split(as.numeric(mark$x), mark$id),
                        y = split(as.numeric(mark$y), mark$id))
        labels <- make_label(labels = x$label, dims = x$labeldim, polygons = polygons,
                             ghosts = x$ghosts, con_type = x$con.type,
                             con_border = x$con.border, con_cap = x$con.cap,
                             con_gp = x$con.gp, anchor_mod = 2)
        grid::setChildren(x, do.call(gList, c(list(mark), labels)))
    } else {
        grid::setChildren(x, gList(mark))
    }
}
