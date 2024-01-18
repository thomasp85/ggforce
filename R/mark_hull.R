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
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomMarkHull <- ggproto('GeomMarkHull', GeomMarkCircle,
  draw_panel = function(self, data, panel_params, coord, expand = unit(5, 'mm'),
                        radius = unit(2.5, 'mm'), concavity = 2,
                        label.margin = margin(2, 2, 2, 2, 'mm'),
                        label.width = NULL, label.minwidth = unit(50, 'mm'),
                        label.hjust = 0, label.buffer = unit(10, 'mm'),
                        label.fontsize = 12, label.family = '',
                        label.fontface = c('bold', 'plain'),
                        label.lineheight = 1,
                        label.fill = 'white', label.colour = 'black',
                        con.colour = 'black', con.size = 0.5, con.type = 'elbow',
                        con.linetype = 1, con.border = 'one',
                        con.cap = unit(3, 'mm'), con.arrow = NULL) {
    if (nrow(data) == 0) return(zeroGrob())

    # As long as coord$transform() doesn't recognise x0/y0
    data$xmin <- data$x0
    data$ymin <- data$y0
    coords <- coord$transform(data, panel_params)
    if (!is.integer(coords$group)) {
      coords$group <- match(coords$group, unique0(coords$group))
    }
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


    hullEncGrob(coords$x, coords$y,
      default.units = 'native',
      id = coords$group, expand = expand, radius = radius,
      concavity = concavity, label = label, ghosts = ghosts,
      mark.gp = gpar(
        col = first_rows$colour,
        fill = alpha(first_rows$fill, first_rows$alpha),
        lwd = (first_rows$linewidth %||% first_rows$size) * .pt,
        lty = first_rows$linetype
      ),
      label.gp = gpar(
        col = label.colour,
        fill = label.fill,
        fontface = label.fontface,
        fontfamily = label.family,
        fontsize = label.fontsize,
        lineheight = label.lineheight
      ),
      con.gp = gpar(
        col = con.colour,
        fill = con.colour,
        lwd = con.size * .pt,
        lty = con.linetype
      ),
      label.margin = label.margin,
      label.width = label.width,
      label.minwidth = label.minwidth,
      label.hjust = label.hjust,
      label.buffer = label.buffer,
      con.type = con.type,
      con.border = con.border,
      con.cap = con.cap,
      con.arrow = con.arrow,
      anchor.x = first_rows$xmin,
      anchor.y = first_rows$ymin
    )
  }
)

#' @rdname geom_mark_hull
#' @export
geom_mark_hull <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', expand = unit(5, 'mm'),
                           radius = unit(2.5, 'mm'), concavity = 2,
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

# Helpers -----------------------------------------------------------------

hullEncGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                        id.lengths = NULL, expand = 0, radius = 0, concavity = 2,
                        label = NULL, ghosts = NULL, default.units = 'npc',
                        name = NULL, mark.gp = gpar(), label.gp = gpar(),
                        con.gp = gpar(), label.margin = margin(),
                        label.width = NULL, label.minwidth = unit(50, 'mm'),
                        label.hjust = 0, label.buffer = unit(10, 'mm'),
                        con.type = 'elbow', con.border = 'one',
                        con.cap = unit(3, 'mm'), con.arrow = NULL,
                        anchor.x = NULL, anchor.y = NULL, vp = NULL) {
  mark <- shapeGrob(
    x = x, y = y, id = id, id.lengths = id.lengths,
    expand = expand, radius = radius,
    default.units = default.units, name = name, gp = mark.gp,
    vp = vp
  )
  if (!is.null(label)) {
    label <- lapply(seq_len(nrow(label)), function(i) {
      if (is.na(label$label[i] %||% NA) && is.na(label$description[i] %||% NA)) return(zeroGrob())
      grob <- labelboxGrob(label$label[i], 0, 0, label$description[i],
        gp = label.gp, pad = label.margin, width = label.width,
        min.width = label.minwidth, hjust = label.hjust
      )
      if (con.border == 'all') {
        grob$children[[1]]$gp$col <- con.gp$col
        grob$children[[1]]$gp$lwd <- con.gp$lwd
        grob$children[[1]]$gp$lty <- con.gp$lty
      }
      grob
    })
    labeldim <- lapply(label, function(l) {
      c(
        convertWidth(grobWidth(l), 'mm', TRUE),
        convertHeight(grobHeight(l), 'mm', TRUE)
      )
    })
    ghosts <- lapply(ghosts, unit, default.units)
  } else {
    labeldim <- NULL
  }
  if (!is.null(anchor.x) && !is.unit(anchor.x)) {
    anchor.x <- unit(anchor.x, default.units)
  }
  if (!is.null(anchor.y) && !is.unit(anchor.y)) {
    anchor.y <- unit(anchor.y, default.units)
  }
  gTree(
    mark = mark, concavity = concavity, label = label, labeldim = labeldim,
    buffer = label.buffer, ghosts = ghosts, con.gp = con.gp, con.type = con.type,
    con.cap = as_mm(con.cap, default.units), con.border = con.border,
    con.arrow = con.arrow, anchor.x = anchor.x, anchor.y = anchor.y, name = name,
    vp = vp, cl = 'hull_enc'
  )
}
#' @importFrom grid convertX convertY unit makeContent setChildren gList
#' @export
makeContent.hull_enc <- function(x) {
  mark <- x$mark
  x_new <- convertX(mark$x, 'mm', TRUE)
  x_new <- split(x_new, mark$id)
  y_new <- convertY(mark$y, 'mm', TRUE)
  y_new <- split(y_new, mark$id)
  polygons <- Map(function(xx, yy, type) {
    mat <- unique0(cbind(xx, yy))
    if (nrow(mat) <= 2) {
      return(mat)
    }
    if (length(unique0(xx)) == 1) {
      return(mat[c(which.min(mat[, 2]), which.max(mat[, 2])), ])
    }
    if (length(unique0((yy[-1] - yy[1]) / (xx[-1] - xx[1]))) == 1) {
      return(mat[c(which.min(mat[, 1]), which.max(mat[, 1])), ])
    }
    concaveman(mat, x$concavity, 0)
  }, xx = x_new, yy = y_new)
  # ensure that all polygons have the same set of column names
  polygons <- lapply(polygons, function(x) {
    colnames(x) <- c("x", "y")
    return(x)
  })
  mark$id <- rep(seq_along(polygons), vapply(polygons, nrow, numeric(1)))
  polygons <- vec_rbind(!!!polygons)
  mark$x <- unit(polygons[, 1], 'mm')
  mark$y <- unit(polygons[, 2], 'mm')
  if (inherits(mark, 'shape')) mark <- makeContent(mark)
  if (!is.null(x$label)) {
    polygons <- Map(function(x, y) list(x = x, y = y),
      x = split(as.numeric(mark$x), mark$id),
      y = split(as.numeric(mark$y), mark$id)
    )
    anchor_x <- if (is.null(x$anchor.x)) NULL else convertX(x$anchor.x, 'mm', TRUE)
    anchor_y <- if (is.null(x$anchor.y)) NULL else convertY(x$anchor.y, 'mm', TRUE)
    labels <- make_label(
      labels = x$label, dims = x$labeldim, polygons = polygons,
      ghosts = x$ghosts, buffer = x$buffer, con_type = x$con.type,
      con_border = x$con.border, con_cap = x$con.cap,
      con_gp = x$con.gp, anchor_mod = 2, anchor_x = anchor_x,
      anchor_y = anchor_y, arrow = x$con.arrow
    )
    setChildren(x, inject(gList(!!!c(list(mark), labels))))
  } else {
    setChildren(x, gList(mark))
  }
}
