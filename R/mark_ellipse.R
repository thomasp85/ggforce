#' Annotate areas with ellipses
#'
#' This geom lets you annotate sets of points via ellipses. The enclosing
#' ellipses are estimated using the Khachiyan algorithm which guarantees an
#' optimal solution within the given tolerance level. As this geom is often
#' expanded it is of lesser concern that some points are slightly outside the
#' ellipsis. The Khachiyan algorithm has polynomial complexity and can thus
#' suffer from scaling issues. Still, it is only calculated on the convex hull
#' of the groups, so performance issues should be rare (it can easily handle a
#' hull consisting of 1000 points).
#'
#' @inheritSection geom_mark_circle Annotation
#' @inheritSection geom_mark_circle Filtering
#' @section Aesthetics:
#' `geom_mark_ellipse` understands the following aesthetics (required aesthetics are
#' in bold):
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
#' @inheritParams geom_mark_circle
#'
#' @param n The number of points used to draw each ellipse. Defaults to `100`.
#' @param tol The tolerance cutoff. Lower values will result in ellipses closer
#' to the optimal solution. Defaults to `0.01`.
#'
#' @family mark geoms
#'
#' @name geom_mark_ellipse
#' @rdname geom_mark_ellipse
#'
#' @examples
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_ellipse(aes(fill = Species, filter = Species != 'versicolor')) +
#'   geom_point()
#'
#' # Add annotation
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_ellipse(aes(fill = Species, label = Species)) +
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
#'   geom_mark_ellipse(aes(fill = Species, label = Species, description = desc,
#'                         filter = Species == 'setosa')) +
#'   geom_point()
#'
#' # Change the buffer size to move labels farther away (or closer) from the
#' # marks
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_ellipse(aes(fill = Species, label = Species),
#'                     label.buffer = unit(40, 'mm')) +
#'   geom_point()
#'
#' # The connector is capped a bit before it reaches the mark, but this can be
#' # controlled
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_ellipse(aes(fill = Species, label = Species),
#'                     con.cap = 0) +
#'   geom_point()
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomMarkEllipse <- ggproto('GeomMarkEllipse', GeomShape,
  setup_data = function(self, data, params) {
    if (!is.null(data$filter)) {
      self$removed <- data[!data$filter, c('x', 'y', 'PANEL')]
      data <- data[data$filter, ]
    }
    data
  },
  draw_panel = function(self, data, panel_params, coord, expand = unit(5, 'mm'),
                        radius = expand, n = 100, tol = 0.01,
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

    coords <- coord$transform(data, panel_params)
    if (!is.integer(coords$group)) {
      coords$group <- match(coords$group, unique(coords$group))
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

    ellipEncGrob(coords$x, coords$y,
      default.units = 'native',
      id = coords$group, expand = expand, radius = radius, n = n,
      tol = tol, label = label, ghosts = ghosts,
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
      con.arrow = con.arrow
    )
  },
  default_aes = GeomMarkCircle$default_aes
)

#' @rdname geom_mark_ellipse
#' @export
geom_mark_ellipse <- function(mapping = NULL, data = NULL, stat = 'identity',
                              position = 'identity', expand = unit(5, 'mm'),
                              radius = expand, n = 100, tol = 0.01,
                              label.margin = margin(2, 2, 2, 2, 'mm'),
                              label.width = NULL,
                              label.minwidth = unit(50, 'mm'),
                              label.hjust = 0, label.fontsize = 12,
                              label.family = '', label.lineheight = 1,
                              label.fontface = c('bold', 'plain'),
                              label.fill = 'white',
                              label.colour = 'black',
                              label.buffer = unit(10, 'mm'),
                              con.colour = 'black', con.size = 0.5,
                              con.type = 'elbow', con.linetype = 1,
                              con.border = 'one', con.cap = unit(3, 'mm'),
                              con.arrow = NULL, ..., na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMarkEllipse,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      expand = expand,
      radius = radius,
      n = n,
      tol = tol,
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

#' @importFrom grDevices chull
ellipEncGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                         id.lengths = NULL, expand = 0, radius = 0, n = 100,
                         tol = 0.01, label = NULL, ghosts = NULL,
                         default.units = 'npc', name = NULL, mark.gp = gpar(),
                         label.gp = gpar(), con.gp = gpar(),
                         label.margin = margin(), label.width = NULL,
                         label.minwidth = unit(50, 'mm'), label.hjust = 0,
                         label.buffer = unit(10, 'mm'), con.type = 'elbow',
                         con.border = 'one', con.cap = unit(3, 'mm'),
                         con.arrow = NULL, vp = NULL) {
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
  include <- unlist(lapply(split(seq_along(x), id), function(i) {
    xi <- x[i]
    yi <- y[i]
    if (length(unique(xi)) == 1) {
      return(i[c(which.min(yi), which.max(yi))])
    }
    if (length(unique(yi)) == 1) {
      return(i[c(which.min(xi), which.max(xi))])
    }
    i[chull(xi, yi)]
  }))
  mark <- shapeGrob(
    x = x[include], y = y[include], id = id[include],
    id.lengths = NULL, expand = expand, radius = radius,
    default.units = default.units, name = name, gp = mark.gp,
    vp = vp
  )
  if (!is.null(label)) {
    label <- lapply(seq_len(nrow(label)), function(i) {
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
  gTree(
    mark = mark, n = n, tol = tol, label = label, labeldim = labeldim,
    buffer = label.buffer, ghosts = ghosts, con.gp = con.gp, con.type = con.type,
    con.cap = as_mm(con.cap, default.units), con.border = con.border,
    con.arrow = con.arrow, name = name, vp = vp, cl = 'ellip_enc'
  )
}
#' @importFrom grid convertX convertY unit makeContent childNames addGrob
#' setChildren gList
#' @export
makeContent.ellip_enc <- function(x) {
  mark <- x$mark
  x_new <- convertX(mark$x, 'mm', TRUE)
  y_new <- convertY(mark$y, 'mm', TRUE)
  ellipses <- enclose_ellip_points(round(x_new, 2), round(y_new, 2), mark$id, x$tol)
  ellipses$id <- seq_len(nrow(ellipses))
  ellipses <- ellipses[rep(ellipses$id, each = x$n), ]
  points <- 2 * pi * (seq_len(x$n) - 1) / x$n
  x_tmp <- cos(points) * ellipses$a
  y_tmp <- sin(points) * ellipses$b
  ellipses$x <- ellipses$x0 + x_tmp * cos(ellipses$angle) - y_tmp * sin(ellipses$angle)
  ellipses$y <- ellipses$y0 + x_tmp * sin(ellipses$angle) + y_tmp * cos(ellipses$angle)
  ellipses <- unique(ellipses)
  mark$x <- unit(ellipses$x, 'mm')
  mark$y <- unit(ellipses$y, 'mm')
  mark$id <- ellipses$id
  if (inherits(mark, 'shape')) mark <- makeContent(mark)
  if (!is.null(x$label)) {
    polygons <- Map(function(x, y) list(x = x, y = y),
      x = split(as.numeric(mark$x), mark$id),
      y = split(as.numeric(mark$y), mark$id)
    )
    labels <- make_label(
      labels = x$label, dims = x$labeldim, polygons = polygons,
      ghosts = x$ghosts, buffer = x$buffer, con_type = x$con.type,
      con_border = x$con.border, con_cap = x$con.cap,
      con_gp = x$con.gp, anchor_mod = 2, arrow = x$con.arrow
    )
    setChildren(x, do.call(gList, c(list(mark), labels)))
  } else {
    setChildren(x, gList(mark))
  }
}
