#' Annotate areas with circles
#'
#' This geom lets you annotate sets of points via circles. The enclosing circles
#' are calculated at draw time and the most optimal enclosure at the given
#' aspect ratio is thus guaranteed. As with the other `geom_mark_*` geoms the
#' enclosure inherits from [geom_shape()] and defaults to be expanded slightly
#' to better enclose the points.
#'
#' @section Annotation:
#' All `geom_mark_*` allow you to put descriptive textboxes connected to the
#' mark on the plot, using the `label` and `description` aesthetics. The
#' textboxes are automatically placed close to the mark, but without obscuring
#' any of the datapoints in the layer. The placement is dynamic so if you resize
#' the plot you'll see that the annotation might move around as areas become big
#' enough or too small to fit the annotation. If there's not enough space for
#' the annotation without overlapping data it will not get drawn. In these cases
#' try resizing the plot, change the size of the annotation, or decrease the
#' buffer region around the marks.
#'
#' @section Filtering:
#' Often marks are used to draw attention to, or annotate specific features of
#' the plot and it is thus not desirable to have marks around everything. While
#' it is possible to simply pre-filter the data used for the mark layer, the
#' `geom_mark_*` geoms also comes with a dedicated `filter` aesthetic that, if
#' set, will remove all rows where it evalutates to `FALSE`. There are
#' multiple benefits of using this instead of prefiltering. First, you don't
#' have to change your data source, making your code more adaptable for
#' exploration. Second, the data removed by the filter aesthetic is remembered
#' by the geom, and any annotation will take care not to overlap with the
#' removed data.
#'
#' @section Aesthetics:
#' geom_mark_circle understand the following aesthetics (required aesthetics are
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
#' @inheritParams geom_shape
#'
#' @param n The number of points used to draw each circle. Defaults to `100`.
#' @param label.margin The margin around the annotation boxes, given by a call
#' to [ggplot2::margin()].
#' @param label.width A fixed width for the label. Set to `NULL` to let the text
#' or `label.minwidth` decide.
#' @param label.minwidth The minimum width to provide for the description. If
#' the size of the label exceeds this, the description is allowed to fill as
#' much as the label.
#' @param label.fontsize The size of the text for the annotation. If it contains
#' two elements the first will be used for the label and the second for the
#' description.
#' @param label.family The font family used for the annotation. If it contains
#' two elements the first will be used for the label and the second for the
#' description.
#' @param label.fontface The font face used for the annotation. If it contains
#' two elements the first will be used for the label and the second for the
#' description.
#' @param label.lineheight The height of a line as a multipler of the fontsize.
#' If it contains two elements the first will be used for the label and the
#' second for the description.
#' @param label.hjust The horizontal justification for the annotation. If it
#' contains two elements the first will be used for the label and the second for
#' the description.
#' @param label.fill The fill colour for the annotation box.
#' @param label.colour The text colour for the annotation. If it contains
#' two elements the first will be used for the label and the second for the
#' description.
#' @param label.buffer The size of the region around the mark where labels
#' cannot be placed.
#' @param con.colour The colour for the line connecting the annotation to the
#' mark.
#' @param con.size The width of the connector.
#' @param con.type The type of the connector. Either `"elbow"`, `"straight"`, or
#' `"none"`.
#' @param con.linetype The linetype of the connector.
#' @param con.border The bordertype of the connector. Either `"one"` (to draw a
#' line on the horizontal side closest to the mark), `"all"` (to draw a border
#' on all sides), or `"none"` (not going to explain that one).
#' @param con.cap The distance before the mark that the line should stop at.
#' @param con.arrow An arrow specification for the connection using
#' [grid::arrow()] for the end pointing towards the mark.
#'
#' @family mark geoms
#'
#' @name geom_mark_circle
#' @rdname geom_mark_circle
#'
#' @examples
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_circle(aes(fill = Species, filter = Species != 'versicolor')) +
#'   geom_point()
#'
#' # Add annotation
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_circle(aes(fill = Species, label = Species)) +
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
#'   geom_mark_circle(aes(fill = Species, label = Species, description = desc,
#'                        filter = Species == 'setosa')) +
#'   geom_point()
#'
#' # Change the buffer size to move labels farther away (or closer) from the
#' # marks
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_circle(aes(fill = Species, label = Species),
#'                    label.buffer = unit(30, 'mm')) +
#'   geom_point()
#'
#' # The connector is capped a bit before it reaches the mark, but this can be
#' # controlled
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_circle(aes(fill = Species, label = Species),
#'                    con.cap = 0) +
#'   geom_point()
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomMarkCircle <- ggproto('GeomMarkCircle', GeomShape,
  setup_data = function(self, data, params) {
    if (!is.null(data$filter)) {
      self$removed <- data[!data$filter, c('x', 'y', 'PANEL')]
      data <- data[data$filter, ]
    }
    data
  },
  draw_panel = function(self, data, panel_params, coord, expand = unit(5, 'mm'),
                        radius = expand, n = 100,
                        label.margin = margin(2, 2, 2, 2, 'mm'),
                        label.width = NULL, label.minwidth = unit(50, 'mm'),
                        label.hjust = 0, label.buffer = unit(10, 'mm'),
                        label.fontsize = 12, label.family = '',
                        label.fontface = c('bold', 'plain'),
                        label.fill = 'white', label.colour = 'black',
                        label.lineheight = 1,
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

    circEncGrob(coords$x, coords$y,
      default.units = 'native',
      id = coords$group, expand = expand, radius = radius, n = n,
      label = label, ghosts = ghosts,
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
      con.arrow = con.arrow
    )
  },
  default_aes = combine_aes(
    GeomShape$default_aes,
    aes(fill = NA, colour = 'black', alpha = 0.3, filter = NULL, label = NULL,
        description = NULL)
  )
)

#' @rdname geom_mark_circle
#' @export
geom_mark_circle <- function(mapping = NULL, data = NULL, stat = 'identity',
                             position = 'identity', expand = unit(5, 'mm'),
                             radius = expand, n = 100,
                             label.margin = margin(2, 2, 2, 2, 'mm'),
                             label.width = NULL, label.minwidth = unit(50, 'mm'),
                             label.hjust = 0, label.fontsize = 12,
                             label.family = '', label.lineheight = 1,
                             label.fontface = c('bold', 'plain'),
                             label.fill = 'white', label.colour = 'black',
                             label.buffer = unit(10, 'mm'), con.colour = 'black',
                             con.size = 0.5, con.type = 'elbow',
                             con.linetype = 1, con.border = 'one',
                             con.cap = unit(3, 'mm'), con.arrow = NULL, ...,
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMarkCircle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      expand = expand,
      radius = radius,
      n = n,
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
circEncGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                        id.lengths = NULL, expand = 0, radius = 0, n = 100,
                        label = NULL, ghosts = NULL, default.units = 'npc',
                        name = NULL, mark.gp = gpar(), label.gp = gpar(),
                        con.gp = gpar(), label.margin = margin(),
                        label.width = NULL, label.minwidth = unit(50, 'mm'),
                        label.hjust = 0, label.buffer = unit(10, 'mm'),
                        con.type = 'elbow', con.border = 'one',
                        con.cap = unit(3, 'mm'), con.arrow = NULL, vp = NULL) {
  if (is.null(id)) {
    if (is.null(id.lengths)) {
      id <- rep(1, length(x))
    } else {
      id <- rep(seq_along(id.lengths), id.lengths)
      if (length(id) != length(x)) {
        cli::cli_abort('{.arg id.lengths} must sum up to the number of points')
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
    mark = mark, n = n, label = label, labeldim = labeldim,
    buffer = label.buffer, ghosts = ghosts, con.gp = con.gp, con.type = con.type,
    con.cap = as_mm(con.cap, default.units), con.border = con.border,
    con.arrow = con.arrow, name = name, vp = vp, cl = 'circ_enc'
  )
}
#' @importFrom grid convertX convertY unit makeContent setChildren gList
#' @export
makeContent.circ_enc <- function(x) {
  mark <- x$mark
  x_new <- convertX(mark$x, 'mm', TRUE)
  y_new <- convertY(mark$y, 'mm', TRUE)
  circles <- enclose_points(round(x_new, 2), round(y_new, 2), mark$id)
  circles$id <- seq_len(nrow(circles))
  circles <- circles[rep(circles$id, each = x$n), ]
  points <- 2 * pi * (seq_len(x$n) - 1) / x$n
  circles$x <- circles$x0 + cos(points) * circles$r
  circles$y <- circles$y0 + sin(points) * circles$r
  circles <- unique(circles)
  mark$x <- unit(circles$x, 'mm')
  mark$y <- unit(circles$y, 'mm')
  mark$id <- circles$id
  if (inherits(mark, 'shape')) makeContent(mark)
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
