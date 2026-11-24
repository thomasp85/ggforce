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
#' @inheritParams geom_shape
#'
#' @param n The number of points used to draw each circle. Defaults to `100`.
#' @param label.margin The margin around the annotation boxes, given by a call
#' to [ggplot2::margin()].
#' @param label.maxwidth The maximum width to allow before starting to wrap the
#' text. If you set the `width` aesthetic then it will overwrite this.
#' @param label.width `r lifecycle::badge('deprecated')` Use the `width`
#' aesthetic instead.
#' @param label.minwidth `r lifecycle::badge('deprecated')` No longer in use.
#' @param label.fontsize,label.family,label.fontface,label.lineheight,label.hjust,label.fill,label.colour `r lifecycle::badge('deprecated')` Use the `style` aesthetic instead and construct an appropriate style with [mark_style()]
#' @param label.buffer The size of the region around the mark where labels
#' cannot be placed.
#' @param con.colour The colour for the line connecting the annotation to the
#' mark. Use `"inherit"` to use the border colour of the enclosure or
#' `"inherit_fill"` to use the fill colour from the enclosure.
#' @param con.size The width of the connector. Use `"inherit"` to use the border
#' width of the enclosure.
#' @param con.type The type of the connector. Either `"elbow"`, `"straight"`, or
#' `"none"`.
#' @param con.linetype The linetype of the connector. Use `"inherit"` to use the
#' border linetype of the enclosure.
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
#'
#' # If you want to use the scaled colours for the labels or connectors you can
#' # use the "inherit" keyword instead
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_circle(aes(fill = Species, label = Species),
#'                    label.fill = "inherit") +
#'   geom_point()
#'
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomMarkCircle <- ggproto('GeomMarkCircle', GeomShape,
  setup_data = function(self, data, params) {
    if (!is.null(data$filter)) {
      data$filter <- ifelse(is.na(data$filter), FALSE, data$filter)
      self$removed <- data[!data$filter, c('x', 'y', 'PANEL')]
      data <- data[data$filter, ]
    }
    data
  },
  draw_panel = function(self, data, panel_params, coord, expand = unit(5, 'mm'),
                        radius = expand, n = 100, label.buffer = unit(10, 'mm'),
                        label.maxwidth = unit(50, "mm"), con.colour = 'black',
                        con.size = 0.5, con.type = 'elbow', con.linetype = 1,
                        con.border = 'one', con.cap = unit(3, 'mm'), con.arrow = NULL,
                        label.fill = NULL, label.colour = NULL) {
    if (nrow(data) == 0) return(zeroGrob())

    # As long as coord$transform() doesn't recognise x0/y0
    data$xmin <- data$x0
    data$ymin <- data$y0
    coords <- coord$transform(data, panel_params)
    # coord transforms can produce non-finite positions (e.g. out-of-range
    # values under transformed coords) which would poison the enclosing-shape
    # and label-placement geometry
    coords <- coords[is.finite(coords$x) & is.finite(coords$y), ]
    if (nrow(coords) == 0) return(zeroGrob())
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
      label <- apply_label_style(first_rows, label.fill, label.colour)
      is_ghost <- which(self$removed$PANEL == coords$PANEL[1])
      if (length(is_ghost) > 0) {
        ghosts <- self$removed[is_ghost, ]
        ghosts <- coord$transform(ghosts, panel_params)
        ghost_keep <- is.finite(ghosts$x) & is.finite(ghosts$y)
        ghosts <- list(x = ghosts$x[ghost_keep], y = ghosts$y[ghost_keep])
      }
    }

    gp <- gpar(
      col = first_rows$colour,
      fill = ggplot2::fill_alpha(first_rows$fill, first_rows$alpha),
      lwd = (first_rows$linewidth %||% first_rows$size) * .pt,
      lty = first_rows$linetype,
      fontsize = (first_rows$size %||% 4.217518) * .pt
    )

    circEncGrob(coords$x, coords$y,
      default.units = 'native',
      id = coords$group, expand = expand, radius = radius, n = n,
      label = label, ghosts = ghosts,
      mark.gp = gp,
      con.gp = inherit_gp(
        col = con.colour,
        fill = con.colour,
        lwd = if (is.numeric(con.size)) con.size * .pt else con.size,
        lty = con.linetype,
        gp = gp
      ),
      label.buffer = label.buffer,
      label.maxwidth = label.maxwidth,
      con.type = con.type,
      con.border = con.border,
      con.cap = con.cap,
      con.arrow = con.arrow,
      anchor.x = first_rows$xmin,
      anchor.y = first_rows$ymin
    )
  },
  default_aes = combine_aes(
    GeomShape$default_aes,
    aes(fill = NA, colour = 'black', alpha = 0.3, filter = NULL, label = NULL,
        description = NULL, x0 = NULL, y0 = NULL, style = mark_style(), width = NA)
  ),
  handle_na = function(self, data, params) {
    remove_missing(data, params$na.rm,
                   c(self$required_aes, self$non_missing_aes),
                   snake_class(self)
    )
  },
  extra_params = 'na.rm'
)

#' @rdname geom_mark_circle
#' @export
geom_mark_circle <- function(mapping = NULL, data = NULL, stat = 'identity',
                             position = 'identity', expand = unit(5, 'mm'),
                             radius = expand, n = 100, label.maxwidth = unit(50, 'mm'),
                             label.buffer = unit(10, 'mm'), con.colour = 'black',
                             con.size = 0.5, con.type = 'elbow',
                             con.linetype = 1, con.border = 'one',
                             con.cap = unit(3, 'mm'), con.arrow = NULL, ...,
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE,
                             label.margin = deprecated(),
                             label.width = deprecated(),
                             label.minwidth = deprecated(),
                             label.hjust = deprecated(),
                             label.fontsize = deprecated(),
                             label.family = deprecated(),
                             label.lineheight = deprecated(),
                             label.fontface = deprecated(),
                             label.fill = deprecated(),
                             label.colour = deprecated()
                            ) {
  if (lifecycle::is_present(label.margin)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.margin)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.width)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.width)", I("the `width` aesthetic instead"))
  }
  if (lifecycle::is_present(label.minwidth)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.minwidth)")
  }
  if (lifecycle::is_present(label.hjust)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.hjust)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.fontsize)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.fontsize)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.family)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.family)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.lineheight)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.lineheight)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.fontface)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.fontface)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.fill)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.fill)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.colour)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_circle(label.colour)", I("the `style` aesthetic instead"))
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMarkCircle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      expand = expand,
      radius = radius,
      n = n,
      label.maxwidth = label.maxwidth,
      label.buffer = label.buffer,
      con.colour = con.colour,
      con.size = con.size,
      con.type = con.type,
      con.linetype = con.linetype,
      con.border = con.border,
      con.cap = con.cap,
      con.arrow = con.arrow,
      label.fill = if (lifecycle::is_present(label.fill)) label.fill else NULL,
      label.colour = if (lifecycle::is_present(label.colour)) label.colour else NULL,
      ...
    )
  )
}

# Helpers -----------------------------------------------------------------

#' @importFrom grDevices chull
#' @importFrom grid gTree
circEncGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                        id.lengths = NULL, expand = 0, radius = 0, n = 100,
                        label = NULL, ghosts = NULL, default.units = 'npc',
                        name = NULL, mark.gp = gpar(), con.gp = gpar(),
                        label.buffer = unit(10, 'mm'), label.maxwidth = unit(50, "mm"),
                        con.type = 'elbow', con.border = 'one',
                        con.cap = unit(3, 'mm'), con.arrow = NULL,
                        anchor.x = NULL, anchor.y = NULL, vp = NULL) {
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
    if (length(unique0(xi)) == 1) {
      return(i[c(which.min(yi), which.max(yi))])
    }
    if (length(unique0(yi)) == 1) {
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
    label_width <- as_mm(label$width, "mm")
    max_width <- as_mm(label.maxwidth)
    label <- lapply(seq_len(nrow(label)), function(i) {
      style <- label$style[i]
      if (con.border == 'all') {
        con.gp <- subset_gp(con.gp, i)
        style <- marquee::modify_style(
          style,
          "body",
          border = con.gp$col,
          border_width = marquee::trbl(grid::unit(con.gp$lwd, "pt")),
          border_type = con.gp$lty
        )
      }
      desc <- as.character(label$description[i] %||% "") %|% ""
      if (!is.null(label$label) && !is.na(label$label[i])) {
        desc <- sub("\n$", "", paste0("##### ", as.character(label$label[i]) %|% "", "\n", desc))
      }

      if (nchar(desc) == 0) return(zeroGrob())
      grob <- marquee::marquee_grob(desc, style, force_body_margin = TRUE, x = 0, y = 0, width = grid::unit(label_width[i], "mm"), hjust = 0.5, vjust = 0.5)
      if (is.na(label$width[i]) && as_mm(grobWidth(grob)) > max_width) {
        grob <- marquee::marquee_grob(desc, style, force_body_margin = TRUE, x = 0, y = 0, width = grid::unit(max_width, "mm"), hjust = 0.5, vjust = 0.5)
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
    mark = mark, n = n, label = label, labeldim = labeldim,
    buffer = label.buffer, ghosts = ghosts, con.gp = con.gp, con.type = con.type,
    con.cap = as_mm(con.cap, default.units), con.border = con.border,
    con.arrow = con.arrow, anchor.x = anchor.x, anchor.y = anchor.y,
    name = name, vp = vp, cl = 'circ_enc'
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
  circles <- unique0(circles)
  mark$x <- unit(circles$x, 'mm')
  mark$y <- unit(circles$y, 'mm')
  mark$id <- circles$id
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
