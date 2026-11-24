#' Annotate areas with rectangles
#'
#' This geom lets you annotate sets of points via rectangles. The rectangles are
#' simply scaled to the range of the data and as with the other
#' `geom_mark_*()` geoms expanded and have rounded corners.
#'
#' @inheritSection geom_mark_circle Annotation
#' @inheritSection geom_mark_circle Filtering
#' @section Aesthetics:
#' `geom_mark_rect` understands the following aesthetics (required aesthetics are
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
#' @family mark geoms
#'
#' @name geom_mark_rect
#' @rdname geom_mark_rect
#'
#' @examples
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_rect(aes(fill = Species, filter = Species != 'versicolor')) +
#'   geom_point()
#'
#' # Add annotation
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_rect(aes(fill = Species, label = Species)) +
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
#'   geom_mark_rect(aes(fill = Species, label = Species, description = desc,
#'                      filter = Species == 'setosa')) +
#'   geom_point()
#'
#' # Change the buffer size to move labels farther away (or closer) from the
#' # marks
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_rect(aes(fill = Species, label = Species),
#'                  label.buffer = unit(30, 'mm')) +
#'   geom_point()
#'
#' # The connector is capped a bit before it reaches the mark, but this can be
#' # controlled
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_rect(aes(fill = Species, label = Species),
#'                  con.cap = 0) +
#'   geom_point()
#'
#' # If you want to use the scaled colours for the labels or connectors you can
#' # use the "inherit" keyword instead
#' ggplot(iris, aes(Petal.Length, Petal.Width)) +
#'   geom_mark_rect(aes(fill = Species, label = Species),
#'                  label.fill = "inherit") +
#'   geom_point()
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomMarkRect <- ggproto('GeomMarkRect', GeomMarkCircle,
  setup_data = function(self, data, params) {
    if (!is.null(data$filter)) {
      data$filter <- ifelse(is.na(data$filter), FALSE, data$filter)
      self$removed <- data[!data$filter, c('x', 'y', 'PANEL')]
      data <- data[data$filter, ]
    }
    if (nrow(data) == 0) return(data)
    vec_rbind(!!!lapply(split(data, list(data$PANEL, data$group)), function(d) {
      if (nrow(d) == 1) return(d)
      x_range <- range(d$x, na.rm = TRUE)
      y_range <- range(d$y, na.rm = TRUE)
      d_new <- data_frame0(
        x = x_range[c(1, 1, 2, 2)],
        y = y_range[c(1, 2, 2, 1)]
      )
      d$x <- NULL
      d$y <- NULL
      unique0(cbind(d_new, d[rep(1, 4), ]))
    }))
  },
  draw_panel = function(self, data, panel_params, coord, expand = unit(5, 'mm'),
                        radius = unit(2.5, 'mm'),
                        label.maxwidth = unit(50, 'mm'),
                        label.buffer = unit(10, 'mm'),
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

    gp <- gpar(
      col = first_rows$colour,
      fill = ggplot2::fill_alpha(first_rows$fill, first_rows$alpha),
      lwd = (first_rows$linewidth %||% first_rows$size) * .pt,
      lty = first_rows$linetype,
      fontsize = (first_rows$size %||% 4.217518) * .pt
    )

    rectEncGrob(coords$x, coords$y,
      default.units = 'native',
      id = coords$group, expand = expand, radius = radius,
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
  }
)
#' @rdname geom_mark_rect
#' @export
geom_mark_rect <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', expand = unit(5, 'mm'),
                           radius = unit(2.5, 'mm'),
                           label.buffer = unit(10, 'mm'),
                           label.maxwidth = unit(50, 'mm'), con.colour = 'black',
                           con.size = 0.5, con.type = 'elbow', con.linetype = 1,
                           con.border = 'one', con.cap = unit(3, 'mm'),
                           con.arrow = NULL, ..., na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE,
                           label.margin = deprecated(),
                           label.width = deprecated(),
                           label.minwidth = deprecated(),
                           label.hjust = deprecated(),
                           label.fontsize = deprecated(),
                           label.family = deprecated(),
                           label.lineheight = deprecated(),
                           label.fontface = deprecated(),
                           label.fill = deprecated(),
                           label.colour = deprecated()) {
  if (lifecycle::is_present(label.margin)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.margin)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.width)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.width)", I("the `width` aesthetic instead"))
  }
  if (lifecycle::is_present(label.minwidth)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.minwidth)")
  }
  if (lifecycle::is_present(label.hjust)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.hjust)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.fontsize)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.fontsize)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.family)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.family)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.lineheight)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.lineheight)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.fontface)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.fontface)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.fill)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.fill)", I("the `style` aesthetic instead"))
  }
  if (lifecycle::is_present(label.colour)) {
    lifecycle::deprecate_soft("0.6.0", "geom_mark_rect(label.colour)", I("the `style` aesthetic instead"))
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMarkRect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      expand = expand,
      radius = radius,
      label.buffer = label.buffer,
      label.maxwidth = label.maxwidth,
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

#' @importFrom grid gTree
rectEncGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                        id.lengths = NULL, expand = 0, radius = 0, label = NULL,
                        ghosts = NULL, default.units = 'npc', name = NULL,
                        mark.gp = gpar(), con.gp = gpar(),
                        label.buffer = unit(10, 'mm'),
                        label.maxwidth = unit(50, 'mm'), con.type = 'elbow',
                        con.border = 'one', con.cap = unit(3, 'mm'),
                        con.arrow = NULL, anchor.x = NULL, anchor.y = NULL,
                        vp = NULL) {
  mark <- shapeGrob(
    x = x, y = y, id = id, id.lengths = id.lengths,
    expand = expand, radius = radius,
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
    mark = mark, label = label, labeldim = labeldim,
    buffer = label.buffer, ghosts = ghosts, con.gp = con.gp, con.type = con.type,
    con.cap = as_mm(con.cap, default.units), con.border = con.border,
    con.arrow = con.arrow, anchor.x = anchor.x, anchor.y = anchor.y, name = name,
    vp = vp, cl = 'rect_enc'
  )
}
#' @importFrom grid makeContent setChildren gList
#' @export
makeContent.rect_enc <- function(x) {
  mark <- x$mark
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
      con_gp = x$con.gp, anchor_mod = 3, anchor_x = anchor_x,
      anchor_y = anchor_y, arrow = x$con.arrow
    )
    setChildren(x, inject(gList(!!!c(list(mark), labels))))
  } else {
    setChildren(x, gList(mark))
  }
}
