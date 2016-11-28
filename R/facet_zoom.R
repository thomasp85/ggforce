#' Facet data for zoom with context
#'
#' This facetting provides the means to zoom in on a subset of the data, while
#' keeping the view of the full dataset as a separate panel. The zoomed-in area
#' will be indicated on the full dataset panel for reference. It is possible to
#' zoom in on both the x and y axis at the same time. If this is done it is
#' possible to both get each zoom separately and combined or just combined.
#'
#' @param x,y,xy An expression evaluating to a logical vector that determines
#' the subset of data to zoom in on
#'
#' @param split If both \code{x} and \code{y} is given, should each axis zoom
#' be shown separately as well? Defaults to \code{FALSE}
#'
#' @param horizontal If both \code{x} and \code{y} is given and
#' \code{split = FALSE} How should the zoom panel be positioned relative to the
#' full data panel? Defaults to \code{TRUE}
#'
#' @param zoom.size Sets the relative size of the zoom panel to the full data
#' panel. The default (\code{2}) makes the zoom panel twice the size of the full
#' data panel.
#'
#' @param show.area Should the zoom area be drawn below the data points on the
#' full data panel? Defaults to \code{TRUE}.
#'
#' @inheritParams ggplot2::facet_wrap
#'
#' @family ggforce facets
#'
#' @importFrom lazyeval lazy
#' @export
#'
#' @examples
#' # Zoom in on the versicolor species on the x-axis
#' ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
#'     geom_point() +
#'     facet_zoom(x = Species == "versicolor")
#'
#' # Zoom in on versicolor on both axes
#' ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
#'     geom_point() +
#'     facet_zoom(xy = Species == "versicolor")
#'
#' # Use different zoom criteria on each axis
#' ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
#'     geom_point() +
#'     facet_zoom(x = Species != 'setosa', y = Species == 'versicolor')
#'
#' # Get each axis zoom separately as well
#' ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
#'     geom_point() +
#'     facet_zoom(xy = Species == "versicolor", split = TRUE)
#'
facet_zoom <- function(x, y, xy, split = FALSE, horizontal = TRUE, zoom.size = 2, show.area = TRUE, shrink = TRUE) {
    x <- if (missing(x)) if (missing(xy)) NULL else lazy(xy) else lazy(x)
    y <- if (missing(y)) if (missing(xy)) NULL else lazy(xy) else lazy(y)
    if (is.null(x) && is.null(y)) {
        stop("Either x- or y-zoom must be given", call. = FALSE)
    }
    ggproto(NULL, FacetZoom,
        shrink = shrink,
        params = list(
            x = x, y = y, split = split, zoom.size = zoom.size, show.area = show.area,
            horizontal = horizontal
        )
    )
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid convertUnit unit unit.c polygonGrob segmentsGrob gpar grobTree rectGrob
#' @importFrom gtable gtable_add_cols gtable_add_rows gtable_add_grob
#' @importFrom scales rescale
#' @importFrom lazyeval lazy_eval
#' @export
FacetZoom <- ggproto("FacetDuplicate", Facet,
    compute_layout = function(data, params) {
        layout <- rbind(
            data.frame(name = 'orig', SCALE_X = 1L, SCALE_Y = 1L),
            data.frame(name = 'x', SCALE_X = 2L, SCALE_Y = 1L),
            data.frame(name = 'y', SCALE_X = 1L, SCALE_Y = 2L),
            data.frame(name = 'full', SCALE_X = 2L, SCALE_Y = 2L)
        )
        if (is.null(params$y)) {
            layout <- layout[c(1,2),]
        } else if (is.null(params$x)) {
            layout <- layout[c(1,3),]
        }
        layout$PANEL <- seq_len(nrow(layout))
        layout
    },
    map_data = function(data, layout, params) {
        if (plyr::empty(data)) {
            return(cbind(data, PANEL = integer(0)))
        }
        rbind(
            cbind(data, PANEL = 1L),
            if ('x' %in% layout$name) {
                index_x <- tryCatch(lazy_eval(params$x, data), error = function(e) FALSE)
                if (sum(index_x) != 0) {
                    cbind(data[index_x, ], PANEL = layout$PANEL[layout$name == "x"])
                }
            },
            if ('y' %in% layout$name) {
                index_y <- tryCatch(lazy_eval(params$y, data), error = function(e) FALSE)
                if (sum(index_y) != 0) {
                    cbind(data[index_y, ], PANEL = layout$PANEL[layout$name == "y"])
                }
            }
        )
    },
    finish_data = function(data, layout, x_scales, y_scales, params) {
        data <- do.call(rbind, lapply(unique(layout$PANEL), function(panel) {
            d <- data[data$PANEL == 1, ]
            d$PANEL <- panel
            d
        }))
        data
    },
    draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                           data, theme, params) {
        if (is.null(params$x)) {
            params$horizontal <- TRUE
        } else if (is.null(params$y)) {
            params$horizontal <- FALSE
        }
        if (is.null(theme[['zoom']])) {
            theme$zoom <- theme$strip.background
        }
        if (is.null(theme$zoom.x)) {
            theme$zoom.x <- theme$zoom
        }
        if (is.null(theme$zoom.y)) {
            theme$zoom.y <- theme$zoom
        }
        # Construct the panels
        axes <- render_axes(ranges, ranges, coord, theme, FALSE)
        panelGrobs <- create_panels(panels, axes$x, axes$y)

        if ('full' %in% layout$name && !params$split) {
            panelGrobs <- panelGrobs[c(1, 4)]
        }

        if ('y' %in% layout$name) {
            if (!inherits(theme$zoom.y, 'element_blank')) {
                zoom_prop <- rescale(y_scales[[2]]$dimension(expansion(y_scales[[2]])),
                                     from = y_scales[[1]]$dimension(expansion(y_scales[[1]])))
                indicator <- polygonGrob(c(1, 1, 0, 0), c(zoom_prop, 1, 0), gp = gpar(col = NA, fill = alpha(theme$zoom.y$fill, 0.5)))
                lines <- segmentsGrob(y0 = c(0, 1), x0 = c(0, 0), y1 = zoom_prop, x1 = c(1, 1), gp = gpar(col = theme$zoom.y$colour,
                                                                                                          lty = theme$zoom.y$linetype,
                                                                                                          lwd = theme$zoom.y$size,
                                                                                                          lineend = 'round'))
                indicator_h <- grobTree(indicator, lines)
            } else {
                indicator_h <- zeroGrob()
            }
        }
        if ('x' %in% layout$name) {
            if (!inherits(theme$zoom.x, 'element_blank')) {
                zoom_prop <- rescale(x_scales[[2]]$dimension(expansion(x_scales[[2]])),
                                     from = x_scales[[1]]$dimension(expansion(x_scales[[1]])))
                indicator <- polygonGrob(c(zoom_prop, 1, 0), c(1, 1, 0, 0), gp = gpar(col = NA, fill = alpha(theme$zoom.x$fill, 0.5)))
                lines <- segmentsGrob(x0 = c(0, 1), y0 = c(0, 0), x1 = zoom_prop, y1 = c(1, 1), gp = gpar(col = theme$zoom.x$colour,
                                                                                                          lty = theme$zoom.x$linetype,
                                                                                                          lwd = theme$zoom.x$size,
                                                                                                          lineend = 'round'))
                indicator_v <- grobTree(indicator, lines)
            } else {
                indicator_v <- zeroGrob()
            }
        }

        if ('full' %in% layout$name && params$split) {
            space.x <- theme$panel.spacing.x
            if (is.null(space.x)) space.x <- theme$panel.spacing
            space.x <- unit(5 * as.numeric(convertUnit(space.x, 'cm')), 'cm')
            space.y <- theme$panel.spacing.y
            if (is.null(space.y)) space.y <- theme$panel.spacing
            space.y <- unit(5 * as.numeric(convertUnit(space.y, 'cm')), 'cm')
            final <- gtable_add_cols(panelGrobs[[3]], space.x)
            final <- cbind(final, panelGrobs[[1]], size = 'first')
            final_tmp <- gtable_add_cols(panelGrobs[[4]], space.x)
            final_tmp <- cbind(final_tmp, panelGrobs[[2]], size = 'first')
            final <- gtable_add_rows(final, space.y)
            final <- rbind(final, final_tmp, size = 'first')
            final <- gtable_add_grob(final, list(indicator_h, indicator_h), c(2, 6), 3, c(2, 6), 5, z = -Inf, name = "zoom-indicator")
            final <- gtable_add_grob(final, list(indicator_v, indicator_v), 3, c(2, 6), 5, z = -Inf, name = "zoom-indicator")
            heights <- unit.c(
                unit(max_height(list(axes$x[[1]]$top, axes$x[[3]]$top)), 'cm'),
                unit(1, 'null'),
                unit(max_height(list(axes$x[[1]]$bottom, axes$x[[3]]$bottom)), 'cm'),
                space.y,
                unit(max_height(list(axes$x[[2]]$top, axes$x[[4]]$top)), 'cm'),
                unit(params$zoom.size, 'null'),
                unit(max_height(list(axes$x[[2]]$bottom, axes$x[[4]]$bottom)), 'cm')
            )
            widths <- unit.c(
                unit(max_width(list(axes$y[[3]]$left, axes$y[[4]]$left)), 'cm'),
                unit(params$zoom.size, 'null'),
                unit(max_height(list(axes$y[[3]]$right, axes$y[[4]]$right)), 'cm'),
                space.x,
                unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
                unit(1, 'null'),
                unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')
            )
            final$heights <- heights
            final$widths <- widths
        } else {
            if (params$horizontal) {
                space <- theme$panel.spacing.x
                if (is.null(space)) space <- theme$panel.spacing
                space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
                heights <- unit.c(
                    unit(max_height(list(axes$x[[1]]$top, axes$x[[2]]$top)), 'cm'),
                    unit(1, 'null'),
                    unit(max_height(list(axes$x[[1]]$bottom, axes$x[[2]]$bottom)), 'cm')
                )
                final <- gtable_add_cols(panelGrobs[[2]], space)
                final <- cbind(final, panelGrobs[[1]], size = 'first')
                final$heights <- heights
                final$widths[panel_cols(final)$l] <- unit(c(params$zoom.size, 1), 'null')
                final <- gtable_add_grob(final, indicator_h, 2, 3, 2, 5, z = -Inf, name = "zoom-indicator")
            } else {
                space <- theme$panel.spacing.y
                if (is.null(space)) space <- theme$panel.spacing
                space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
                widths <- unit.c(
                    unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
                    unit(1, 'null'),
                    unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')
                )
                final <- gtable_add_rows(panelGrobs[[1]], space)
                final <- rbind(final, panelGrobs[[2]], size = 'first')
                final$widths <- widths
                final$heights[panel_rows(final)$t] <- unit(c(1, params$zoom.size), 'null')
                final <- gtable_add_grob(final, indicator_v, 3, 2, 5, z = -Inf, name = "zoom-indicator")
            }
        }
        final
    },
    draw_back = function(data, layout, x_scales, y_scales, theme, params) {
        if (is.null(theme[['zoom']])) {
            theme$zoom <- theme$strip.background
        }
        if (is.null(theme$zoom.x)) {
            theme$zoom.x <- theme$zoom
        }
        if (is.null(theme$zoom.y)) {
            theme$zoom.y <- theme$zoom
        }
        if (!is.null(params$x) && params$show.area && !inherits(theme$zoom.x, 'element_blank')) {
            zoom_prop <- rescale(x_scales[[2]]$dimension(expansion(x_scales[[2]])),
                                 from = x_scales[[1]]$dimension(expansion(x_scales[[1]])))
            x_back <- grobTree(
                rectGrob(x = mean(zoom_prop), y = 0.5, width = diff(zoom_prop), height = 1, gp = gpar(col = NA, fill = alpha(theme$zoom.x$fill, 0.5))),
                segmentsGrob(zoom_prop, c(0, 0), zoom_prop, c(1, 1), gp = gpar(col = theme$zoom.x$colour,
                                                                               lty = theme$zoom.x$linetype,
                                                                               lwd = theme$zoom.x$size,
                                                                               lineend = 'round'))
            )
        } else {
            x_back <- zeroGrob()
        }
        if (!is.null(params$y) && params$show.area && !inherits(theme$zoom.y, 'element_blank')) {
            zoom_prop <- rescale(y_scales[[2]]$dimension(expansion(y_scales[[2]])),
                                 from = y_scales[[1]]$dimension(expansion(y_scales[[1]])))
            y_back <- grobTree(
                rectGrob(y = mean(zoom_prop), x = 0.5, height = diff(zoom_prop), width = 1, gp = gpar(col = NA, fill = alpha(theme$zoom.y$fill, 0.5))),
                segmentsGrob(y0 = zoom_prop, x0 = c(0, 0), y1 = zoom_prop, x1 = c(1, 1), gp = gpar(col = theme$zoom.y$colour,
                                                                                                   lty = theme$zoom.y$linetype,
                                                                                                   lwd = theme$zoom.y$size,
                                                                                                   lineend = 'round'))
            )
        } else {
            y_back <- zeroGrob()
        }
        if ('full' %in% layout$name && params$split) {
            list(grobTree(x_back, y_back), y_back, x_back, zeroGrob())
        } else {
            list(grobTree(x_back, y_back), zeroGrob())
        }
    }
)
#' @importFrom grid grobHeight grobWidth unit unit.c
#' @importFrom gtable gtable gtable_add_grob
create_panels <- function(panels, x.axis, y.axis) {
    Map(function(panel, x, y) {
        heights <- unit.c(grobHeight(x$top), unit(1, 'null'), grobHeight(x$bottom))
        widths <- unit.c(grobWidth(y$left), unit(1, 'null'), grobWidth(y$right))
        table <- gtable(widths, heights)
        table <- gtable_add_grob(table, panel, t = 2, l = 2, z = 10, clip = 'on', name = 'panel')
        table <- gtable_add_grob(table, x, t = c(1, 3), l = 2, z = 20, clip = 'off', name = c('axis-t', 'axis-b'))
        table <- gtable_add_grob(table, y, t = 2, l = c(1,3), z = 20, clip = 'off', name = c('axis-l', 'axis-r'))
    }, panel = panels, x = x.axis, y = y.axis)
}

expansion <- function (scale, discrete = c(0, 0.6), continuous = c(0.05, 0)) {
    if (inherits(scale$expand, 'waiver')) {
        if (scale$is_discrete())
            discrete
        else continuous
    } else scale$expand
}
