#' Create a stereogram plot
#'
#' This, arguably pretty useless function, lets you create plots with a sense of
#' depth by creating two slightly different versions of the plot that
#' corresponds to how the eyes would see it if the plot was 3 dimensional. To
#' experience the effect look at the plots through 3D hardware such as Google
#' Cardboard or by relaxing the eyes and focusing into the distance. The
#' depth of a point is calculated for layers having a depth aesthetic supplied.
#' The scaling of the depth can be controlled with \code{\link{scale_depth}} as
#' you would control any aesthetic. Negative values will result in features
#' placed behind the paper plane, while positive values will result in
#' features hovering in front of the paper. While features within each layer is
#' sorted so those closest to you are plotted on top of those more distant, this
#' cannot be done between layers. Thus, layers are always plotted on top of
#' each others, even if the features in one layer lies behind features in a
#' layer behind it. The depth experience is inaccurate and should not be used
#' for conveying important data. Regard this more as a party-trick...
#'
#' @param IPD The interpupillary distance (in mm) used for calculating point
#' displacement. The default value is an average of both genders
#'
#' @param panel.size The final plot size in mm. As IPD this is used to calculate
#' point displacement. Don't take this value too literal but experiment until
#' you get a nice effect. Lower values gives higher displacement and thus
#' require the plots to be observed from a closer distance
#'
#' @inheritParams ggplot2::facet_wrap
#'
#' @family ggforce facets
#'
#' @importFrom ggplot2 ggproto
#' @export
#'
#' @examples
#' ggplot(mtcars) +
#'   geom_point(aes(mpg, disp, depth = cyl)) +
#'   facet_stereo()
#'
facet_stereo <- function(IPD = 63.5, panel.size = 200, shrink = TRUE) {
    ggproto(NULL, FacetStereo,
            shrink = shrink,
            params = list(
                IPD = IPD, panel.size = panel.size
            )
    )
}

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Facet transform_position render_axes
#' @importFrom scales rescale
#' @importFrom gtable gtable_add_cols
#' @export
FacetStereo <- ggproto("FacetStereo", Facet,
    compute_layout = function(data, params) {
        data.frame(PANEL = c(1L, 2L), SCALE_X = 1L, SCALE_Y = 1L)
    },
    map_data = function(data, layout, params) {
        if (empty(data)) {
            return(cbind(data, PANEL = integer(0)))
        }
        rbind(
            cbind(data, PANEL = 1L),
            cbind(data, PANEL = 2L)
        )
    },
    finish_data = function(data, layout, x_scales, y_scales, params) {
        if ('depth' %in% names(data)) {
            if ('.interp' %in% names(data)) {
                data$depth2 <- do.call(rbind, lapply(split(data, data$PANEL), interpolateDataFrame))$depth
            } else {
                data$depth2 <- data$depth
            }
            group_order <- order(sapply(split(data$depth2, data$group), quantile, probs = 0.9, na.rm = TRUE))
            data <- do.call(rbind, split(data, data$group)[group_order])
            data[data$group == -1, ] <- data[data$group == -1, ][order(data$depth2[data$group == -1]), ]
            data$group[data$group != -1] <- match(data$group[data$group != -1], unique(data$group[data$group != -1]))
            x_range <- x_scales[[1]]$dimension(expand_default(x_scales[[1]]))
            k <- ifelse(data$PANEL == 1, -1, 1) * params$IPD/2
            x_transform <- function(d) {
                h <- rescale(d, to = c(-1, 1) * params$panel.size/2, from = x_range)
                new_pos <- h + (h - k) * data$depth2
                rescale(new_pos, to = x_range, from = c(-1, 1) * params$panel.size/2)
            }
            data <- transform_position(data, x_transform)
            data$depth2 <- NULL
        }
        data
    },
    draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                           data, theme, params) {
        axes <- render_axes(ranges, ranges, coord, theme, FALSE)
        panelGrobs <- create_panels(panels, axes$x, axes$y)
        spacing <- theme$panel.spacing.x %||% theme$panel.spacing
        panel <- gtable_add_cols(panelGrobs[[1]], spacing)
        cbind(panel, panelGrobs[[2]], size = 'first')
    },
    draw_labels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, labels, params) {
        panel_dim <-  find_panel(panels)

        xlab_height_top <- grobHeight(labels$x[[1]])
        panels <- gtable_add_rows(panels, xlab_height_top, pos = 0)
        panels <- gtable_add_grob(panels, labels$x[[1]], name = "xlab-t",
                                  l = panel_dim$l, r = panel_dim$r, t = 1, clip = "off")

        xlab_height_bottom <- grobHeight(labels$x[[2]])
        panels <- gtable_add_rows(panels, xlab_height_bottom, pos = -1)
        panels <- gtable_add_grob(panels, labels$x[[2]], name = "xlab-b",
                                  l = panel_dim$l, r = panel_dim$r, t = -1, clip = "off")

        panel_dim <-  find_panel(panels)

        ylab_width_left <- grobWidth(labels$y[[1]])
        panels <- gtable_add_cols(panels, ylab_width_left, pos = 0)
        panels <- gtable_add_grob(panels, labels$y[[1]], name = "ylab-l",
                                  l = 1, b = panel_dim$b, t = panel_dim$t, clip = "off")

        ylab_width_right <- grobWidth(labels$y[[2]])
        panels <- gtable_add_cols(panels, ylab_width_right, pos = -1)
        panels <- gtable_add_grob(panels, labels$y[[2]], name = "ylab-r",
                                  l = -1, b = panel_dim$b, t = panel_dim$t, clip = "off")

        panels
    }
)
