facet_table <- function(row = .ROW, col = .COL, x.scale = 1, y.scale = 1,
                        heights = 1, widths = 1, default.unit = 'null',
                        x.scale.def = NA, y.scale.def = NA, top.strip = NA,
                        bottom.strip = NA, left.strip = NA, right.strip = NA,
                        axis.suppress.top = FALSE, axis.suppress.bottom = FALSE,
                        axis.suppress.left = FALSE, axis.suppress.right = FALSE,
                        shrink = TRUE) {

    ggproto(NULL, FacetTable,
        shrink = shrink,
        params = list(
            row = lazy(row), col = lazy(col), x.scale = x.scale, y.scale = y.scale,
            heights = heights, widths = widths, default.unit = default.unit,
            x.scale.def = x.scale.def, y.scale.def = y.scale.def, top.strip = top.strip,
            bottom.strip = bottom.strip, left.strip = left.strip, right.strip = right.strip,
            axis.suppress.top = axis.suppress.top, axis.suppress.bottom = axis.suppress.bottom,
            axis.suppress.left = axis.suppress.left, axis.suppress.right = axis.suppress.right
        )
    )
}

FacetTable <- ggproto('FacetTable', Facet,
    compute_layout = function(data, params) {
        places <- do.call(rbind,lapply(data, function(d) {
            data.frame(
                ROW = as.integer(lazy_eval(params$row, d)),
                COL = as.integer(lazy_eval(params$col, d))
            )
        }))
        panels <- unique(na.omit(places))
        panels <- panels[order(panels$ROW, panels$COL),]
        panels$PANEL <- seq_len(nrow(panels))
        panels
    },
    map_data = function(data, layout, params) {
        if (plyr::empty(data)) {
            return(cbind(data, PANEL = integer(0)))
        }
        row <- as.integer(lazy_eval(params$row, data))
        col <- as.integer(lazy_eval(params$col, data))
        single_panels <- !is.na(row) & !is.na(col)
        data$PANEL <- NA_integer_
        data$PANEL[single_panels, ] <- layout$PANEL[match(paste0(row, '-', col)[single_panels], paste0(layout$ROW, '-', layout$COL))]
        if (any(!single_panels)) {

        }
        data
    }
)
expand_to <- function(x, row, col) {
    x <- as.matrix(x)
    x[rep(seq_len(nrow(x)), length.out = row), rep(seq_len(ncol(x)), length.out = col)]
}
