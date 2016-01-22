#' @importFrom ggplot2 ggproto GeomPath coord_munch zeroGrob alpha .pt
#' @importFrom grid segmentsGrob polylineGrob gpar
#' @importFrom dplyr %>% group_by_ do ungroup
GeomPathInterpolate <- ggproto('GeomPathInterpolate', GeomPath,
    draw_panel = function(data, panel_scales, coord, arrow = NULL,
                          lineend = "butt", linejoin = "round", linemitre = 1,
                          na.rm = FALSE) {
        if (!anyDuplicated(data$group)) {
            message("geom_path_interpolate: Each group consists of only one observation. ",
                         "Do you need to adjust the group aesthetic?")
        }
        data <- data[order(data$group), , drop = FALSE]
        data <- interpolateDataFrame(data)
        munched <- coord_munch(coord, data, panel_scales)
        rows <- stats::ave(seq_len(nrow(munched)), munched$group,
                           FUN = length)
        munched <- munched[rows >= 2, ]
        if (nrow(munched) < 2)
            return(zeroGrob())
        attr <- data %>% group_by_(~group) %>%
            do({
                data.frame(solid = identical(unique(.$linetype), 1),
                           constant = nrow(unique(.[, c("alpha", "colour",
                                                        "size", "linetype")])) == 1)
            }) %>%
            ungroup()

        solid_lines <- all(attr$solid)
        constant <- all(attr$constant)
        if (!solid_lines && !constant) {
            stop("geom_path_interpolate: If you are using dotted or dashed lines",
                 ", colour, size and linetype must be constant over the line",
                 call. = FALSE)
        }
        n <- nrow(munched)
        group_diff <- munched$group[-1] != munched$group[-n]
        start <- c(TRUE, group_diff)
        end <- c(group_diff, TRUE)
        if (!constant) {
            segmentsGrob(munched$x[!end], munched$y[!end], munched$x[!start],
                         munched$y[!start], default.units = "native", arrow = arrow,
                         gp = gpar(col = alpha(munched$colour, munched$alpha)[!end],
                                   fill = alpha(munched$colour, munched$alpha)[!end],
                                   lwd = munched$size[!end] * .pt, lty = munched$linetype[!end],
                                   lineend = lineend, linejoin = linejoin, linemitre = linemitre))
        }
        else {
            id <- match(munched$group, unique(munched$group))
            polylineGrob(munched$x, munched$y, id = id, default.units = "native",
                         arrow = arrow, gp = gpar(col = alpha(munched$colour,
                                                              munched$alpha)[start], fill = alpha(munched$colour,
                                                                                                  munched$alpha)[start], lwd = munched$size[start] *
                                                      .pt, lty = munched$linetype[start], lineend = lineend,
                                                  linejoin = linejoin, linemitre = linemitre))
        }
    },
    handle_na = function(data, params) {
        data
    }
)
#' @importFrom dplyr %>% group_by_ do ungroup
#' @export
interpolateDataFrame <- function(data) {
    if (is.null(data$group)) {
        stop('data must have a group column')
    }
    if (!any(is.na(data))) {
        return(data)
    }
    fixedCols <- names(data) %in% c('x', 'y', 'index') | apply(is.na(data), 2, all)
    interpCol <- data[!data$.interp, !fixedCols]
    colTypes <- guessTypes(interpCol)
    constantCols <- lengths(lapply(as.list(interpCol), unique)) == 1
    cols <- names(interpCol)[names(interpCol) != '.interp']
    interpCol <- data[, !fixedCols]

    interpolated <- interpCol %>% group_by_(~group) %>%
        do({
            n <- nrow(.)
            interpolated <- lapply(cols, function(i) {
                if (length(unique(.[[i]][.$.interp])) != 1) return(.[[i]])
                base <- which(!.$.interp)
                column <- if (constantCols[i] || length(unique(.[[i]][base])) != 1) {
                    rep(.[base[1], i], n)
                } else {
                    switch(
                        colTypes[i],
                        numeric = interp_numeric(.[[i]][base], n),
                        num_character = interp_num_character(.[[i]][base], n),
                        colour = interp_colour(.[[i]][base], n),
                        character = interp_unknown(.[[i]][base], n),
                        date = interp_date(.[[i]][base], n),
                        datetime = interp_datetime(.[[i]][base], n),
                        interp_unknown(.[[i]][base], n)
                    )
                }
                unlist(column)
            })
            interpolated <- as.data.frame(interpolated, stringsAsFactors = FALSE)
            names(interpolated) <- cols
            interpolated
        }) %>%
        ungroup()

    cbind(data[, fixedCols], interpolated)
}
guessTypes <- function(data) {
    unlist(lapply(as.list(data), function(column) {
        if (is.numeric(column)) {
            'numeric'
        } else if (is.character(column)) {
            if (!any(is.na(suppressWarnings(as.numeric(column))))) {
                'num_character'
            } else {
                tryCatch(
                    {
                        suppressWarnings(col2rgb(column))
                        'colour'
                    },
                    error = function(e) {
                        'character'
                    }
                )
            }
        } else if (inherits(column, 'Date')) {
            'date'
        } else if (inherits(column, 'POSIXt')) {
            'datetime'
        } else {
            'unknown'
        }
    }))
}
interp_numeric <- function(x, n) {
    approx(x, n = n)$y
}
interp_num_character <- function(x, n) {
    as.character(approx(as.numeric(x), n = n)$y)
}
interp_colour <- function(x, n) {
    colorRampPalette(x)(n)
}
interp_date <- function(x, n) {
    as.Date(approx(as.numeric(x), n = n)$y, origin = '1970-01-01')
}
interp_datetime <- function(x, n) {
    switch(
        class(x)[1],
        POSIXct = as.POSIXct(approx(as.numeric(x), n = n)$y, origin = '1970-01-01 01:00:00 CET'),
        POSIXlt = as.POSIXlt(approx(as.numeric(x), n = n)$y, origin = '1970-01-01 01:00:00 CET')
    )
}
interp_unknown <- function(x, n) {
    each <- round(n/length(x))
    rep(x, each = each)[seq.int(each/2, length.out = n)]
}
