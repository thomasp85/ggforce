#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom grid segmentsGrob polylineGrob gpar
GeomPathInterpolate <- ggproto('GeomPathInterpolate', GeomPath,
  draw_panel = function(self, data, panel_scales, coord, arrow = NULL,
                        lineend = 'butt', linejoin = 'round', linemitre = 1,
                        na.rm = FALSE) {
    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(
        "{.fn {snake_class(self)}}: Each group consists of only one observation.",
        i = "Do you need to adjust the {.field group} aesthetic?"
      ))
    }
    data <- data[order(data$group), , drop = FALSE]
    data <- interpolateDataFrame(data)
    munched <- coord_munch(coord, data, panel_scales)
    rows <- stats::ave(seq_len(nrow(munched)), munched$group,
      FUN = length
    )
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) {
      return(zeroGrob())
    }
    attr <- dapply(data, 'group', function(df) {
      data_frame0(
        solid = identical(unique0(df$linetype), 1),
        constant = nrow(unique0(df[, names(df) %in% c(
          'alpha', 'colour',
          'linewidth', 'size', 'linetype'
        )])) == 1
      )
    })

    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      cli::cli_abort("{.fn {snake_class(self)}} can't have varying {.field colour}, {.field linewidth}, and/or {.field alpha} along the line when {.field linetype} isn't solid")
    }
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <- c(group_diff, TRUE)
    if (!constant) {
      segmentsGrob(munched$x[!end], munched$y[!end], munched$x[!start],
        munched$y[!start],
        default.units = 'native', arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[!end],
          fill = alpha(munched$colour, munched$alpha)[!end],
          lwd = (munched$linewidth[!end] %||% munched$size[!end]) * .pt,
          lty = munched$linetype[!end],
          lineend = lineend, linejoin = linejoin, linemitre = linemitre
        )
      )
    }
    else {
      id <- match(munched$group, unique0(munched$group))
      polylineGrob(munched$x, munched$y,
        id = id, default.units = 'native',
        arrow = arrow, gp = gpar(
          col = alpha(munched$colour, munched$alpha)[start],
          fill = alpha(munched$colour, munched$alpha)[start],
          lwd = (munched$linewidth[start] %||% munched$size[start]) * .pt,
          lty = munched$linetype[start], lineend = lineend,
          linejoin = linejoin, linemitre = linemitre
        )
      )
    }
  },
  handle_na = function(data, params) {
    data
  }
)
#' Interpolate layer data
#'
#' @param data A data.frame with data for a layer
#'
#' @return A similar data.frame with NA values interpolated
#'
#' @importFrom tweenr tween_t
#' @keywords internal
#' @export
interpolateDataFrame <- function(data) {
  if (is.null(data$group)) {
    cli::cli_abort('data must have a group column')
  }
  interpLengths <- lengths(split(data$group, data$group))
  for (i in seq_len(ncol(data))) {
    if (names(data)[i] %in% c('x', 'y', 'index', 'group', '.interp') ||
        all(is.na(data[[i]]))) {
      next
    }
    if (length(unique0(data[[i]][data$.interp])) > 1) {
      next
    }
    interpValues <- split(data[[i]][!data$.interp], data$group[!data$.interp])
    data[[i]] <- unlist(tween_t(interpValues, interpLengths))
  }
  data[, names(data) != '.interp']
}
