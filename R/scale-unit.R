#' @export
#' @importFrom scales censor
#' @importFrom units make_unit
scale_x_cunit <- function(name = waiver(), breaks = waiver(), unit = NULL,
                          minor_breaks = waiver(), labels = waiver(),
                          limits = NULL, expand = waiver(), oob = censor,
                          na.value = NA_real_, trans = "identity",
                          position = "bottom", sec.axis = waiver()) {
    sc <- continuous_scale(
        c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
        "position_c", identity, name = name, breaks = breaks,
        minor_breaks = minor_breaks, labels = labels, limits = limits,
        expand = expand, oob = oob, na.value = na.value, trans = trans,
        guide = "none", position = position, super = ScaleContinuousPositionUnit
    )
    sc$unit <- switch(
        class(unit),
        symbolic_units = ,
        'NULL' = unit,
        character = make_unit(unit),
        units = units(unit),
        stop('unit must either be NULL or of class `units` or `symbolic_units`', call. = FALSE)
    )
    if (!inherits(sec.axis, 'waiver')) {
        if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
        if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
        sc$secondary.axis <- sec.axis
    }
    sc
}
#' @export
#' @importFrom scales censor
#' @importFrom units make_unit
scale_y_cunit <- function(name = waiver(), breaks = waiver(), unit = NULL,
                          minor_breaks = waiver(), labels = waiver(),
                          limits = NULL, expand = waiver(), oob = censor,
                          na.value = NA_real_, trans = "identity",
                          position = "left", sec.axis = waiver()) {
    sc <- continuous_scale(
        c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper"),
        "position_c", identity, name = name, breaks = breaks,
        minor_breaks = minor_breaks, labels = labels, limits = limits,
        expand = expand, oob = oob, na.value = na.value, trans = trans,
        guide = "none", position = position, super = ScaleContinuousPositionUnit
    )
    sc$unit <- switch(
        class(unit),
        symbolic_units = ,
        'NULL' = unit,
        character = make_unit(unit),
        units = units(unit),
        stop('unit must either be NULL or of class `units` or `symbolic_units`', call. = FALSE)
    )
    if (!inherits(sec.axis, 'waiver')) {
        if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
        if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
        sc$secondary.axis <- sec.axis
    }
    sc
}
#' @importFrom units as.units make_unit_label
#' @export
ScaleContinuousPositionUnit <- ggproto('ScaleContinuousPositionUnit', ScaleContinuousPosition,
    unit = NULL,

    train = function(self, x) {
        if (length(x) == 0) return()
        if (!is.null(self$unit)) {
            units(x) <- as.units(1, self$unit)
        }
        self$range$train(x)
    },
    map = function(self, x, limits = self$get_limits()) {
        if (inherits(x, 'units')) {
            if (is.null(self$unit)) {
                self$unit <- units(x)
            } else {
                units(x) <- as.units(1, self$unit)
            }
            x <- as.numeric(x)
        }
        ggproto_parent(ScaleContinuousPosition, self)$map(x, limits)
    },
    make_title = function(self, title) {
        make_unit_label(title, as.units(1, self$unit))
    }
)
#' @export
scale_type.units <- function(x) c('cunit', 'continuous')
