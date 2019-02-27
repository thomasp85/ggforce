#' Position scales for units data
#'
#' These are the default scales for the units class. These will
#' usually be added automatically. To override manually, use
#' `scale_*_unit`.
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_x_continuous
#' @param unit A unit specification to use for the axis. If given, the values
#' will be converted to this unit before plotting. An error will be thrown if
#' the specified unit is incompatible with the unit of the data.
#'
#' @examples
#' library(units)
#' gallon <- make_unit('gallon')
#' mtcars$consumption <- mtcars$mpg * with(ud_units, mi / gallon)
#' mtcars$power <- mtcars$hp * with(ud_units, hp)
#'
#' # Use units encoded into the data
#' ggplot(mtcars) +
#'   geom_point(aes(power, consumption))
#'
#' # Convert units on the fly during plotting
#' ggplot(mtcars) +
#'   geom_point(aes(power, consumption)) +
#'   scale_x_unit(unit = 'W') +
#'   scale_y_unit(unit = 'km/l')
#'
#' # Resolve units when transforming data
#' ggplot(mtcars) +
#'   geom_point(aes(power, 1 / consumption))
#' @name scale_unit
#' @aliases NULL
NULL

#' @rdname scale_unit
#' @export
#' @importFrom scales censor
#' @importFrom ggplot2 waiver continuous_scale sec_axis
scale_x_unit <- function(name = waiver(), breaks = waiver(), unit = NULL,
                         minor_breaks = waiver(), labels = waiver(),
                         limits = NULL, expand = waiver(), oob = censor,
                         na.value = NA_real_, trans = 'identity',
                         position = 'bottom', sec.axis = waiver()) {
  if (!requireNamespace('units', quietly = TRUE)) {
    stop('The units package is required for this functionality', call. = FALSE)
  }
  sc <- continuous_scale(
    c('x', 'xmin', 'xmax', 'xend', 'xintercept', 'xmin_final', 'xmax_final',
      'xlower', 'xmiddle', 'xupper'),
    'position_c', identity,
    name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = 'none', position = position, super = ScaleContinuousPositionUnit
  )
  sc$unit <- switch(
    class(unit),
    symbolic_units = ,
    'NULL' = unit,
    character = units::make_unit(unit),
    units = units(unit),
    stop('unit must either be NULL or of class `units` or `symbolic_units`',
         call. = FALSE)
  )
  if (!inherits(sec.axis, 'waiver')) {
    if (inherits(sec.axis, 'formula')) sec.axis <- sec_axis(sec.axis)
    if (!inherits(sec.axis, 'AxisSecondary')) {
      stop('Secondary axes must be specified using \'sec_axis()\'',
           call. = FALSE)
    }
    sc$secondary.axis <- sec.axis
  }
  sc
}
#' @rdname scale_unit
#' @export
#' @importFrom scales censor
#' @importFrom ggplot2 waiver continuous_scale sec_axis
scale_y_unit <- function(name = waiver(), breaks = waiver(), unit = NULL,
                         minor_breaks = waiver(), labels = waiver(),
                         limits = NULL, expand = waiver(), oob = censor,
                         na.value = NA_real_, trans = 'identity',
                         position = 'left', sec.axis = waiver()) {
  if (!requireNamespace('units', quietly = TRUE)) {
    stop('The units package is required for this functionality', call. = FALSE)
  }
  sc <- continuous_scale(
    c('y', 'ymin', 'ymax', 'yend', 'yintercept', 'ymin_final', 'ymax_final',
      'lower', 'middle', 'upper'),
    'position_c', identity,
    name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = 'none', position = position, super = ScaleContinuousPositionUnit
  )
  sc$unit <- switch(
    class(unit),
    symbolic_units = ,
    'NULL' = unit,
    character = units::make_unit(unit),
    units = units(unit),
    stop('unit must either be NULL or of class `units` or `symbolic_units`',
         call. = FALSE)
  )
  if (!inherits(sec.axis, 'waiver')) {
    if (inherits(sec.axis, 'formula')) sec.axis <- sec_axis(sec.axis)
    if (!inherits(sec.axis, 'AxisSecondary')) {
      stop('Secondary axes must be specified using \'sec_axis()\'',
           call. = FALSE)
    }
    sc$secondary.axis <- sec.axis
  }
  sc
}
#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ScaleContinuousPosition ggproto_parent
#' @export
ScaleContinuousPositionUnit <- ggproto('ScaleContinuousPositionUnit', ScaleContinuousPosition,
  unit = NULL,

  train = function(self, x) {
    if (!requireNamespace('units', quietly = TRUE)) {
      stop('The units package is required for this functionality',
           call. = FALSE)
    }
    if (length(x) == 0) return()
    if (!is.null(self$unit)) {
      units(x) <- units::as_units(1, self$unit)
    }
    self$range$train(x)
  },
  map = function(self, x, limits = self$get_limits()) {
    if (inherits(x, 'units')) {
      if (is.null(self$unit)) {
        self$unit <- units(x)
      } else {
        units(x) <- units::as_units(1, self$unit)
      }
    }
    x <- as.numeric(x)
    ggproto_parent(ScaleContinuousPosition, self)$map(x, limits)
  },
  make_title = function(self, title) {
    if (!requireNamespace('units', quietly = TRUE)) {
      stop('The units package is required for this functionality',
           call. = FALSE)
    }
    units::make_unit_label(title, units::as_units(1, self$unit))
  }
)
#' @rdname scale_unit
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 scale_type
#' @export
scale_type.units <- function(x) c('unit', 'continuous')
