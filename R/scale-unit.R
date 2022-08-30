#' Position scales for units data
#'
#' `r lifecycle::badge('deprecated')` These are the default scales for the units
#' class. These will usually be added automatically. To override manually, use
#' `scale_*_unit`.
#'
#' @param ... Passed on to `units::scale_x_unit()` or `units::scale_y_unit()`
#'
#' @name scale_unit
#' @aliases NULL
#' @keywords internal
NULL

#' @rdname scale_unit
#' @export
#' @importFrom scales censor
scale_x_unit <- function(...) {
  lifecycle::deprecate_soft('0.3.4', "scale_x_unit()", "units::scale_x_unit()")
  check_installed('units', 'to use scale_x_unit')
  units::scale_x_units(...)
}
#' @rdname scale_unit
#' @export
#' @importFrom scales censor
scale_y_unit <- function(...) {
  lifecycle::deprecate_soft('0.3.4', "scale_y_unit()", "units::scale_y_unit()")
  check_installed('units', 'to use scale_y_unit')
  units::scale_y_units(...)
}
