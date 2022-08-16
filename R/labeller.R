#' A labeller function to parse TeX syntax
#'
#' This function formats the strip labels of facet grids and wraps that contains
#' TeX expressions. The latex2exp package must be installed.
#'
#' @seealso [ggplot2::labeller], [latex2exp::TeX()]
#'
#' @inheritParams ggplot2::label_parsed
#' @inheritDotParams ggplot2::label_parsed -labels
#'
#' @examples
#' # requires latex2exp package be installed
#' if (requireNamespace("latex2exp", quietly = TRUE)) {
#'   library(ggplot2)
#'   d <- data.frame(x = 1, y = 1, facet = "$\\beta$")
#'   ggplot(d, aes(x, y)) +
#'     geom_point() +
#'     facet_wrap(~ facet, labeller = label_tex)
#' }
#' @importFrom ggplot2 label_parsed
#' @export
label_tex <- function(labels, ...) {
  if (!requireNamespace("latex2exp", quiet = TRUE)) {
    stop("The latex2exp package is needed for this functionality", call. = FALSE)
  }
  label_parsed(
    as.data.frame(
      lapply(labels, latex2exp::TeX, output = "character"),
      stringsAsFactors = FALSE
    ),
    ...
  )
}
