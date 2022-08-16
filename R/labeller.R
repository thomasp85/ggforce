#' A labeller function to parse TeX syntax
#'
#' Thi function formats the strip labels of facet grids and wraps that contains
#' TeX expressions.
#'
#' @seealso [ggplot2::labeller], [latex2exp::TeX()]
#'
#' @inheritParams ggplot2::label_parsed
#' @inheritDotParams ggplot2::label_parsed -labels
#'
#' @examples
#' library(ggplot2)
#' d <- data.frame(x = 1, y = 1, facet = "$\\beta$")
#' ggplot(d, aes(x, y)) +
#'  geom_point() +
#'  facet_wrap(~ facet, labeller = label_tex)
#'
#' @importFrom latex2exp TeX
#' @importFrom ggplot2 label_parsed
#' @export
label_tex <- function(labels, ...) {
  label_parsed(
    as.data.frame(lapply(labels, latex2exp::TeX, output = "character")),
    ...
  )
}
