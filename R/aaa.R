utils::globalVariables(c(
  'x', 'y'
))

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

is.waive <- function(x) inherits(x, 'waiver')

`%|W|%` <- function(x, y) {
  if (is.waive(x)) y else x
}

expand_default <- function(scale, discrete = c(0, 0.6),
                           continuous = c(0.05, 0)) {
  scale$expand %|W|% if (scale$is_discrete()) discrete else continuous
}

combine_aes <- function(aes1, aes2) {
  aes_all <- c(aes1[setdiff(names(aes1), names(aes2))], aes2)
  class(aes_all) <- class(aes1)
  aes_all
}

empty_data <- function(x) {
  length(x) == 0 || nrow(x) == 0
}
