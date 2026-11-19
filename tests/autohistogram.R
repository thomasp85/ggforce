library(ggforce)

plot <- ggplot(data.frame(value = c(1, 1, 5))) + facet_matrix(vars(value))
breaks <- c(0, 2, 4, 6)
counts <- function(...) {
  layer_data(plot + geom_autohistogram(breaks = breaks, ...))$count
}

stopifnot(identical(counts(), c(2, 0, 1)))
stopifnot(identical(counts(drop = FALSE), c(2, 0, 1)))
if (packageVersion("ggplot2") >= "4.0.0") {
  stopifnot(identical(counts(drop = TRUE), c(2, 1)))
}
