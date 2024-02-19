concaveman <- function(points, concavity, threshold) {
  if (nrow(points) < 4) return(unname(points))
  hull <- as.integer(grDevices::chull(points)) - 1L
  concaveman_c(points, hull, concavity, threshold)
}
