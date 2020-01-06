default_axis_guide <- NULL
.onLoad <- function(...) {
  if (utils::packageVersion("ggplot2") > "3.2.1") {
    default_axis_guide <<- ggplot2::waiver()
  } else {
    default_axis_guide <<- "none"
  }
}
