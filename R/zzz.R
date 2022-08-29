default_axis_guide <- NULL
.onLoad <- function(...) {
  if (utils::packageVersion("ggplot2") > "3.2.1") {
    default_axis_guide <<- ggplot2::waiver()
  } else {
    default_axis_guide <<- "none"
  }
  ggplot2::register_theme_elements(
    zoom = element_rect(),
    zoom.x = element_rect(),
    zoom.y = element_rect(),
    element_tree = list(
      zoom = ggplot2::el_def('element_rect', 'strip.background'),
      zoom.x = ggplot2::el_def('element_rect', 'zoom'),
      zoom.y = ggplot2::el_def('element_rect', 'zoom')
    )
  )
}
