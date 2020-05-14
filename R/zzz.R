default_axis_guide <- NULL
.onLoad <- function(...) {
  if (utils::packageVersion("ggplot2") > "3.2.1") {
    default_axis_guide <<- ggplot2::waiver()
  } else {
    default_axis_guide <<- "none"
  }
  
  # Registering of theme elements
  register_theme_elements(
    ggforce.zoom = element_rect(),
    ggforce.zoom.funnel = element_rect(),
    ggforce.zoom.funnel.x = element_rect(),
    ggforce.zoom.funnel.y = element_rect(),
    ggforce.zoom.area = element_rect(),
    ggforce.zoom.area.x = element_rect(),
    ggforce.zoom.area.y = element_rect(),
    element_tree = list(
      ggforce.zoom = el_def("element_rect", "strip.background", 
                            "zoom elements: funnels and areas"),
      ggforce.zoom.funnel = el_def("element_rect", "ggforce.zoom",
                                   "zoom funnel between panels"),
      ggforce.zoom.funnel.x = el_def("element_rect", "ggforce.zoom.funnel"),
      ggforce.zoom.funnel.y = el_def("element_rect", "ggforce.zoom.funnel"),
      ggforce.zoom.area = el_def("element_rect", "ggforce.zoom",
                                 "indicator area for zooming"),
      ggforce.zoom.area.x = el_def("element_rect", "ggforce.zoom.area"),
      ggforce.zoom.area.y = el_def("element_rect", "ggforce.zoom.area")
    )
  )
}
