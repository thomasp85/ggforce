default_axis_guide <- NULL
.onLoad <- function(...) {
  if (utils::packageVersion("ggplot2") > "3.2.1") {
    default_axis_guide <<- ggplot2::waiver()
  } else {
    default_axis_guide <<- "none"
  }

  if ("element_geom" %in% getNamespaceExports("ggplot2")) {
    ggplot2::update_geom_defaults(GeomArc0, ggplot2::aes(colour = from_theme(colour %||% ink), linewidth = from_theme(linewidth), linetype = from_theme(linetype)))
    ggplot2::update_geom_defaults(GeomArcBar, ggplot2::aes(colour = from_theme(colour %||% ink)))
    ggplot2::update_geom_defaults(GeomCircle, ggplot2::aes(colour = from_theme(colour %||% ink)))
    ggplot2::update_geom_defaults(GeomMarkCircle, ggplot2::aes(colour = from_theme(colour %||% ink)))
    ggplot2::update_geom_defaults(GeomMarkEllipse, ggplot2::aes(colour = from_theme(colour %||% ink)))
    ggplot2::update_geom_defaults(GeomMarkHull, ggplot2::aes(colour = from_theme(colour %||% ink)))
    ggplot2::update_geom_defaults(GeomMarkRect, ggplot2::aes(colour = from_theme(colour %||% ink)))
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
