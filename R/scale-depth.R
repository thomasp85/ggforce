#' Scales for depth perception
#'
#' These scales serve to scale the depth aesthetic when creating stereographic
#' plots. The range specifies the relative distance between the points and the
#' paper plane in relation to the distance between the eyes and the paper plane
#' i.e. a range of c(-0.5, 0.5) would put the highest values midways between
#' the eyes and the image plane and the lowest values the same distance behind
#' the image plane. To ensure a nice viewing experience these values should not
#' exceed ~0.3 as it would get hard for the eyes to consolidate the two
#' pictures.
#'
#' @param ... arguments passed on to continuous_scale or discrete_scale
#'
#' @param range The relative range as related to the distance between the eyes
#' and the paper plane.
#'
#' @inheritParams ggplot2::continuous_scale
#'
#' @export
#' @importFrom scales rescale_pal
#'
#' @examples
#' ggplot(mtcars) +
#'   geom_point(aes(mpg, disp, depth = cyl)) +
#'   scale_depth(range = c(-0.1, 0.25)) +
#'   facet_stereo()
scale_depth <- function(..., range = c(0, 0.3)) {
  continuous_scale('depth', 'depth_c', rescale_pal(range), ...)
}

#' @rdname scale_depth
#'
#' @export
scale_depth_continuous <- scale_depth

#' @rdname scale_depth
#'
#' @export
scale_depth_discrete <- function(..., range = c(0, 0.3)) {
  discrete_scale(
    'depth', 'depth_d',
    function(n) seq(range[1], range[2], length.out = n), ...
  )
}
