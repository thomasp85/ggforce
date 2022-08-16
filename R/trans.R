#' Create a power transformation object
#'
#' This function can be used to create a proper trans object that encapsulates
#' a power transformation (x^n).
#'
#' @param n The degree of the power transformation
#'
#' @return A trans object
#'
#' @importFrom scales trans_new extended_breaks format_format
#' @importFrom MASS fractions
#'
#' @export
#'
#' @examples
#' # Power of 2 transformations
#' trans <- power_trans(2)
#' trans$transform(1:10)
#'
#' # Cubic root transformation
#' trans <- power_trans(1 / 3)
#' trans$transform(1:10)
#'
#' # Use it in a plot
#' ggplot() +
#'   geom_line(aes(x = 1:10, y = 1:10)) +
#'   scale_x_continuous(trans = power_trans(2),
#'                      expand = c(0, 1))
power_trans <- function(n) {
  trans_new(
    name = paste0('power of ', fractions(n)),
    transform = function(x) {
      x^n
    },
    inverse = function(x) {
      x^(1 / n)
    },
    breaks = extended_breaks(),
    format = format_format(),
    domain = c(0, Inf)
  )
}
#' Create radial data in a cartesian coordinate system
#'
#' This function creates a trans object that converts radial data to their
#' corresponding coordinates in cartesian space. The trans object is created for
#' a specific radius and angle range that will be mapped to the unit circle so
#' data doesn't have to be normalized to 0-1 and 0-2*pi in advance. While there
#' exists a clear mapping from radial to cartesian, the inverse is not true as
#' radial representation is periodic. It is impossible to know how many
#' revolutions around the unit circle a point has taken from reading its
#' coordinates. The inverse function will always assume that coordinates are in
#' their first revolution i.e. map them back within the range of a.range.
#'
#' @param r.range The range in radius that correspond to 0 - 1 in the unit
#' circle.
#'
#' @param a.range The range in angles that correspond to 2*pi - 0. As radians
#' are normally measured counterclockwise while radial displays are read
#' clockwise it's an inverse mapping
#'
#' @param offset The offset in angles to apply. Determines that start position
#' on the circle. pi/2 (the default) corresponds to 12 o'clock.
#'
#' @param pad Adds to the end points of the angle range in order to separate the
#' start and end point. Defaults to 0.5
#'
#' @param clip Should input data be clipped to r.range and a.range or be allowed
#' to extend beyond. Defaults to FALSE (no clipping)
#'
#' @return A trans object. The transform method for the object takes an r
#' (radius) and a (angle) argument and returns a data.frame with x and y columns
#' with rows for each element in r/a. The inverse method takes an x and y
#' argument and returns a data.frame with r and a columns and rows for each
#' element in x/y.
#'
#' @note While trans objects are often used to modify scales in ggplot2, radial
#' transformation is different as it is a coordinate transformation and takes
#' two arguments. Consider it a trans version of coord_polar and use it to
#' transform your data prior to plotting.
#'
#' @importFrom scales trans_new extended_breaks format_format
#'
#' @export
#'
#' @examples
#' # Some data in radial form
#' rad <- data.frame(r = seq(1, 10, by = 0.1), a = seq(1, 10, by = 0.1))
#'
#' # Create a transformation
#' radial <- radial_trans(c(0, 1), c(0, 5))
#'
#' # Get data in x, y
#' cart <- radial$transform(rad$r, rad$a)
#'
#' # Have a look
#' ggplot() +
#'   geom_path(aes(x = x, y = y), data = cart, color = 'forestgreen') +
#'   geom_path(aes(x = r, y = a), data = rad, color = 'firebrick')
radial_trans <- function(r.range, a.range, offset = pi / 2, pad = 0.5,
                         clip = FALSE) {
  a.range[which.min(a.range)] <- min(a.range) - pad
  a.range[which.max(a.range)] <- max(a.range) + pad
  trans_new(
    name = paste0(
      'radial-to-cartesian: ',
      r.range[1], '-', r.range[2], ' -> 0-1; ',
      a.range[1], '-', a.range[2], ' -> 2pi-0'
    ),
    transform = function(r, a) {
      if (clip) {
        r[r < min(r.range)] <- min(r.range)
        r[r > max(r.range)] <- max(r.range)
        a[a < min(a.range)] <- min(a.range)
        a[a > max(a.range)] <- max(a.range)
      }
      if (diff(r.range) == 0) {
        r <- 1
      } else {
        r <- (r - r.range[1]) / diff(r.range)
      }
      if (diff(a.range) == 0) {
        a <- offset
      } else {
        a <- offset + (a - a.range[1]) / diff(a.range) * -2 * pi
      }
      data.frame(x = r * cos(a), y = r * sin(a))
    },
    inverse = function(x, y) {
      r <- sqrt(x^2 + y^2) * diff(r.range) + r.range[1]
      angle <- -(atan2(y, x) - offset)
      angle[angle < 0] <- 2 * pi + angle[angle < 0]
      a <- angle / (2 * pi) * diff(a.range) + a.range[1]
      data.frame(r = r, a = a)
    },
    breaks = extended_breaks(),
    format = format_format()
  )
}
#' Reverse a transformation
#'
#' While the scales package export a reverse_trans object it does not allow for
#' reversing of already transformed ranged - e.g. a reverse exp transformation
#' is not possible. trans_reverser takes a trans object or something coercible
#' to one and creates a reverse version of it.
#'
#' @param trans A trans object or an object that can be converted to one using
#' [scales::as.trans()]
#'
#' @return A trans object
#'
#' @importFrom scales as.trans trans_new asn_trans atanh_trans boxcox_trans
#' date_trans exp_trans identity_trans log10_trans log1p_trans log2_trans
#' logit_trans log_trans probability_trans probit_trans reciprocal_trans
#' reverse_trans sqrt_trans time_trans
#'
#' @export
#'
#' @examples
#' # Lets make a plot
#' p <- ggplot() +
#'   geom_line(aes(x = 1:10, y = 1:10))
#'
#' # scales already have a reverse trans
#' p + scale_x_continuous(trans = 'reverse')
#'
#' # But what if you wanted to reverse an already log transformed scale?
#' p + scale_x_continuous(trans = trans_reverser('log'))
trans_reverser <- function(trans) {
  transformOrig <- as.trans(trans)
  trans_new(
    name = paste0('reverse-', transformOrig$name),
    transform = function(x) {
      -transformOrig$transform(x)
    },
    inverse = function(x) {
      transformOrig$inverse(-x)
    },
    breaks = transformOrig$breaks,
    format = transformOrig$format,
    domain = transformOrig$domain
  )
}
