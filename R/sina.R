#' Sina plot
#'
#' The sina plot is a data visualization chart suitable for plotting any single
#' variable in a multiclass dataset. It is an enhanced jitter strip chart,
#' where the width of the jitter is controlled by the density distribution of
#' the data within each class.
#'
#' @details There are two available ways to define the x-axis borders for the
#' samples to spread within:
#' \itemize{
#'  \item{`method == "density"`
#'
#'    A density kernel is estimated along the y-axis for every sample group. The
#'    borders are then defined by the density curve. Tuning parameter
#'    `adjust` can be used to control the density bandwidth in the same way
#'    it is used in [stats::density()]. }
#'
#'  \item{`method == "counts"`:
#'
#'    The borders are defined by the number of samples that occupy the same bin.
#'
#'   }
#' }
#'
#' @section Aesthetics:
#' geom_sina understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x**
#' - **y**
#' - color
#' - group
#' - size
#' - alpha
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param binwidth The width of the bins. The default is to use `bins`
#'   bins that cover the range of the data. You should always override
#'   this value, exploring multiple widths to find the best to illustrate the
#'   stories in your data.
#'
#' @param bins Number of bins. Overridden by binwidth. Defaults to 50.
#'
#' @param scale Logical. When set to `TRUE` x-coordinate widths across all
#' groups are scaled based on the densiest area in the plot.
#' Default: `TRUE`
#'
#' @param method Choose the method to spread the samples within the same
#' bin along the x-axis. Available methods: "density", "counts" (can be
#' abbreviated, e.g. "d"). See `Details`.
#'
#' @param maxwidth Control the maximum width the points can spread into. Values
#' between 0 and 1.
#'
#' @param adjust Adjusts the bandwidth of the density kernel when
#' `method == "density"` (see [stats::density()]).
#'
#' @param bin_limit If the samples within the same y-axis bin are more
#' than `bin_limit`, the samples's X coordinates will be adjusted.
#'
#' @param seed a single value that controls the random sample jittering. Set to
#' an integer to enable plot reproducibility. Default NULL.
#'
#' @author Nikos Sidiropoulos
#'
#' @name geom_sina
#' @rdname geom_sina
#'
#' @section Computed variables:
#'
#' \describe{
#'   \item{bin_counts}{sample counts per bin per group}
#'   \item{scaled}{adjusted x-coordinates}
#' }
#'
#'
#' @examples
#' ggplot(midwest, aes(state, area)) + geom_point()
#'
#' # Boxplot and Violin plots convey information on the distribution but not the
#' # number of samples, while Jitter does the opposite.
#' ggplot(midwest, aes(state, area)) + geom_violin()
#' ggplot(midwest, aes(state, area)) + geom_jitter()
#'
#' # Sina does both!
#' ggplot(midwest, aes(state, area)) + geom_violin() + geom_sina()
#'
#' p <- ggplot(midwest, aes(state, popdensity)) + scale_y_log10()
#' p + geom_sina()
#'
#' # Colour the points based on the data set's columns
#' p + geom_sina(aes(colour = inmetro))
#'
#' # Or any other way
#' cols <- midwest$popdensity > 10000
#' p + geom_sina(colour = cols + 1L)
#'
#' # Sina plots with continuous x:
#' p <- ggplot(midwest, aes(cut_width(area, 0.02), popdensity)) + scale_y_log10()
#' p + geom_sina()
#'
#'
#' ###Sample gaussian distributions
#' # Unimodal
#' a <- rnorm(500, 6, 1)
#' b <- rnorm(400, 5, 1.5)
#'
#' # Bimodal
#' c <- c(rnorm(200, 3, .7), rnorm(50, 7, 0.4))
#'
#' # Trimodal
#' d <- c(rnorm(200, 2, 0.7), rnorm(300, 5.5, 0.4), rnorm(100, 8, 0.4))
#'
#' df <- data.frame(
#'   "Distribution" = c(rep("Unimodal 1", length(a)),
#'                      rep("Unimodal 2", length(b)),
#'                      rep("Bimodal", length(c)),
#'                      rep("Trimodal", length(d))),
#'   "Value" = c(a, b, c, d))
#'
#' # Reorder levels
#' df$Distribution <- factor(df$Distribution,
#'                           levels(df$Distribution)[c(3, 4, 1, 2)])
#'
#' p <- ggplot(df, aes(Distribution, Value))
#' p + geom_boxplot()
#' p + geom_violin() + geom_sina()
#'
#' # By default, Sina plot scales the width of the class according to the width
#' # of the class with the highest density. Turn group-wise scaling off with:
#' p + geom_violin() + geom_sina(scale = FALSE)
NULL

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @importFrom plyr ddply
#' @export
StatSina <- ggproto("StatSina", Stat,

  required_aes = c("x", "y"),

  default_aes = aes(xend = ..scaled..),

  setup_data = function(data, params) {
    if (is.double(data$x) && !.has_groups(data) && any(data$x != data$x[1L])) {
      stop("Continuous x aesthetic -- did you forget aes(group=...)?",
           call. = FALSE)
    }

    data
  },

  setup_params = function(data, params) {
    #Limit maxwidth to 0.96 to leave some space between groups
    if (!is.null(params$maxwidth))
      params$maxwidth <- abs(params$maxwidth) # needs to be positive
    else
      params$maxwidth <- 0.96


    if (!is.null(params$seed)) {
      set.seed(params$seed)
    }

    if (is.null(params$binwidth) && is.null(params$bins)) {
      params$bins <- 50
    }

    params
  },

  compute_panel = function(self, data, scales, binwidth = NULL, bins = NULL,
                           scale = TRUE, method = "density", maxwidth = NULL,
                           adjust = 1, bin_limit = 1, seed = NULL, na.rm = FALSE) {
    if (!is.null(binwidth))
      bins <- bin_breaks_width(scales$y$dimension() + 1e-8, binwidth)
    else
      bins <- bin_breaks_bins(scales$y$dimension() + 1e-8, bins)

    data <- ggproto_parent(Stat, self)$compute_panel(data, scales,
      scale = scale, method = method, maxwidth = maxwidth, adjust = adjust,
      bin_limit = bin_limit, seed = seed, bins = bins$breaks, na.rm = na.rm)

    #scale all bins based on their density relative to the densiest bin
    if (scale) {
      group_scaling_factor <-
        ddply(data, "group", plyr::mutate,
              group_max = max(bin_counts))$group_max / max(data$bin_counts)
    } else {
      group_scaling_factor <- 1
    }

    data$scaled <- data$x + data$x_translation * group_scaling_factor
    data$x_translation <- NULL

    #jitter y values if the input is input is integer
    if (all(data$y == floor(data$y)))
      data$y <- jitter(data$y)

    data
  },

  compute_group = function(data, scales, scale = TRUE, method = "density",
                           maxwidth = NULL, adjust = 1, bin_limit = 1,
                           seed = NULL, bins = NULL, na.rm = FALSE) {

    #initialize x_translation and bin_counts to 0
    data$x_translation <- data$bin_counts <- rep(0, nrow(data))

    #if group has less than 2 points return as is
    if (nrow(data) < 2) {
      data$bin_counts <- 1
      return(data)
    }

    #per bin sample count
    bin_counts <- table(findInterval(data$y, bins))

    #per bin sample density
    if (method == "density") {
      densities <- stats::density(data$y, adjust = adjust)

      #confine the samples in a (-maxwidth/2, -maxwidth/2) area around the
      #group's center
      intra_scaling_factor <- 0.5 * maxwidth / max(densities$y)

      data$bin <- findInterval(data$y, densities$x)

      data$bin_counts <- as.numeric(bin_counts[match(findInterval(data$y, bins),
                                                     names(bin_counts))])

      x_translation <- sapply(densities$y[data$bin], jitter, x = 0, factor = 1)
      data$x_translation <- x_translation * intra_scaling_factor

      data$bin <- NULL

    } else {
      #allow up to 50 samples in a bin without scaling
      intra_scaling_factor <- 50 * maxwidth / max(bin_counts)

      for (i in names(bin_counts)) {
        #examine bins with more than 'bin_limit' samples
        if (bin_counts[i] > bin_limit) {
          cur_bin <- bins[ as.integer(i) : (as.integer(i) + 1)]

          #find samples in the current bin and translate their X coord.
          points <- findInterval(data$y, cur_bin) == 1

          #compute the border margin for the current bin.
          xmax <- bin_counts[i] / 100

          #assign the samples uniformely within the specified range
          x_translation <- stats::runif(bin_counts[i], - xmax, xmax)

          #scale and store new x coordinates
          data$x_translation[points] <- x_translation * intra_scaling_factor
          #store bin counts. Used for group-wise scaling.
          data$bin_counts[points] <- bin_counts[i]
        }
      }
    }
    data
  }
)

#' @rdname geom_sina
#' @importFrom ggplot2 layer
#' @export
stat_sina <-function(mapping = NULL, data = NULL,
                     geom = "sina", position = "identity",
                     ...,
                     binwidth = NULL,
                     bins = NULL,
                     scale = TRUE,
                     method = "density",
                     maxwidth = NULL,
                     adjust = 1,
                     bin_limit = 1,
                     seed = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  method <- match.arg(method, c("density", "counts"))

  layer(
    data = data,
    mapping = mapping,
    stat = StatSina,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      scale = scale,
      method = method,
      maxwidth = maxwidth,
      adjust = adjust,
      bin_limit = bin_limit,
      seed = seed,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomPoint
#' @export
GeomSina <- ggproto("GeomSina", GeomPoint,

                    setup_data = function(data, params) {
                      transform(data, x = xend)
                    }
)


#' @rdname geom_sina
#' @importFrom ggplot2 layer
#' @export
geom_sina <- function(mapping = NULL, data = NULL,
                      stat = "sina", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSina,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )

}



# Binning functions -------------------------------------------------------

bins <- function(breaks, closed = c("right", "left"),
                 fuzz = 1e-08 * stats::median(diff(breaks))) {
  stopifnot(is.numeric(breaks))
  closed <- match.arg(closed)

  breaks <- sort(breaks)
  # Adapted base::hist - this protects from floating point rounding errors
  if (closed == "right") {
    fuzzes <- c(-fuzz, rep.int(fuzz, length(breaks) - 1))
  } else {
    fuzzes <- c(rep.int(-fuzz, length(breaks) - 1), fuzz)
  }

  structure(
    list(
      breaks = breaks,
      fuzzy = breaks + fuzzes,
      right_closed = closed == "right"
    ),
    class = "ggplot2_bins"
  )
}

# Compute parameters -----------------------------------------------------------

bin_breaks <- function(breaks, closed = c("right", "left")) {
  bins(breaks, closed)
}

bin_breaks_width <- function(x_range, width = NULL, center = NULL,
                             boundary = NULL, closed = c("right", "left")) {
  stopifnot(length(x_range) == 2)

  # if (length(x_range) == 0) {
  #   return(bin_params(numeric()))
  # }
  stopifnot(is.numeric(width), length(width) == 1)
  if (width <= 0) {
    stop("`binwidth` must be positive", call. = FALSE)
  }

  if (!is.null(boundary) && !is.null(center)) {
    stop("Only one of 'boundary' and 'center' may be specified.")
  } else if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither edge nor center given, compute both using tile layer's
      # algorithm. This puts min and max of data in outer half of their bins.
      boundary <- width / 2

    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }

  # Find the left side of left-most bin: inputs could be Dates or POSIXct, so
  # coerce to numeric first.
  x_range <- as.numeric(x_range)
  width <- as.numeric(width)
  boundary <- as.numeric(boundary)
  shift <- floor((x_range[1] - boundary) / width)
  origin <- boundary + shift * width

  # Small correction factor so that we don't get an extra bin when, for
  # example, origin = 0, max(x) = 20, width = 10.
  max_x <- x_range[2] + (1 - 1e-08) * width
  breaks <- seq(origin, max_x, width)

  bin_breaks(breaks, closed = closed)
}

bin_breaks_bins <- function(x_range, bins = 30, center = NULL,
                            boundary = NULL, closed = c("right", "left")) {
  stopifnot(length(x_range) == 2)

  bins <- as.integer(bins)
  if (bins < 1) {
    stop("Need at least one bin.", call. = FALSE)
  } else if (bins == 1) {
    width <- diff(x_range)
    boundary <- x_range[1]
  } else {
    width <- (x_range[2] - x_range[1]) / (bins - 1)
  }

  bin_breaks_width(x_range, width, boundary = boundary, center = center,
                   closed = closed)
}

.has_groups <- function(data) {
  # If no group aesthetic is specified, all values of the group column equal to
  # -1L. On the other hand, if a group aesthetic is specified, all values
  # are different from -1L (since they are a result of plyr::id()). NA is
  # returned for 0-row data frames.
  data$group[1L] != -1L
}
