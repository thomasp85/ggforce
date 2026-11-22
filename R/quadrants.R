#' @rdname ggforce-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomAnnotateQuadrants <- ggproto(
  'GeomAnnotateQuadrants', Geom,
  draw_key = draw_key_blank,
  draw_panel = function(
    data, panel_params, coord, x_cutoff = NULL, y_cutoff = NULL,
    fill, alpha = 0.2, show_lines = FALSE, line_colour = 'grey40',
    line_width = 0.5, line_type = 'dashed'
  ) {
    if (!inherits(coord, 'CoordCartesian') || inherits(coord, 'CoordSf')) {
      cli::cli_abort('{.fn annotate_quadrants} requires a Cartesian coordinate system')
    }
    # Apply the scale transformation (log, reverse, ...) to the data-unit
    # cutoffs. Under coord_flip() the scale for data x sits on the panel's y.
    flip <- inherits(coord, 'CoordFlip')
    to_scale <- function(cutoff, view) {
      if (is.null(cutoff)) return(NA_real_)
      if (view$scale$is_discrete()) return(cutoff)
      view$scale$get_transformation()$transform(cutoff)
    }
    # coord_flip() swaps the columns so x and y are always the panel axes
    cut <- coord$transform(
      data.frame(
        x = to_scale(x_cutoff, if (flip) panel_params$y else panel_params$x),
        y = to_scale(y_cutoff, if (flip) panel_params$x else panel_params$y)
      ),
      panel_params
    )
    x <- pmin(pmax(cut$x, 0), 1)
    y <- pmin(pmax(cut$y, 0), 1)
    xs <- c(0, x[!is.na(x)], 1)
    ys <- c(0, y[!is.na(y)], 1)
    # Left to right, then bottom to top
    cells <- expand.grid(ix = seq_len(length(xs) - 1), iy = seq_len(length(ys) - 1))
    if (nrow(cells) != length(fill)) {
      cli::cli_abort('The cutoffs could not be transformed by the position scales')
    }
    rects <- grid::rectGrob(
      x = xs[cells$ix], y = ys[cells$iy],
      width = diff(xs)[cells$ix], height = diff(ys)[cells$iy],
      just = c('left', 'bottom'), default.units = 'npc',
      gp = grid::gpar(fill = scales::alpha(fill, alpha), col = NA)
    )
    x <- x[!is.na(x) & x > 0 & x < 1]
    y <- y[!is.na(y) & y > 0 & y < 1]
    if (!show_lines || length(x) + length(y) == 0) return(rects)
    lines <- grid::segmentsGrob(
      x0 = c(x, rep(0, length(y))), x1 = c(x, rep(1, length(y))),
      y0 = c(rep(0, length(x)), y), y1 = c(rep(1, length(x)), y),
      default.units = 'npc',
      gp = grid::gpar(col = line_colour, lwd = line_width * .pt, lty = line_type)
    )
    grid::grobTree(rects, lines)
  }
)


#' Annotate the panel background with rectangular sections
#'
#' This function divides the panel background into two or four coloured
#' sections, split at the given cutoffs. With only one cutoff the panel is
#' split in two, with both it is split into four quadrants. The cutoffs are
#' given in data units and respect scale transformations and coordinate
#' flipping. Unlike [ggplot2::geom_vline()] and [ggplot2::geom_hline()] they do
#' not expand the range of the scales; a cutoff outside the panel is placed at
#' the panel edge. The sections are drawn in the layer order so you should usually add this
#' annotation before the layers that should be drawn on top of it.
#'
#' When faceting, the same cutoffs are applied to every panel and placed
#' according to each panel's own scales, so with free scales a cutoff may fall
#' outside some panels. It is not supported by [facet_matrix()].
#'
#' @param x_cutoff,y_cutoff The position, in data units, at which to split the
#' panel along the x and y axis. A single number, or a `Date`/`POSIXct` for
#' date scales. At least one must be given.
#' @param fill The fill colours of the sections. Must be two colours if only
#' one cutoff is given and four if both are given. The colours are applied in
#' the order the sections appear in the panel: left to right and then bottom to
#' top (i.e. bottom-left, bottom-right, top-left, top-right for four sections).
#' @param alpha The opacity of the sections, between 0 and 1.
#' @param show_lines Logical. Should the dividing lines be drawn?
#' @param line_colour,line_width,line_type The colour, width (in mm), and line
#' type of the dividing lines.
#'
#' @return A ggplot2 layer
#'
#' @export
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   annotate_quadrants(x_cutoff = 3.5) +
#'   geom_point()
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   annotate_quadrants(
#'     x_cutoff = 3.5,
#'     y_cutoff = 20,
#'     fill = c('grey90', 'tomato', 'seagreen', 'grey90'),
#'     show_lines = TRUE
#'   ) +
#'   geom_point()
annotate_quadrants <- function(
  x_cutoff = NULL, y_cutoff = NULL, fill = NULL,
  alpha = 0.2, show_lines = FALSE,
  line_colour = 'grey40', line_width = 0.5,
  line_type = 'dashed'
) {
  for (cutoff in list(x_cutoff, y_cutoff)) {
    # Date/POSIXct keep their class so date scales can transform them
    ok_type <- is.numeric(cutoff) || inherits(cutoff, c('Date', 'POSIXct'))
    if (!is.null(cutoff) && !(ok_type && length(cutoff) == 1 && is.finite(cutoff))) {
      cli::cli_abort('{.arg x_cutoff} and {.arg y_cutoff} must be single finite numbers or dates')
    }
  }
  if (is.null(x_cutoff) && is.null(y_cutoff)) {
    cli::cli_abort('Either {.arg x_cutoff}, {.arg y_cutoff}, or both must be given')
  }
  n <- if (is.null(x_cutoff) || is.null(y_cutoff)) 2L else 4L
  fill <- fill %||% c('#F9D5D3', '#D6EAF8', '#FCF3CF', '#D5F5E3')[seq_len(n)]
  if (length(fill) != n) {
    cli::cli_abort('{.arg fill} must contain exactly {n} colours')
  }
  if (!(is.numeric(alpha) && length(alpha) == 1 && !is.na(alpha) && alpha >= 0 && alpha <= 1)) {
    cli::cli_abort('{.arg alpha} must be a number between 0 and 1')
  }
  if (!(is.logical(show_lines) && length(show_lines) == 1 && !is.na(show_lines))) {
    cli::cli_abort('{.arg show_lines} must be {.val TRUE} or {.val FALSE}')
  }

  layer(
    data = data.frame(.quadrant = 1L),
    mapping = NULL,
    stat = 'identity',
    geom = GeomAnnotateQuadrants,
    position = 'identity',
    inherit.aes = FALSE,
    show.legend = FALSE,
    params = list(
      x_cutoff = x_cutoff,
      y_cutoff = y_cutoff,
      fill = fill,
      alpha = alpha,
      show_lines = show_lines,
      line_colour = line_colour,
      line_width = line_width,
      line_type = line_type
    )
  )
}
