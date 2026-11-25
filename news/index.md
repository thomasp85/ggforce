# Changelog

## ggforce (development version)

## ggforce 0.6.0

CRAN release: 2026-11-25

- Fixed a bug in the enclosing ellipse calculation that could return a
  mirrored (wrongly oriented) ellipse for some point configurations, and
  could error on input with leading duplicate points
  ([\#354](https://github.com/thomasp85/ggforce/issues/354))
- The `geom_mark_*()` family now gracefully ignores non-finite positions
  arising during coordinate transformation instead of corrupting the
  mark and label placement
  ([\#347](https://github.com/thomasp85/ggforce/issues/347))
- Fixed a bug in
  [`geom_mark_hull()`](https://ggforce.data-imaginist.com/reference/geom_mark_hull.md)
  where near-collinear groups with duplicated points would produce
  hugely oversized hulls due to unbounded miter joins
  ([\#345](https://github.com/thomasp85/ggforce/issues/345))
- The deprecated `label.fill` and `label.colour` arguments in the
  `geom_mark_*()` family now work again, including the `"inherit"`
  keyword, by being mapped onto the `style` aesthetic
  ([\#331](https://github.com/thomasp85/ggforce/issues/331))
- [`geom_link2()`](https://ggforce.data-imaginist.com/reference/geom_link.md)
  now works with single-observation groups instead of erroring
  ([\#328](https://github.com/thomasp85/ggforce/issues/328))
- **BREAKING** The `geom_mark_*()` family now uses marquee for the
  content of the text boxes. Use markdown to add titles etc rather than
  the previous label title and body.
- [`geom_autohistogram()`](https://ggforce.data-imaginist.com/reference/geom_autohistogram.md)
  draws continuous histograms with ggplot2 4.0.
- The vendored `robust_predicates` C++ library has been replaced with a
  new in-house implementation, making the package lighter and faster to
  install, and the additional license restrictions in `LICENSE.note`
  have been removed
  ([\#348](https://github.com/thomasp85/ggforce/issues/348),
  [\#349](https://github.com/thomasp85/ggforce/issues/349))
- Compatibility with the S7-based ggplot2
  ([\#359](https://github.com/thomasp85/ggforce/issues/359))

## ggforce 0.5.0

CRAN release: 2025-06-18

- Fixed a bug that would cause reordering of data in some geoms
  ([\#314](https://github.com/thomasp85/ggforce/issues/314))
- The concaveman package is no longer a dependency for
  [`geom_mark_hull()`](https://ggforce.data-imaginist.com/reference/geom_mark_hull.md)
  ([\#308](https://github.com/thomasp85/ggforce/issues/308))
- Setting both label and description to `NA` will now turn off label
  drawing for that mark completely while still drawing the mark
- Fix a bug in
  [`geom_mark_rect()`](https://ggforce.data-imaginist.com/reference/geom_mark_rect.md)
  where the mark would not be shown in all panels
  ([\#307](https://github.com/thomasp85/ggforce/issues/307))
- Fixed a bug in the `geom_mark_*()` geoms where having `NA` values in
  the `filter` aesthetic would lead to weird errors. `NA` is now treated
  as `FALSE` ([\#306](https://github.com/thomasp85/ggforce/issues/306))
- [`shapeGrob()`](https://ggforce.data-imaginist.com/reference/shapeGrob.md)
  is now exported for use by other packages
  ([\#303](https://github.com/thomasp85/ggforce/issues/303))
- Fix a bug in
  [`geom_autohistogram()`](https://ggforce.data-imaginist.com/reference/geom_autohistogram.md)
  that prevented it to be used with continuous data
  ([\#297](https://github.com/thomasp85/ggforce/issues/297))
- [`facet_zoom()`](https://ggforce.data-imaginist.com/reference/facet_zoom.md)
  now throws a better error when used with
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  ([\#143](https://github.com/thomasp85/ggforce/issues/143))
- You can now use `"inherit"`, `"inherit_fill"`, and `"inherit_col"` for
  the styling of the label box and connector in the `geom_mark_*()`
  family of geoms
  ([\#240](https://github.com/thomasp85/ggforce/issues/240))
- Prepare for upcoming changes in ggplot2

## ggforce 0.4.2

CRAN release: 2024-02-19

- Fix a documentation issue reported by CRAN

## ggforce 0.4.1

CRAN release: 2022-10-04

- Fixed a sanitizer error in the decent calculations used for the mark
  geoms
- Fixed a typo bug in the vctrs implementations

## ggforce 0.4.0

CRAN release: 2022-09-29

- Moved to vctrs internally
- Updated error messaging to use cli
- [`geom_diagonal()`](https://ggforce.data-imaginist.com/reference/geom_diagonal.md),
  [`geom_diagonal_wide()`](https://ggforce.data-imaginist.com/reference/geom_diagonal_wide.md),
  [`geom_parallel_sets()`](https://ggforce.data-imaginist.com/reference/geom_parallel_sets.md),
  and
  [`geom_sina()`](https://ggforce.data-imaginist.com/reference/geom_sina.md)
  are now bidirectional in the style of ggplot2
- `geom_mark_*()` now works correctly in the presence of `NA` values in
  `x` and `y`
- The `zoom`, `zoom.x`, and `zoom.y` elements now uses proper
  registration of theme elements, inheriting from `strip.background`.
- Fixed bug in
  [`geom_sina()`](https://ggforce.data-imaginist.com/reference/geom_sina.md)
  when groups contained less than 3 points
- Fixed bug in
  [`geom_parallel_sets()`](https://ggforce.data-imaginist.com/reference/geom_parallel_sets.md)
  that erroneously removed grouping information in some configurations
- Added `jitter_y` argument to
  [`geom_sina()`](https://ggforce.data-imaginist.com/reference/geom_sina.md)
  to control whether y jittering is performed on integerish y values.
- [`facet_zoom()`](https://ggforce.data-imaginist.com/reference/facet_zoom.md)
  now works with patchwork
- Fix bug in
  [`geom_mark_ellipse()`](https://ggforce.data-imaginist.com/reference/geom_mark_ellipse.md)
  that caused wrong orientation of ellipses with groups of two.
- [`gather_set_data()`](https://ggforce.data-imaginist.com/reference/gather_set_data.md)
  now supports tidyselect.
- [`position_jitternormal()`](https://ggforce.data-imaginist.com/reference/position_jitternormal.md)
  gains a `seed` argument in parallel to
  [`ggplot2::position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html)
- [`geom_sina()`](https://ggforce.data-imaginist.com/reference/geom_sina.md)
  now works when the group only have a single unique y value
- [`facet_zoom()`](https://ggforce.data-imaginist.com/reference/facet_zoom.md)
  now works correctly with transformed scales
- [`facet_wrap_paginate()`](https://ggforce.data-imaginist.com/reference/facet_wrap_paginate.md)
  now works correctly with `dir = 'v'`
- [`facet_matrix()`](https://ggforce.data-imaginist.com/reference/facet_matrix.md)
  now supports a labeller
- fix a bug in
  [`geom_parallel_sets_axes()`](https://ggforce.data-imaginist.com/reference/geom_parallel_sets.md)
  that prevented coloring of axis fill
- fix a bug in
  [`stat_circle()`](https://ggforce.data-imaginist.com/reference/geom_circle.md)
  if the `x` and `y` aesthetics were inherited from the global mapping
- [`facet_zoom()`](https://ggforce.data-imaginist.com/reference/facet_zoom.md)
  now works even when limits are set by the scales

## ggforce 0.3.4

CRAN release: 2022-08-18

- Changed documentation to comply with new units package
- Fixed unintentional re-ordering of shapes
  ([\#224](https://github.com/thomasp85/ggforce/issues/224))
- Deprecate `scale_[x|y]_unit` in favor of `units::scale_[x|y]_units`

## ggforce 0.3.2

CRAN release: 2020-06-23

- Changes to comply with latest ggplot2 release
- Make sure ggforce pass test even if concaveman is not available

## ggforce 0.3.1

CRAN release: 2019-08-20

- Better fix for gganimate compatibility
  ([\#157](https://github.com/thomasp85/ggforce/issues/157))

## ggforce 0.3.0

CRAN release: 2019-08-09

- Added
  [`facet_matrix()`](https://ggforce.data-imaginist.com/reference/facet_matrix.md)
  in order to facet different data columns into different rows and
  columns in a grid, in order to make e.g. scatterplot matrices and
  pairs plot
- Added
  [`geom_autopoint()`](https://ggforce.data-imaginist.com/reference/geom_autopoint.md)
  and
  [`position_auto()`](https://ggforce.data-imaginist.com/reference/position_auto.md)
  to jitter points based on the type of positional scales in the panel
- Added
  [`geom_autohistogram()`](https://ggforce.data-imaginist.com/reference/geom_autohistogram.md)
  and
  [`geom_autodensity()`](https://ggforce.data-imaginist.com/reference/geom_autohistogram.md)
  for facilitating distribution plots along the diagonal in a
  [`facet_matrix()`](https://ggforce.data-imaginist.com/reference/facet_matrix.md)
  plot.
- Added
  [`facet_row()`](https://ggforce.data-imaginist.com/reference/facet_row.md)
  and `facet_col` to have one-dimensional
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  with possibility of variable sizing.
- Stats should now always keep the old group variable (potentially with
  something added), making them work with gganimate
- Removed the *Visual Guide* vignette to reduce compilation time. See
  the website for an overview of all functionality with compiled
  examples (<https://ggforce.data-imaginist.com>)

## ggforce 0.2.2

CRAN release: 2019-04-23

- Fixed a regression in
  [`geom_sina()`](https://ggforce.data-imaginist.com/reference/geom_sina.md)
  where the computation would fail with a warning due to
  [`tolower()`](https://rdrr.io/r/base/chartr.html) being masked
  ([\#134](https://github.com/thomasp85/ggforce/issues/134),
  [\#135](https://github.com/thomasp85/ggforce/issues/135)).

## ggforce 0.2.1

CRAN release: 2019-03-12

- Fixed a bug in the calculation of open and closed b-splines, where the
  interval would exceed the defined region and result in an
  out-of-bounds memory error

## ggforce 0.2.0

CRAN release: 2019-03-07

### New features

- `linear_trans` for composing linear transformation using `rotate`,
  `stretch`, `shear`, `reflect`, and `translate`
- `facet_stereo` added for creating stereographic projections
- `geom_voronoi_[tile|segment]`,
  `geom_delaunay_[tile|segment|segment2]`, and `stat_delvor_summary` has
  been added for tesselation and triangulation.
- `geom_spiro` has been added for drawing spirographs
- Add `geom_ellipse` for drawing regular and superellipses
- Add `geom_regon` for drawing regular polygons
- Add `geom_diagonal`, `geom_diagonal_wide` and `geom_parallel_sets` for
  drawing parallel sets diagrams and other visualizations based on
  diagonals.
- Add `geom_shape` for drawing polygons with rounded corners and
  expanded/contracted sides. `geom_shape` replaces all `geom_polygon`
  internally.
- Added `geom_bspline_closed` to draw polygons defined as b-splines
- Add `geom_mark_[rect|circle|ellipse|hull]` to encircle a group of
  points and optionally add textual annotation to it
- Add `position_jitternormal` to jitter points based on a normal
  distribution
  1.  

### Improvements

- `facet_[wrap|grid]_paginate` will now try to make panels on the last
  page the same size as on full pages
  ([\#7](https://github.com/thomasp85/ggforce/issues/7))
- `facet_zoom` now gains `xlim` and `ylim` arguments to control zoom
  range directly
- `facet_zoom` now gains `zoom.data` to control which data gets plotted
  in which panel
- Slimmed down the dependencies for the package. `plyr`, `lazyeval` and
  `dplyr` has all been removed
- Rewrite `geom_sina` to match `geom_violin` and allow for dodging
- Add `open`/`clamped` option to \`geom_bspline

### Bug fixes

- Fix interpolation of `x` and `y` values in `geom_link2`
  ([@thomasp85](https://github.com/thomasp85) and
  2.  
- `stat_link` no longer replicates the group column
- arcs and links no longer rename aesthetics when only one aesthetic is
  present (`drop = FALSE`)
- `stat_bezier0` and `stat_bezier2` now return data in the expected
  format
- Fix bug with `n_pages` due to internal changes in ggplot2
- Fix bug in `facet_zoom` in combination with secondary y-axis where the
  space for the y-axis would become huge
- Correctly detect and error out when scales and coords does not work
  with `facet_zoom`
- The \*2 versions of line geoms no longer adds an `NA` to guides.

## ggforce 0.1.1

CRAN release: 2016-11-28

### New features

- Zoom indicator styling can now be specified separetely for x and y
  zoom using `zoom.x` and `zoom.y` in theme (inherits from `zoom` that
  inherits from `strip.background`)

### Bug fixes

- Fix bug in `facet_wrap_paginate` that threw errors when using it with
  free scales ([\#19](https://github.com/thomasp85/ggforce/issues/19))
- Fixes bug in `facet_zoom` where y-axis would be incorrectly displayed
  when zooming on both axes without splitting the view
  ([\#23](https://github.com/thomasp85/ggforce/issues/23))
- Fixes bug in `facet_zoom` where scale expansion where not taken into
  account when drawing the indicator area
  ([\#22](https://github.com/thomasp85/ggforce/issues/22))
- Fixes a bug in `facet_zoom` that would throw errors with layers not
  containing the column that is zoomed by
  ([\#21](https://github.com/thomasp85/ggforce/issues/21))

## ggforce 0.1.0

CRAN release: 2016-11-22

### Major changes

- `geom_edge_bundle` has been renamed `geom_bspline` and lost the
  tension argument. True edge bundle functionality has been moved to
  `ggraph`

### New features

- `geom_bezier` for drawing quadratic and cubic beziers
- `geom_link` for augmented segment/path drawing
- `geom_sina` as an alternative to `geom_violin` and `geom_beeswarm`
- `scale_[x|y]_unit` for using units vectors
- `facet_[wrap|grid]_paginate` to split facetting into multiple pages
- `facet_zoom` for contextual zooming

## ggforce 0.0.1

### Major changes

- First commit

### New features

- `geom_arc` / `stat_arc` for drawing circle segments
- `geom_edge_bundle` / `stat_edge_bundle` for drawing edge bundles based
  on control points
- `geom_arc_bar` /`stat_arc_bar` / `stat_pie` for drawing arcs and
  wedges with fill
- `geom_circle` / `stat_circle` for drawing circles with radius based on
  coordinate system scale
- `power_trans` for creating power transformations
- `radial_trans` for creating transformation between radial and
  cartesian coordinates
- `trans_reverser` for reversing a trans object
