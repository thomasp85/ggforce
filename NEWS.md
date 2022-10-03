# ggforce 0.4.1

* Fixed a sanitizer error in the decent calculations used for the mark geoms
* Fixed a typo bug in the vctrs implementations

# ggforce 0.4.0

* Moved to vctrs internally
* Updated error messaging to use cli
* `geom_diagonal()`, `geom_diagonal_wide()`, `geom_parallel_sets()`, and 
  `geom_sina()` are now bidirectional in the style of ggplot2
* `geom_mark_*()` now works correctly in the presence of `NA` values in `x` and 
  `y`
* The `zoom`, `zoom.x`, and `zoom.y` elements now uses proper registration of
  theme elements, inheriting from `strip.background`.
* Fixed bug in `geom_sina()` when groups contained less than 3 points
* Fixed bug in `geom_parallel_sets()` that erroneously removed grouping 
  information in some configurations
* Added `jitter_y` argument to `geom_sina()` to control whether y jittering is 
  performed on integerish y values.
* `facet_zoom()` now works with patchwork
* Fix bug in `geom_mark_ellipse()` that caused wrong orientation of ellipses 
  with groups of two.
* `gather_set_data()` now supports tidyselect.
* `position_jitternormal()` gains a `seed` argument in parallel to 
  `ggplot2::position_jitter()`
* `geom_sina()` now works when the group only have a single unique y value
* `facet_zoom()` now works correctly with transformed scales
* `facet_wrap_paginate()` now works correctly with `dir = 'v'`
* `facet_matrix()` now supports a labeller
* fix a bug in `geom_parallel_sets_axes()` that prevented coloring of axis fill
* fix a bug in `stat_circle()` if the `x` and `y` aesthetics were inherited from 
  the global mapping
* `facet_zoom()` now works even when limits are set by the scales

# ggforce 0.3.4

- Changed documentation to comply with new units package
- Fixed unintentional re-ordering of shapes (#224)
- Deprecate `scale_[x|y]_unit` in favor of `units::scale_[x|y]_units`

# ggforce 0.3.2

- Changes to comply with latest ggplot2 release
- Make sure ggforce pass test even if concaveman is not available

# ggforce 0.3.1

- Better fix for gganimate compatibility (#157)

# ggforce 0.3.0

- Added `facet_matrix()` in order to facet different data columns into different
  rows and columns in a grid, in order to make e.g. scatterplot matrices and 
  pairs plot
- Added `geom_autopoint()` and `position_auto()` to jitter points based on the
  type of positional scales in the panel
- Added `geom_autohistogram()` and `geom_autodensity()` for facilitating 
  distribution plots along the diagonal in a `facet_matrix()` plot.
- Added `facet_row()` and `facet_col` to have one-dimensional `facet_wrap()` 
  with possibility of variable sizing.
- Stats should now always keep the old group variable (potentially with 
  something added), making them work with gganimate
- Removed the *Visual Guide* vignette to reduce compilation time. See the
  website for an overview of all functionality with compiled examples 
  (https://ggforce.data-imaginist.com)

# ggforce 0.2.2

- Fixed a regression in `geom_sina()` where the computation would fail with a
  warning due to `tolower()` being masked (#134, #135).

# ggforce 0.2.1

- Fixed a bug in the calculation of open and closed b-splines, where the
  interval would exceed the defined region and result in an out-of-bounds memory
  error

# ggforce 0.2.0

## New features
- `linear_trans` for composing linear transformation using `rotate`, `stretch`, 
  `shear`, `reflect`, and `translate`
- `facet_stereo` added for creating stereographic projections
- `geom_voronoi_[tile|segment]`, `geom_delaunay_[tile|segment|segment2]`, and
  `stat_delvor_summary` has been added for tesselation and triangulation.
- `geom_spiro` has been added for drawing spirographs
- Add `geom_ellipse` for drawing regular and superellipses
- Add `geom_regon` for drawing regular polygons
- Add `geom_diagonal`, `geom_diagonal_wide` and `geom_parallel_sets` for drawing
  parallel sets diagrams and other visualizations based on diagonals.
- Add `geom_shape` for drawing polygons with rounded corners and 
  expanded/contracted sides. `geom_shape` replaces all `geom_polygon` 
  internally.
- Added `geom_bspline_closed` to draw polygons defined as b-splines
- Add `geom_mark_[rect|circle|ellipse|hull]` to encircle a group of points and
  optionally add textual annotation to it
- Add `position_jitternormal` to jitter points based on a normal distribution
  (@andrewheiss)

## Improvements
- `facet_[wrap|grid]_paginate` will now try to make panels on the last page the
  same size as on full pages (#7)
- `facet_zoom` now gains `xlim` and `ylim` arguments to control zoom range 
  directly
- `facet_zoom` now gains `zoom.data` to control which data gets plotted in which 
  panel
- Slimmed down the dependencies for the package. `plyr`, `lazyeval` and `dplyr`
  has all been removed
- Rewrite `geom_sina` to match `geom_violin` and allow for dodging
- Add `open`/`clamped` option to `geom_bspline

## Bug fixes
- Fix interpolation of `x` and `y` values in `geom_link2` (@thomasp85 and 
  @lepennec)
- `stat_link` no longer replicates the group column
- arcs and links no longer rename aesthetics when only one aesthetic is present
  (`drop = FALSE`)
- `stat_bezier0` and `stat_bezier2` now return data in the expected format
- Fix bug with `n_pages` due to internal changes in ggplot2
- Fix bug in `facet_zoom` in combination with secondary y-axis where the space for 
  the y-axis would become huge
- Correctly detect and error out when scales and coords does not work with 
  `facet_zoom`
- The *2 versions of line geoms no longer adds an `NA` to guides.

# ggforce 0.1.1

## New features
- Zoom indicator styling can now be specified separetely for x and y zoom using 
`zoom.x` and `zoom.y` in theme (inherits from `zoom` that inherits from 
`strip.background`)

## Bug fixes
- Fix bug in `facet_wrap_paginate` that threw errors when using it with free 
scales (#19)
- Fixes bug in `facet_zoom` where y-axis would be incorrectly displayed when 
zooming on both axes without splitting the view (#23)
- Fixes bug in `facet_zoom` where scale expansion where not taken into account
when drawing the indicator area (#22)
- Fixes a bug in `facet_zoom` that would throw errors with layers not containing
the column that is zoomed by (#21)

# ggforce 0.1.0

## Major changes
- `geom_edge_bundle` has been renamed `geom_bspline` and lost the tension 
argument. True edge bundle functionality has been moved to `ggraph`

## New features
- `geom_bezier` for drawing quadratic and cubic beziers
- `geom_link` for augmented segment/path drawing
- `geom_sina` as an alternative to `geom_violin` and `geom_beeswarm`
- `scale_[x|y]_unit` for using units vectors
- `facet_[wrap|grid]_paginate` to split facetting into multiple pages
- `facet_zoom` for contextual zooming

# ggforce 0.0.1

## Major changes
- First commit

## New features
- `geom_arc` / `stat_arc` for drawing circle segments
- `geom_edge_bundle` / `stat_edge_bundle` for drawing edge bundles based on 
control points
- `geom_arc_bar` /`stat_arc_bar` / `stat_pie` for drawing arcs and wedges with 
fill
- `geom_circle` / `stat_circle` for drawing circles with radius based on 
coordinate system scale
- `power_trans` for creating power transformations
- `radial_trans` for creating transformation between radial and cartesian 
coordinates
- `trans_reverser` for reversing a trans object
