# ggforce 0.1.1.99
- `facet_[wrap|grid]_paginate` will now try to make panels on the last page the
same size as on full pages (#7)
- `facet_zoom` now gains `xlim` and `ylim` arguments to control zoom range 
directly
- `facet_zoom` now gains `zoom.data` to control which data gets plotted in which 
panel

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
