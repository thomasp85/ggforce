# Package index

## Shapes

Shapes are, in essence, anything with volume. These geoms allow you to
draw differnt types of parameterised shapes, all taking advantage of the
benefit of the `geom_shape` improvements to `geom_polygon`.

- [`geom_shape()`](https://ggforce.data-imaginist.com/reference/geom_shape.md)
  : Draw polygons with expansion/contraction and/or rounded corners
- [`stat_circle()`](https://ggforce.data-imaginist.com/reference/geom_circle.md)
  [`geom_circle()`](https://ggforce.data-imaginist.com/reference/geom_circle.md)
  : Circles based on center and radius
- [`stat_ellip()`](https://ggforce.data-imaginist.com/reference/geom_ellipse.md)
  [`geom_ellipse()`](https://ggforce.data-imaginist.com/reference/geom_ellipse.md)
  : Draw (super)ellipses based on the coordinate system scale
- [`stat_regon()`](https://ggforce.data-imaginist.com/reference/geom_regon.md)
  [`geom_regon()`](https://ggforce.data-imaginist.com/reference/geom_regon.md)
  : Draw regular polygons by specifying number of sides
- [`stat_arc_bar()`](https://ggforce.data-imaginist.com/reference/geom_arc_bar.md)
  [`stat_pie()`](https://ggforce.data-imaginist.com/reference/geom_arc_bar.md)
  [`geom_arc_bar()`](https://ggforce.data-imaginist.com/reference/geom_arc_bar.md)
  : Arcs and wedges as polygons
- [`stat_bspline_closed()`](https://ggforce.data-imaginist.com/reference/geom_bspline_closed.md)
  [`geom_bspline_closed()`](https://ggforce.data-imaginist.com/reference/geom_bspline_closed.md)
  [`geom_bspline_closed0()`](https://ggforce.data-imaginist.com/reference/geom_bspline_closed.md)
  : Create closed b-spline shapes
- [`stat_diagonal_wide()`](https://ggforce.data-imaginist.com/reference/geom_diagonal_wide.md)
  [`geom_diagonal_wide()`](https://ggforce.data-imaginist.com/reference/geom_diagonal_wide.md)
  : Draw an area defined by an upper and lower diagonal
- [`stat_parallel_sets()`](https://ggforce.data-imaginist.com/reference/geom_parallel_sets.md)
  [`geom_parallel_sets()`](https://ggforce.data-imaginist.com/reference/geom_parallel_sets.md)
  [`stat_parallel_sets_axes()`](https://ggforce.data-imaginist.com/reference/geom_parallel_sets.md)
  [`geom_parallel_sets_axes()`](https://ggforce.data-imaginist.com/reference/geom_parallel_sets.md)
  [`geom_parallel_sets_labels()`](https://ggforce.data-imaginist.com/reference/geom_parallel_sets.md)
  : Create Parallel Sets diagrams
- [`geom_voronoi_tile()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`geom_voronoi_segment()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`geom_delaunay_tile()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`geom_delaunay_segment()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`geom_delaunay_segment2()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`stat_delvor_summary()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  : Voronoi tesselation and delaunay triangulation

## Lines

The different line geoms are all parameterised versions of different
line types, greatly easing your pain when needing a special type of
stroke. Many of them have several versions depending on whether you want
to show gradients along the lines, interpolate between endpoint
aesthetics, or simply have a barebone version.

- [`stat_link()`](https://ggforce.data-imaginist.com/reference/geom_link.md)
  [`stat_link2()`](https://ggforce.data-imaginist.com/reference/geom_link.md)
  [`geom_link()`](https://ggforce.data-imaginist.com/reference/geom_link.md)
  [`geom_link2()`](https://ggforce.data-imaginist.com/reference/geom_link.md)
  [`geom_link0()`](https://ggforce.data-imaginist.com/reference/geom_link.md)
  : Link points with paths
- [`stat_arc()`](https://ggforce.data-imaginist.com/reference/geom_arc.md)
  [`geom_arc()`](https://ggforce.data-imaginist.com/reference/geom_arc.md)
  [`stat_arc2()`](https://ggforce.data-imaginist.com/reference/geom_arc.md)
  [`geom_arc2()`](https://ggforce.data-imaginist.com/reference/geom_arc.md)
  [`stat_arc0()`](https://ggforce.data-imaginist.com/reference/geom_arc.md)
  [`geom_arc0()`](https://ggforce.data-imaginist.com/reference/geom_arc.md)
  : Arcs based on radius and radians
- [`stat_bezier()`](https://ggforce.data-imaginist.com/reference/geom_bezier.md)
  [`geom_bezier()`](https://ggforce.data-imaginist.com/reference/geom_bezier.md)
  [`stat_bezier2()`](https://ggforce.data-imaginist.com/reference/geom_bezier.md)
  [`geom_bezier2()`](https://ggforce.data-imaginist.com/reference/geom_bezier.md)
  [`stat_bezier0()`](https://ggforce.data-imaginist.com/reference/geom_bezier.md)
  [`geom_bezier0()`](https://ggforce.data-imaginist.com/reference/geom_bezier.md)
  : Create quadratic or cubic bezier curves
- [`stat_bspline()`](https://ggforce.data-imaginist.com/reference/geom_bspline.md)
  [`geom_bspline()`](https://ggforce.data-imaginist.com/reference/geom_bspline.md)
  [`stat_bspline2()`](https://ggforce.data-imaginist.com/reference/geom_bspline.md)
  [`geom_bspline2()`](https://ggforce.data-imaginist.com/reference/geom_bspline.md)
  [`stat_bspline0()`](https://ggforce.data-imaginist.com/reference/geom_bspline.md)
  [`geom_bspline0()`](https://ggforce.data-imaginist.com/reference/geom_bspline.md)
  : B-splines based on control points
- [`stat_diagonal()`](https://ggforce.data-imaginist.com/reference/geom_diagonal.md)
  [`geom_diagonal()`](https://ggforce.data-imaginist.com/reference/geom_diagonal.md)
  [`stat_diagonal2()`](https://ggforce.data-imaginist.com/reference/geom_diagonal.md)
  [`geom_diagonal2()`](https://ggforce.data-imaginist.com/reference/geom_diagonal.md)
  [`stat_diagonal0()`](https://ggforce.data-imaginist.com/reference/geom_diagonal.md)
  [`geom_diagonal0()`](https://ggforce.data-imaginist.com/reference/geom_diagonal.md)
  : Draw horizontal diagonals
- [`stat_spiro()`](https://ggforce.data-imaginist.com/reference/geom_spiro.md)
  [`geom_spiro()`](https://ggforce.data-imaginist.com/reference/geom_spiro.md)
  : Draw spirograms based on the radii of the different "wheels"
  involved
- [`geom_voronoi_tile()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`geom_voronoi_segment()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`geom_delaunay_tile()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`geom_delaunay_segment()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`geom_delaunay_segment2()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  [`stat_delvor_summary()`](https://ggforce.data-imaginist.com/reference/geom_delvor.md)
  : Voronoi tesselation and delaunay triangulation

## Annotation

Annotation is important for storytelling, and ggforce provides a family
of geoms that makes it easy to draw attention to, and describe, features
of the plot. They all work in the same way, but differ in the way they
enclose the area you want to draw attention to.

- [`geom_mark_rect()`](https://ggforce.data-imaginist.com/reference/geom_mark_rect.md)
  : Annotate areas with rectangles
- [`geom_mark_circle()`](https://ggforce.data-imaginist.com/reference/geom_mark_circle.md)
  : Annotate areas with circles
- [`geom_mark_ellipse()`](https://ggforce.data-imaginist.com/reference/geom_mark_ellipse.md)
  : Annotate areas with ellipses
- [`geom_mark_hull()`](https://ggforce.data-imaginist.com/reference/geom_mark_hull.md)
  : Annotate areas with hulls
- [`mark_style()`](https://ggforce.data-imaginist.com/reference/mark_style.md)
  : A marquee styleset for geom_mark\_\*()

## Facets

Facets are one of the greatest things in ggplot2, and ggforce comes with
more of the awesomeness, both with variants of `facet_grid` and
`facet_wrap`, as well as completely new takes on faceting.

- [`facet_matrix()`](https://ggforce.data-imaginist.com/reference/facet_matrix.md)
  : Facet by different data columns
- [`facet_zoom()`](https://ggforce.data-imaginist.com/reference/facet_zoom.md)
  : Facet data for zoom with context
- [`facet_row()`](https://ggforce.data-imaginist.com/reference/facet_row.md)
  [`facet_col()`](https://ggforce.data-imaginist.com/reference/facet_row.md)
  : One-dimensional facets
- [`facet_wrap_paginate()`](https://ggforce.data-imaginist.com/reference/facet_wrap_paginate.md)
  : Split facet_wrap over multiple plots
- [`facet_grid_paginate()`](https://ggforce.data-imaginist.com/reference/facet_grid_paginate.md)
  : Split facet_grid over multiple plots
- [`facet_stereo()`](https://ggforce.data-imaginist.com/reference/facet_stereo.md)
  : Create a stereogram plot

## Scales

While separate packages comes with different palettes for already
established scales, ggforce provides two completely new ones.

- [`scale_x_unit()`](https://ggforce.data-imaginist.com/reference/scale_unit.md)
  [`scale_y_unit()`](https://ggforce.data-imaginist.com/reference/scale_unit.md)
  **\[deprecated\]** : Position scales for units data
- [`scale_depth()`](https://ggforce.data-imaginist.com/reference/scale_depth.md)
  [`scale_depth_continuous()`](https://ggforce.data-imaginist.com/reference/scale_depth.md)
  [`scale_depth_discrete()`](https://ggforce.data-imaginist.com/reference/scale_depth.md)
  : Scales for depth perception

## Transformations

Transformations can both be used to transform scales and coordinate
systems but can also be used more broadly for describing specific types
of spatial transformation of data.

- [`trans_reverser()`](https://ggforce.data-imaginist.com/reference/trans_reverser.md)
  : Reverse a transformation
- [`power_trans()`](https://ggforce.data-imaginist.com/reference/power_trans.md)
  : Create a power transformation object
- [`radial_trans()`](https://ggforce.data-imaginist.com/reference/radial_trans.md)
  : Create radial data in a cartesian coordinate system
- [`linear_trans()`](https://ggforce.data-imaginist.com/reference/linear_trans.md)
  [`rotate()`](https://ggforce.data-imaginist.com/reference/linear_trans.md)
  [`stretch()`](https://ggforce.data-imaginist.com/reference/linear_trans.md)
  [`shear()`](https://ggforce.data-imaginist.com/reference/linear_trans.md)
  [`translate()`](https://ggforce.data-imaginist.com/reference/linear_trans.md)
  [`reflect()`](https://ggforce.data-imaginist.com/reference/linear_trans.md)
  : Create a custom linear transformation

## Misc

ggforce contains an assortment of various stuff that doesn’t fit into a
bigger bucket. That doesn’t make it any less useful.

- [`stat_sina()`](https://ggforce.data-imaginist.com/reference/geom_sina.md)
  [`geom_sina()`](https://ggforce.data-imaginist.com/reference/geom_sina.md)
  : Sina plot
- [`geom_autopoint()`](https://ggforce.data-imaginist.com/reference/geom_autopoint.md)
  : A point geom specialised for scatterplot matrices
- [`geom_autodensity()`](https://ggforce.data-imaginist.com/reference/geom_autohistogram.md)
  [`geom_autohistogram()`](https://ggforce.data-imaginist.com/reference/geom_autohistogram.md)
  : A distribution geoms that fills the panel and works with discrete
  and continuous data
- [`label_tex()`](https://ggforce.data-imaginist.com/reference/label_tex.md)
  : A labeller function to parse TeX syntax
- [`stat_err()`](https://ggforce.data-imaginist.com/reference/stat_err.md)
  : Intervals in vertical and horizontal directions
- [`position_auto()`](https://ggforce.data-imaginist.com/reference/position_auto.md)
  : Jitter based on scale types
- [`position_jitternormal()`](https://ggforce.data-imaginist.com/reference/position_jitternormal.md)
  : Jitter points with normally distributed random noise
- [`gather_set_data()`](https://ggforce.data-imaginist.com/reference/gather_set_data.md)
  : Tidy data for use with geom_parallel_sets
- [`n_pages()`](https://ggforce.data-imaginist.com/reference/n_pages.md)
  : Determine the number of pages in a paginated facet plot
- [`theme_no_axes()`](https://ggforce.data-imaginist.com/reference/theme_no_axes.md)
  : Theme without axes and gridlines
- [`ggforce`](https://ggforce.data-imaginist.com/reference/ggforce-package.md)
  [`ggforce-package`](https://ggforce.data-imaginist.com/reference/ggforce-package.md)
  : ggforce: Accelerating 'ggplot2'
- [`GeomShape`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatArcBar`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatPie`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomArcBar`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatArc`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomArc`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatArc2`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatArc0`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomArc0`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatAutodensity`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomAutoarea`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatAutobin`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomAutorect`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatBezier`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatBezier2`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatBezier0`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomBezier0`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatBspline`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatBspline2`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomBspline0`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomBsplineClosed0`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatCircle`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomCircle`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatDiagonal`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatDiagonal2`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatDiagonal0`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatDiagonalWide`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatEllip`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatErr`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`FacetGridPaginate`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`FacetMatrix`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`FacetRow`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`FacetCol`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`FacetStereo`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`FacetWrapPaginate`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`FacetZoom`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`ggforce-extensions`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomPathInterpolate`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatLink`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatLink2`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomMarkCircle`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomMarkEllipse`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomMarkHull`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomMarkRect`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatParallelSets`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatParallelSetsAxes`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`GeomParallelSetsAxes`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`PositionJitterNormal`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`PositionAuto`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`PositionFloatstack`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatRegon`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatSina`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatSpiro`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatVoronoiTile`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatVoronoiSegment`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatDelaunayTile`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatDelaunaySegment`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatDelaunaySegment2`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  [`StatDelvorSummary`](https://ggforce.data-imaginist.com/reference/ggforce-extensions.md)
  : ggforce extensions to ggplot2
