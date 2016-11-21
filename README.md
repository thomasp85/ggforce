# ggforce
*Accelerating ggplot2*

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/ggforce.svg?branch=master)](https://travis-ci.org/thomasp85/ggforce)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/ggforce?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/ggforce)

### About
ggforce is a package aimed at providing missing functionality to ggplot2 through
the extension system introduced with ggplot2 v2.0.0. In general ggplot2 has been
aimed primarily at ad hoc data visualization in order to investigate the data at 
hand, and less at utilities for composing custom plots a la D3.js. ggforce is 
mainly an attempt to address these "shortcomming" (design choices might be a 
better description). The long term goal is to have a repository of geoms, stats
etc that are as well documented and implemented as the official ones found in 
ggplot2.

#### Disclaimer
The inclusion of any geom, stat, position etc in ggforce is not necessarily a 
recommendation of their use. ggplot2 has been succesfull in being opinionated
about what functionality should be available. This is good as it insulates the
user from making bad decisions when analyzing their data, but it also makes it
difficult to develop novel visualizations using the ggplot2 API. ggforce on the
other hand positions itself closer to the "anything goes - the user is 
responsible for the quality of the output". Despite this keep in mind that some
things are always bad choices in data visualization: rainbow color scales, pie 
charts, overplotting etc. Don't do these things except with very good reasons.

### Installation
Until more features are available the package will remain solely on Github.
Once I believe it offers a large enough set of extensions it will be submitted
to CRAN. Installation of ggforce is currently as follows:

```r
if (!require(devtools)) {
    install.packages('devtools')
}
devtools::install_github('thomasp85/ggforce')
```

### Current extensions
Following is a list of the functionality currently offered through ggforce

#### Geoms
- ***geom_arc:*** Drawing of circle segments
- ***geom_arc_bar:*** Drawing thick arcs and wedges
- ***geom_circle:*** Drawing circles with radius relative to coordinate scales
- ***geom_edge_bundle:*** Drawing of edge bundles from control points

#### Stats
- ***stat_arc:*** Companion to geom_arc
- ***stat_arc_bar:*** Generate arc/wedge polygons based on center, radius start
and end angle
- ***stat_pie:*** Generate arc/wedge polygons based on center, radius and value
- ***stat_circle:*** Generate circle polygon based on center and radius
- ***stat_edge_bundle:*** Generate coordinates based on control points for use 
with geom_edge_bundle

#### Transformations
- ***power_trans:*** Create power transformations - somehow missing from scales
- ***trans_reverser:*** Reverse any transformation
- ***radial_trans:*** Transform between radial and cartesian coordinates

#### Themes
- ***theme_no_axes:*** A theme_bw() derived theme that removes axes and grids
from the plot

### Pending extensions
- scale_direction for symbolizing segment/path/line direction using a 
color/alpha gradient without occupying the color scale
- geom_spline for drawing xplines (low priority as already available through 
[ggalt](https://github.com/hrbrmstr/ggalt))
- geom_d for drawing complex shapes in the manner of svg's d specification
- geom_chord for drawing chords (connections between circle segments)
- geom_bezier for drawing curves defined by start, end and two control points
- geom_pie_point for drawing scatterplots based on small pie charts
- geom_wordcloud for drawing wordclouds
- geom_axis for drawing axes in the plotting region
- geom_radial_axis as above but circular
- linear_trans for doing linear coordinate transformations (rotation, shearing,
reflection...)
- position_steam for drawing steamgraphs
- stat_arc_stacked for automatically calculating data for pie/donut charts
- stat_path_dir and stat_segment_dir for calculating path and segments 
compatible with scale_direction

### Contributions
Pull and feature requests are very welcome. Obviously PR's will lead to faster
implementations than feature requests. I would like to urge requests to include
the following if possible:

#### Pull requests
If a PR is for a new feature, it should be self contained, possibly using 
already implemented functionality if applicable. All exported functions should
be documented following the style from ggplot2 using roxygen2 comment. You can
credit yourself with the implementation in the documentation. If the feature 
concerns a visualization appraoch invented by others, please link to the article
describing the approach.

#### Feature requests
If a feature is wished, but skill, time or other is lacking to create a full PR,
please file an issue. The feature request should provide a detailed description
of the nature of the feature, with links to relevant litterature describing the
visualization type, as well as possible use cases to guide in designing the use
cases.

### Related projects
ggforce isn't nor should be the only package providing additional ggplot2 
functionality. Following is a list in no particular order of kindred packages 
that I know of:

- [***ggplus:***](https://github.com/guiastrennec/ggplus) Based on pre-v2.0.0 
ggplot2 some/most of this package might be unusable at this point. Only 
available on Github.
- [***ggalt:***](https://github.com/hrbrmstr/ggalt) Similar in scope to ggforce.
Also available on 
[CRAN](https://cran.r-project.org/web/packages/ggalt/index.html)
- [***ggfortify:***](https://github.com/sinhrks/ggfortify) Defines ggplot2
interfaces to common R packages using fortify and autoplot. Also available on 
[CRAN](https://cran.r-project.org/web/packages/ggfortify/index.html)
- [***ggrepel:***](https://github.com/slowkow/ggrepel) Defines a
nifty geom_text_repel that forces text to move away from their origin to avoid
overlaps. Available on 
[CRAN](https://cran.r-project.org/web/packages/ggrepel/index.html)
- [***ggtern:***](https://github.com/nicholasehamilton/ggtern) An extension for
creating terniary plots in ggplot2. Also available on 
[CRAN](https://cran.r-project.org/web/packages/ggtern/index.html)
