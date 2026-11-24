# Draw horizontal diagonals

A diagonal is a bezier curve where the control points are moved
perpendicularly towards the center in either the x or y direction a
fixed amount. The versions provided here calculates horizontal diagonals
meaning that the x coordinate is moved to achieve the control point. The
`geom_diagonal()` and `stat_diagonal()` functions are simply helpers
that takes care of calculating the position of the control points and
then forwards the actual bezier calculations to
[`geom_bezier()`](https://ggforce.data-imaginist.com/reference/geom_bezier.md).

## Usage

``` r
stat_diagonal(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  n = 100,
  strength = 0.5,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

geom_diagonal(
  mapping = NULL,
  data = NULL,
  stat = "diagonal",
  position = "identity",
  n = 100,
  na.rm = FALSE,
  orientation = NA,
  strength = 0.5,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

stat_diagonal2(
  mapping = NULL,
  data = NULL,
  geom = "path_interpolate",
  position = "identity",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  n = 100,
  strength = 0.5,
  inherit.aes = TRUE,
  ...
)

geom_diagonal2(
  mapping = NULL,
  data = NULL,
  stat = "diagonal2",
  position = "identity",
  arrow = NULL,
  lineend = "butt",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  n = 100,
  strength = 0.5,
  ...
)

stat_diagonal0(
  mapping = NULL,
  data = NULL,
  geom = "bezier0",
  position = "identity",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  strength = 0.5,
  ...
)

geom_diagonal0(
  mapping = NULL,
  data = NULL,
  stat = "diagonal0",
  position = "identity",
  arrow = NULL,
  lineend = "butt",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  strength = 0.5,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- n:

  The number of points to create for each segment

- strength:

  The proportion to move the control point along the x-axis towards the
  other end of the bezier curve

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- orientation:

  The orientation of the layer. The default (`NA`) automatically
  determines the orientation from the aesthetic mapping. In the rare
  event that this fails it can be given explicitly by setting
  `orientation` to either `"x"` or `"y"`. See the *Orientation* section
  for more detail.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- arrow:

  Arrow specification, as created by
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

- lineend:

  Line end style (round, butt, square).

## Aesthetics

geom_diagonal and geom_diagonal0 understand the following aesthetics
(required aesthetics are in bold):

- **x**

- **y**

- **xend**

- **yend**

- color

- linewidth

- linetype

- alpha

- lineend

geom_diagonal2 understand the following aesthetics (required aesthetics
are in bold):

- **x**

- **y**

- **group**

- color

- linewidth

- linetype

- alpha

- lineend

## Computed variables

- x, y:

  The interpolated point coordinates

- index:

  The progression along the interpolation mapped between 0 and 1

## Orientation

This geom treats each axis differently and, thus, can thus have two
orientations. Often the orientation is easy to deduce from a combination
of the given mappings and the types of positional scales in use. Thus,
ggplot2 will by default try to guess which orientation the layer should
have. Under rare circumstances, the orientation is ambiguous and
guessing may fail. In that case the orientation can be specified
directly using the `orientation` parameter, which can be either `"x"` or
`"y"`. The value gives the axis that the geom should run along, `"x"`
being the default orientation you would expect for the geom.

## Examples

``` r
data <- data.frame(
  x = rep(0, 10),
  y = 1:10,
  xend = 1:10,
  yend = 2:11
)

ggplot(data) +
  geom_diagonal(aes(x, y, xend = xend, yend = yend))


# The standard version provides an index to create gradients
ggplot(data) +
  geom_diagonal(aes(x, y, xend = xend, yend = yend, alpha = after_stat(index)))


# The 0 version uses bezierGrob under the hood for an approximation
ggplot(data) +
  geom_diagonal0(aes(x, y, xend = xend, yend = yend))


# The 2 version allows you to interpolate between endpoint aesthetics
data2 <- data.frame(
  x = c(data$x, data$xend),
  y = c(data$y, data$yend),
  group = rep(1:10, 2),
  colour = sample(letters[1:5], 20, TRUE)
)
ggplot(data2) +
  geom_diagonal2(aes(x, y, group = group, colour = colour))


# Use strength to control the steepness of the central region
ggplot(data, aes(x, y, xend = xend, yend = yend)) +
  geom_diagonal(strength = 0.75, colour = 'red') +
  geom_diagonal(strength = 0.25, colour = 'blue')

```
