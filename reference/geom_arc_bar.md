# Arcs and wedges as polygons

This set of stats and geoms makes it possible to draw arcs and wedges as
known from pie and donut charts as well as more specialized plottypes
such as sunburst plots.

## Usage

``` r
stat_arc_bar(
  mapping = NULL,
  data = NULL,
  geom = "arc_bar",
  position = "identity",
  n = 360,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

stat_pie(
  mapping = NULL,
  data = NULL,
  geom = "arc_bar",
  position = "identity",
  n = 360,
  sep = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

geom_arc_bar(
  mapping = NULL,
  data = NULL,
  stat = "arc_bar",
  position = "identity",
  n = 360,
  expand = 0,
  radius = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
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

  The number of points used to draw a full circle. The number of points
  on each arc will then be calculated as n / span-of-arc

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

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

- sep:

  The separation between arcs in pie/donut charts

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

- expand:

  A numeric or unit vector of length one, specifying the expansion
  amount. Negative values will result in contraction instead. If the
  value is given as a numeric it will be understood as a proportion of
  the plot area width.

- radius:

  As `expand` but specifying the corner radius.

## Details

An arc bar is the thick version of an arc; that is, a circle segment
drawn as a polygon in the same way as a rectangle is a thick version of
a line. A wedge is a special case of an arc where the inner radius is 0.
As opposed to applying coord_polar to a stacked bar chart, these layers
are drawn in cartesian space, which allows for transformations not
possible with the native ggplot2 approach. Most notable of these are the
option to explode arcs and wedgets away from their center point, thus
detaching it from the main pie/donut.

## Aesthetics

geom_arc_bar understand the following aesthetics (required aesthetics
are in bold):

- **x0**

- **y0**

- **r0**

- **r**

- **start** - when using stat_arc_bar

- **end** - when using stat_arc_bar

- **amount** - when using stat_pie

- explode

- color

- fill

- linewidth

- linetype

- alpha

## Computed variables

- x, y:

  x and y coordinates for the polygon

&nbsp;

- x, y:

  The start coordinates for the segment

## See also

[`geom_arc()`](https://ggforce.data-imaginist.com/reference/geom_arc.md)
for drawing arcs as lines

## Examples

``` r
# If you know the angle spans to plot it is easy
arcs <- data.frame(
  start = seq(0, 2 * pi, length.out = 11)[-11],
  end = seq(0, 2 * pi, length.out = 11)[-1],
  r = rep(1:2, 5)
)

# Behold the arcs
ggplot(arcs) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = r - 1, r = r, start = start,
                   end = end, fill = r))


# geom_arc_bar uses geom_shape to draw the arcs, so you have all the
# possibilities of that as well, e.g. rounding of corners
ggplot(arcs) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = r - 1, r = r, start = start,
                   end = end, fill = r), radius = unit(4, 'mm'))


# If you got values for a pie chart, use stat_pie
states <- c(
  'eaten', "eaten but said you didn\'t", 'cat took it', 'for tonight',
  'will decompose slowly'
)
pie <- data.frame(
  state = factor(rep(states, 2), levels = states),
  type = rep(c('Pie', 'Donut'), each = 5),
  r0 = rep(c(0, 0.8), each = 5),
  focus = rep(c(0.2, 0, 0, 0, 0), 2),
  amount = c(4, 3, 1, 1.5, 6, 6, 1, 2, 3, 2)
)

# Look at the cakes
ggplot() + geom_arc_bar(aes(
  x0 = 0, y0 = 0, r0 = r0, r = 1, amount = amount,
  fill = state, explode = focus
),
data = pie, stat = 'pie'
) +
  facet_wrap(~type, ncol = 1) +
  coord_fixed() +
  theme_no_axes() +
  scale_fill_brewer('', type = 'qual')

```
