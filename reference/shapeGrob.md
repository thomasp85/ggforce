# The grob powering geom_shape

This is the underlying grob constructor for
[`geom_shape()`](https://ggforce.data-imaginist.com/reference/geom_shape.md).
It is exported for others to use but with limited support

## Usage

``` r
shapeGrob(
  x = c(0, 0.5, 1, 0.5),
  y = c(0.5, 1, 0.5, 0),
  id = NULL,
  id.lengths = NULL,
  expand = 0,
  radius = 0,
  default.units = "npc",
  name = NULL,
  gp = gpar(),
  vp = NULL
)
```

## Arguments

- x:

  A numeric vector or unit object specifying x-locations.

- y:

  A numeric vector or unit object specifying y-locations.

- id:

  A numeric vector used to separate locations in `x` and `y` into
  multiple polygons. All locations with the same `id` belong to the same
  polygon.

- id.lengths:

  A numeric vector used to separate locations in `x` and `y` into
  multiple polygons. Specifies consecutive blocks of locations which
  make up separate polygons.

- expand:

  An expansion size to expand each shape with, given in units or a
  numeric refering to `default.units`

- radius:

  The corner radius to apply to each shape, given in units or a numeric
  refering to `default.units`

- default.units:

  A string indicating the default units to use if `x`, `y`, `width`, or
  `height` are only given as numeric vectors.

- name:

  A character identifier.

- gp:

  An object of class `"gpar"`, typically the output from a call to the
  function [`gpar`](https://rdrr.io/r/grid/gpar.html). This is basically
  a list of graphical parameter settings.

- vp:

  A Grid viewport object (or NULL).

## Value

A grob of class `shape` or, of `expand` and `radius` are `0` a regular
polygon grob
