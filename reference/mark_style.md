# A marquee styleset for geom_mark\_\*()

This function constructs stylesets with defaults appropriate for the
text boxes in the geom_mark\_\*() family of geoms.

## Usage

``` r
mark_style(
  base_size = 10,
  background = "white",
  color = "black",
  header_color = color,
  lineheight = 1,
  margin = marquee::trbl(grid::unit(2, "mm")),
  ...
)
```

## Arguments

- base_size:

  The base size for the text (depending on the style, some text may be
  larger, e.g. headings)

- background:

  The background color of the textbox

- color:

  The text color in the textbox

- header_color:

  The text color for headings in the textbox

- lineheight:

  The base lineheight for the text in the textbox

- margin:

  The margin around the text in the textbox

- ...:

  Further arguments to pass on to
  [`marquee::classic_style()`](https://marquee.r-lib.org/reference/classic_style.html)

## Value

A marquee style_set object with a length corresponding to the length of
the input
