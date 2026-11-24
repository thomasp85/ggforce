# Theme without axes and gridlines

This theme is a simple wrapper around any complete theme that removes
the axis text, title and ticks as well as the grid lines for plots where
these have little meaning.

## Usage

``` r
theme_no_axes(base.theme = theme_bw())
```

## Arguments

- base.theme:

  The theme to use as a base for the new theme. Defaults to
  [`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Value

A modified version of base.theme

## Examples

``` r
p <- ggplot() + geom_point(aes(x = wt, y = qsec), data = mtcars)

p + theme_no_axes()

p + theme_no_axes(theme_grey())

```
