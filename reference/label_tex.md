# A labeller function to parse TeX syntax

This function formats the strip labels of facet grids and wraps that
contains TeX expressions. The latex2exp package must be installed.

## Usage

``` r
label_tex(labels, ...)
```

## Arguments

- labels:

  Data frame of labels. Usually contains only one element, but faceting
  over multiple factors entails multiple label variables.

- ...:

  Arguments passed on to
  [`ggplot2::label_parsed`](https://ggplot2.tidyverse.org/reference/labellers.html)

  `multi_line`

  :   Whether to display the labels of multiple factors on separate
      lines.

  `sep`

  :   String separating variables and values.

  `width`

  :   Maximum number of characters before wrapping the strip.

## See also

[ggplot2::labeller](https://ggplot2.tidyverse.org/reference/labeller.html),
[`latex2exp::TeX()`](https://www.stefanom.io/latex2exp/reference/TeX.html)

## Examples

``` r
# requires latex2exp package be installed
if (requireNamespace("latex2exp", quietly = TRUE)) {
  library(ggplot2)
  d <- data.frame(x = 1, y = 1, facet = "$\\beta$")
  ggplot(d, aes(x, y)) +
    geom_point() +
    facet_wrap(~ facet, labeller = label_tex)
}
```
