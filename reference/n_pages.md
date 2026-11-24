# Determine the number of pages in a paginated facet plot

This is a simple helper that returns the number of pages it takes to
plot all panels when using
[`facet_wrap_paginate()`](https://ggforce.data-imaginist.com/reference/facet_wrap_paginate.md)
and
[`facet_grid_paginate()`](https://ggforce.data-imaginist.com/reference/facet_grid_paginate.md).
It partially builds the plot so depending on the complexity of your plot
it might take some time to calculate...

## Usage

``` r
n_pages(plot)
```

## Arguments

- plot:

  A ggplot object using either facet_wrap_paginate or
  facet_grid_paginate

## Value

If the plot uses using either facet_wrap_paginate or facet_grid_paginate
it returns the total number of pages. Otherwise it returns NULL

## Examples

``` r
p <- ggplot(diamonds) +
  geom_point(aes(carat, price), alpha = 0.1) +
  facet_wrap_paginate(~ cut:clarity, ncol = 3, nrow = 3, page = 1)
n_pages(p)
#> [1] 5
```
