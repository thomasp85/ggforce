# Tidy data for use with geom_parallel_sets

This helper function makes it easy to change tidy data into a tidy(er)
format that can be used by geom_parallel_sets.

## Usage

``` r
gather_set_data(data, x, id_name = "id")
```

## Arguments

- data:

  A tidy dataframe with some categorical columns

- x:

  The columns to use for axes in the parallel sets diagram

- id_name:

  The name of the column that will contain the original index of the
  row.

## Value

A data.frame

## Examples

``` r
data <- reshape2::melt(Titanic)
head(gather_set_data(data, 1:4))
#>   Class    Sex   Age Survived value id     x    y
#> 1   1st   Male Child       No     0  1 Class  1st
#> 2   2nd   Male Child       No     0  2 Class  2nd
#> 3   3rd   Male Child       No    35  3 Class  3rd
#> 4  Crew   Male Child       No     0  4 Class Crew
#> 5   1st Female Child       No     0  5 Class  1st
#> 6   2nd Female Child       No     0  6 Class  2nd
head(gather_set_data(data, c("Class","Sex","Age","Survived")))
#>   Class    Sex   Age Survived value id     x    y
#> 1   1st   Male Child       No     0  1 Class  1st
#> 2   2nd   Male Child       No     0  2 Class  2nd
#> 3   3rd   Male Child       No    35  3 Class  3rd
#> 4  Crew   Male Child       No     0  4 Class Crew
#> 5   1st Female Child       No     0  5 Class  1st
#> 6   2nd Female Child       No     0  6 Class  2nd
```
