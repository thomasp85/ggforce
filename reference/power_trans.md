# Create a power transformation object

This function can be used to create a proper trans object that
encapsulates a power transformation (x^n).

## Usage

``` r
power_trans(n)
```

## Arguments

- n:

  The degree of the power transformation

## Value

A trans object

## Examples

``` r
# Power of 2 transformations
trans <- power_trans(2)
trans$transform(1:10)
#>  [1]   1   4   9  16  25  36  49  64  81 100

# Cubic root transformation
trans <- power_trans(1 / 3)
trans$transform(1:10)
#>  [1] 1.000000 1.259921 1.442250 1.587401 1.709976 1.817121 1.912931 2.000000
#>  [9] 2.080084 2.154435

# Use it in a plot
ggplot() +
  geom_line(aes(x = 1:10, y = 1:10)) +
  scale_x_continuous(trans = power_trans(2),
                     expand = c(0, 1))
```
