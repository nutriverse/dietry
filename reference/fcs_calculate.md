# Calculate Food Consumption Score (FCS)

Calculate Food Consumption Score (FCS)

## Usage

``` r
fcs_calculate(df, var_map, weights = NULL, add = TRUE)
```

## Arguments

- df:

  A data.frame with FCS data.

- var_map:

  A named list of FCS food groups mapped to corresponding variable names
  in `df`. This can be produced using
  [`fcs_fg_map_variables()`](https://nutriverse.io/dietry/reference/map_variables.md).

- weights:

  A numeric vector of FCS weights applied to corresponding food groups.
  The weights should be ordered as that for staples, pulses, vegetables,
  fruits, meat and fish, dairy, sugar, oil, and condiments. Default to
  NULL which uses the weights based on current FCS recommendations. Only
  change this if new recommendations have been provided or for
  testing/studying new/experimental FCS weighting systems.

- add:

  Logical. Should the resulting FCS scores be added to `df`? Default to
  TRUE.

## Value

If `add = TRUE`, a data.frame based on `df` with a new variable named
`fcs` for the calculated food consumption scores. Otherwise, a numeric
vector of the calculated food consumption scores.

## Author

Ernest Guevarra

## Examples

``` r
var_map <- fcs_fg_map_variables(
  staples = "FCSStap",
  pulses = "FCSPulse",
  vegetables = "FCSVeg",
  fruits = "FCSFruit",
  meatfish = "FCSPr",
  milk = "FCSDairy",
  sugar = "FCSSugar",
  oil = "FCSFat",
  condiment = "FCSCond"
) 

fcs_calculate(df = fcs01, var_map = var_map)
#>    FCSStap FCSPulse FCSVeg FCSFruit FCSPr FCSDairy FCSSugar FCSFat FCSCond  fcs
#> 1        7        4      5        3     2        1        6      0       2 49.0
#> 2        1        2      2        3     1        5        4      2       6 40.0
#> 3        7        4      4        2     2        1        2      0       7 45.0
#> 4        2        5      4        7     7        0        5      0       0 60.5
#> 5        1        1      1        3     0        4        7      5       0 31.0
#> 6        2        1      2        3     5        4        4      7       0 53.5
#> 7        3        0      4        6     2        2        0      0       2 32.0
#> 8        6        4      1        2     3        2        0      4       0 49.0
#> 9        5        0      1        2     0        4        3      6       4 33.5
#> 10       3        6      1        2     1        5        4      4       0 55.0
#> 11       4        0      4        5     7        3        6      1       5 60.5
#> 12       0        0      7        7     5        5        7      5       1 60.0
#> 13       1        3      4        6     7        6        1      7       1 77.0
#> 14       0        3      5        3     0        4        4      5       4 37.5
#> 15       4        5      5        7     5        3        7      0       2 70.5
#> 16       0        5      4        6     1        6        1      1       5 54.0
#> 17       3        6      6        7     7        3        6      0       1 80.0
#> 18       0        6      3        1     3        0        3      6       2 38.5
#> 19       0        3      3        5     3        0        5      7       1 35.0
#> 20       4        5      2        7     5        3        4      7       7 69.5
#> 21       3        6      6        3     5        3        6      0       4 68.0
#> 22       3        6      5        4     1        7        6      5       0 70.5
#> 23       1        2      3        1     3        6        2      3       1 50.5
#> 24       6        2      3        1     5        1        6      7       5 52.5
#> 25       0        0      0        0     0        0        0      0       0  0.0
#> 26       1        1      1        1     1        1        1      1       1 16.0
```
