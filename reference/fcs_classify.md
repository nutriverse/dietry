# Classify Food Consumption Score (FCS)

Classify Food Consumption Score (FCS)

## Usage

``` r
fcs_classify(fcs, cutoff = NULL, add = FALSE, spread = FALSE)
```

## Arguments

- fcs:

  A vector of food consumption scores.

- cutoff:

  A numeric vector of length 2 for the cut-offs to use for classifying
  FCS. Default to NULL in which case standard recommended cut-off values
  for FCS are used.

- add:

  Logical. Should classification be column bound to fcs? Default to
  FALSE.

- spread:

  Logical. Should classification be spread into columns? Default to
  FALSE.

## Value

If `spread = TRUE`, a data.frame with number of rows equal to the length
of `fcs` and number of columns equal to length of `fill` plus an initial
column named `fcs` containing the FCS values provided by `fcs` argument
if `add = TRUE`. Otherwise, a vector of class factor containing FCS
classifications. If `add = TRUE`, this vector is concatenated with the
`fcs` values in a data.frame.

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

fcs <- fcs_calculate(df = fcs01, var_map = var_map)

fcs_classify(fcs$fcs)
#>  [1] acceptable acceptable acceptable acceptable borderline acceptable
#>  [7] borderline acceptable borderline acceptable acceptable acceptable
#> [13] acceptable acceptable acceptable acceptable acceptable acceptable
#> [19] borderline acceptable acceptable acceptable acceptable acceptable
#> [25] <NA>       poor      
#> Levels: poor borderline acceptable
```
