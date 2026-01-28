# Map data variables to corresponding indicators

Map data variables to corresponding indicators

## Usage

``` r
map_variables(...)

fcs_fg_map_variables(
  ...,
  foodgroups = c("staples", "pulses", "vegetables", "fruits", "meatfish", "milk",
    "sugar", "oil", "condiment")
)

hdds_fg_map_variables(
  ...,
  foodgroups = c("cereals", "roots_tubers", "vegetables", "fruits", "meat", "eggs",
    "fish", "pulses", "milk", "oil", "sugar", "condiments")
)

mddw_fg_map_variables(
  ...,
  foodgroups = c("staples", "pulses", "nuts", "milk", "meat_fish", "eggs", "green_leafy",
    "other_vita", "other_vegetables", "other_fruits")
)
```

## Arguments

- ...:

  Name-value pairs. Name gives the labels for indicators. The value
  should be the corresponding variable name in a dataset used for that
  indicator.

- foodgroups:

  A character vector of food group labels for a specific dietary intake
  indicator set.

## Value

A named list of variable name/s for corresponding food groups.

## Examples

``` r
## Variable names in fcs01 mapped to corresponding food group labels
map_variables(
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
#> $staples
#> [1] "FCSStap"
#> 
#> $pulses
#> [1] "FCSPulse"
#> 
#> $vegetables
#> [1] "FCSVeg"
#> 
#> $fruits
#> [1] "FCSFruit"
#> 
#> $meatfish
#> [1] "FCSPr"
#> 
#> $milk
#> [1] "FCSDairy"
#> 
#> $sugar
#> [1] "FCSSugar"
#> 
#> $oil
#> [1] "FCSFat"
#> 
#> $condiment
#> [1] "FCSCond"
#> 
```
