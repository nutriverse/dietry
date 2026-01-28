# Recode Food Consumption Score (FCS) data

Recode Food Consumption Score (FCS) data

## Usage

``` r
fcs_recode(x, na_values = NULL)
```

## Arguments

- x:

  A vector of numeric values that can range from 0 to 7 for the number
  of days in a week that a food group is eaten by a household as per FCS
  guidelines.

- na_values:

  A value or a vector of values that are to be considered as NA. Default
  to NA.

## Value

An integer vector with possible values ranging from 0 to 7.

## Author

Ernest Guevarra

## Examples

``` r
fcs_recode(fcs01$FCSStap)
#>  [1] 7 1 7 2 1 2 3 6 5 3 4 0 1 0 4 0 3 0 0 4 3 3 1 6 0 1
```
