# Convert character vector of categorical responses into unique variables

Function transforms a vector of categorical responses into `n` number of
new columns/variables equal to the number of unique categorical values.

## Usage

``` r
spread_vector_to_columns(x, fill = NULL, na_rm = FALSE, prefix)
```

## Arguments

- x:

  Vector of categorical values.

- fill:

  Vector of all possible unique categorical values for `x`.

- na_rm:

  Logical. Should NA values in `x` be included as a category? Default to
  FALSE.

- prefix:

  A character string to prepend to the names of the new columns to be
  created

## Value

A data.frame with number of rows equal to the length of `x`. If `fill`
is not NULL, number of columns is equal to length of `fill` plus one if
`na_rm = TRUE`. Otherwise, number of columns is equal to length of
unique categorical values in `x` plus one if `na_rm = TRUE`. Variable
names of output data.frame is a concatenation of the `prefix` and the
unique categorical values in `x` or the values in `fill` if
`fill = TRUE`.
