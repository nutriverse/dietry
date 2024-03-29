
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dietry: Utilities for Calculating Dietary Intake Indicators for Food Security Assessments

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/nutriverse/dietry/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nutriverse/dietry/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nutriverse/dietry/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nutriverse/dietry?branch=main)
<!-- badges: end -->

Food security assessments utilise several dietary intake indicators as
proxy measures for diet quality, diet sufficiency, and food availability
either at individual or household level. Utilities for recoding and
calculating these indicators support in establishing consistent and
reliable results.

## Installation

The `dietry` package is still in active development and not yet
available on [CRAN](https://cran.r-project.org/).

You can install the development version of dietry from
[GitHub](https://github.com/) with:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("nutriverse/dietry")
```

## What does `dietry` do?

## Citation

If you find the `dietry` package useful, please cite using the suggested
citation provided by a call to the `citation` function as follows:

``` r
citation("dietry")
#> To cite dietry in publications use:
#> 
#>   Ernest Guevarra (2023). dietry: Utilities for Calculating Dietary
#>   Intake Indicators for Food Security Assessments R package version
#>   0.0.0.9000 URL https://nutriverse.io/dietry/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {dietry: Utilities for Calculating Dietary Intake Indicators for Food Security
#> Assessments},
#>     author = {{Ernest Guevarra}},
#>     year = {2023},
#>     note = {R package version 0.0.0.9000},
#>     url = {https://nutriverse.io/dietry/},
#>   }
```

## Community guidelines

Feedback, bug reports, and feature requests are welcome; file issues or
seek support [here](https://github.com/nutriverse/dietry/issues). If you
would like to contribute to the package, please see our [contributing
guidelines](https://nutriverse.io/dietry/CONTRIBUTING.html).

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
