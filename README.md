
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dietry: Utilities for Dietary Intake Indicators for Food Security Assessments <img src="man/figures/logo.png" width="200" align="right" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/nutriverse/dietry/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nutriverse/dietry/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nutriverse/dietry/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nutriverse/dietry?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/nutriverse/dietry/badge)](https://www.codefactor.io/repository/github/nutriverse/dietry)
[![DOI](https://zenodo.org/badge/477273747.svg)](https://zenodo.org/badge/latestdoi/477273747)
<!-- badges: end -->

Food security assessments utilise several dietary intake indicators as
proxy measures for diet quality, diet sufficiency, and food availability
either at individual or household level. Utilities for recoding and
calculating these indicators support in establishing consistent and
reliable results.

## What does `dietry` do?

Currently, the `dietry` package has functions for:

  - Cleaning, processing, scoring, and classifying the [Food Consumption
    Score](https://resources.vam.wfp.org/data-analysis/quantitative/food-security/food-consumption-score).

## Installation

The `dietry` package is still in active development and not yet
available on [CRAN](https://cran.r-project.org/).

You can install the development version of `dietry` from the [nutriverse
R Universe](https://nutriverse.r-universe.dev) with:

``` r
install.packages(
  "dietry", 
  repos = c('https://nutriverse.r-universe.dev', 'https://cloud.r-project.org')
)
```

## Citation

If you find the `dietry` package useful, please cite using the suggested
citation provided by a call to the `citation` function as follows:

``` r
citation("dietry")
#> To cite dietry in publications, use:
#> 
#>   Ernest Guevarra (2024). _dietry: Utilities for Dietary Intake
#>   Indicators for Food Security Assessments_. R package version
#>   0.0.0.9000, <https://nutriverse.io/dietry/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {dietry: Utilities for Dietary Intake Indicators for Food Security Assessments},
#>     author = {{Ernest Guevarra}},
#>     year = {2024},
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
