# dietry: Utilities for Dietary Intake Indicators for Food Security Assessments

Food security assessments utilise several dietary intake indicators as
proxy measures for diet quality, diet sufficiency, and food availability
either at individual or household level. Utilities for recoding and
calculating these indicators support in establishing consistent and
reliable results.

## What does the package do?

Currently, the [dietry](https://nutriverse.io/dietry/) package has
functions for:

- Cleaning, processing, scoring, and classifying the [Food Consumption
  Score](https://resources.vam.wfp.org/data-analysis/quantitative/food-security/food-consumption-score).

## Installation

The [dietry](https://nutriverse.io/dietry/) package is still in active
development and not yet available on
[CRAN](https://cran.r-project.org/).

You can install the development version of
[dietry](https://nutriverse.io/dietry/) from the [nutriverse R
Universe](https://nutriverse.r-universe.dev) with:

``` r
install.packages(
  "dietry", 
  repos = c('https://nutriverse.r-universe.dev', 'https://cloud.r-project.org')
)
```

## Citation

If you find the [dietry](https://nutriverse.io/dietry/) package useful,
please cite using the suggested citation provided by a call to the
`citation` function as follows:

``` r
citation("dietry")
#> To cite dietry in publications, use:
#> 
#>   Ernest Guevarra (2024). _dietry: Utilities for Dietary Intake
#>   Indicators for Food Security Assessments_. R package version
#>   0.0.0.9001, <https://nutriverse.io/dietry/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {dietry: Utilities for Dietary Intake Indicators for Food Security Assessments},
#>     author = {{Ernest Guevarra}},
#>     year = {2024},
#>     note = {R package version 0.0.0.9001},
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

  

[![This is part of the nutriverse project under the Oxford iHealth
initiative of the MSc in International Health and Tropical Medicine,
Nuffield Department of Medicine, University of
Oxford](https://github.com/nutriverse/nutriverse-images/blob/main/nutriverse/nutriverse_footer.png?raw=true)](https://nutriverse.io)
