
<!-- README.md is generated from README.Rmd. Please edit that file -->

# link

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/link)](https://CRAN.R-project.org/package=link)
<!-- badges: end -->

The goal of link is to …

## Installation

You can install the development version of link like so:

``` r
pak::pak("tadascience/link")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
link::to(dplyr::mutate)
#> <a href="https://dplyr.tidyverse.org/reference/mutate.html">dplyr::mutate()</a>
link::to(package = "dplyr")
#> <a href="https://dplyr.tidyverse.org">dplyr</a>
link::to("gather", package = "tidyr")
#> <a href="https://tidyr.tidyverse.org/reference/gather.html">tidyr::gather()</a>
```
