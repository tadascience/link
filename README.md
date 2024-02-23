
<!-- README.md is generated from README.Rmd. Please edit that file -->

# link <a href="https://link.tada.science"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/link)](https://CRAN.R-project.org/package=link)
[![R-CMD-check](https://github.com/tadascience/link/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tadascience/link/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of link is to help blog authors include links to their prose.

## Installation

You can install the development version of link like so:

``` r
pak::pak("tadascience/link")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
link::to(package = "admiral")
#> <a href="https://pharmaverse.github.io/admiral/">admiral</a>

link::to("init", package = "teal")
#> Registered S3 method overwritten by 'teal':
#>   method        from      
#>   c.teal_slices teal.slice
#> <a href="https://insightsengineering.github.io/teal/latest-tag/reference/init.html">teal::init()</a>
link::to(teal::init)
#> <a href="https://insightsengineering.github.io/teal/latest-tag/reference/init.html">teal::init()</a>
```
