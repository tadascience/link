
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

Include this in your `Rmd` or `qmd` document to turn `<pkg>::<fun>()`
and `{<pkg>}` into hyperlinks.

    ```{r, echo = FALSE}
    link::auto()
    ```

You can drop the `{` and the `pkg::` if you like:

    ```{r, echo = FALSE}
    link::auto(keep_braces = FALSE, keep_pkg_prefix = FALSE)
    ```

By default, [bslib](https://rstudio.github.io/bslib/) powered tooltips
are added. You can opt-out with `type = "plain"`.

    ```{r, echo = FALSE}
    link::auto(type = "plain")
    ```
