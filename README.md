
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pdftextclusteR <img src="man/figures/logo.png" align="right" height="132"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The \[pdftools\] package is available for importing PDF files. However,
this package does not work optimally when importing PDF files with
multiple columns and text boxes. This package uses a Density-Based
Spatial Clustering algorithm to determine which words are (likely)
associated with each other.

## Installation

You can install the current version of pdftextclusteR using the
following code.

``` r
devtools::install_github("coeneisma/pdftextclusteR")
```

The latest version can be found on the development branch. You can
install it using the function:

``` r
devtools::install_github("coeneisma/pdftextclusteR")
  ref = "development")
```
