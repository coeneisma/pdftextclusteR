
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pdftextclusteR <img src="man/figures/logo.png" align="right" height="132"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/coeneisma/pdftextclusteR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coeneisma/pdftextclusteR/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The [pdftools](https://docs.ropensci.org/pdftools/) package is available
for importing PDF files. However, this package does not work optimally
when importing PDF files with multiple columns and text boxes. Since
the`pdftools::pdf_text()` function from the `pdftools` package processes
text line by line, it often fails to maintain the context of the text.
As a result, the output may contain sentences with unrelated fragments
of text from different parts of the page. Words that are not placed in
the correct context are unsuitable for text analysis.

However, the words grouped into clusters by this package using a
Density-Based Spatial Clustering algorithm are likely to be contextually
related and thus suitable for text analysis.

See `vignette("pdftextclusteR")` for more information on the usage of
the package.

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
