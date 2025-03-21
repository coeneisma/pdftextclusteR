
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pdftextclusteR <img src="man/figures/logo.png" align="right" height="132"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/coeneisma/pdftextclusteR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coeneisma/pdftextclusteR/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The [pdftools](https://docs.ropensci.org/pdftools/) package is available
for importing PDF files. However, this package does not work optimally
when importing PDF files with multiple columns and text boxes. Since the
`pdftools::pdf_text()` function from the `pdftools` package processes
text line by line, it often fails to maintain the context of the text.
As a result, the output may contain sentences with unrelated fragments
of text from different parts of the page. Words that are not placed in
the correct context are unsuitable for text analysis.

However, the words grouped into clusters by this package using a
Density-Based Spatial Clustering algorithm are likely to be contextually
related and thus suitable for text analysis. This package directly
utilizes the clustering algorithms implemented in the
[dbscan](https://github.com/mhahsler/dbscan) package.

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

## Example

This is a basic example of the capabilities of this package.

``` r
library(pdftextclusteR)
library(pdftools)

# Read a PDF-file with pdftools::pdf_data()
ka <- pdf_data("https://www.rijksoverheid.nl/binaries/rijksoverheid/documenten/rapporten/2024/06/10/kwaliteitsagenda-2024-2027-mediacollege-amsterdam/Kwaliteitsagenda+2024-2027+Mediacollege+Amsterdam.pdf")

# Detect clusters on the 7th page
ka_clusters <- ka[[7]] |> 
  pdf_detect_clusters()

# Plot clusters on the first page
ka_clusters |> 
  pdf_plot_clusters()
```

<img src="man/figures/README-example-1.png" width="100%" />

Compared with the orriginal document it is quite acurate.

![](vignettes/images/example_pdf.png) Text can be extracted to do
further analysis:

``` r
ka_clusters_text <- ka_clusters |> 
  pdf_extract_clusters()

ka_clusters_text
#> # A tibble: 7 × 3
#>   .cluster word_count text                                                      
#>   <fct>         <int> <chr>                                                     
#> 1 0                 2 "Inleiding\n 7\n"                                         
#> 2 1                89 "Waar liggen de belangrijkste ontwikkelopgaven voor onze …
#> 3 2                89 "Met deze Kwaliteitsagenda 2024-2027 wil MA haar\n ambiti…
#> 4 3                 3 "Kwaliteitsagenda 2024-2027\n"                            
#> 5 4                63 "We bouwen voort op de doelstellingen uit de\n Kwaliteits…
#> 6 5                53 "De strategie en daarmee de prioriteiten voor de MA\n Kwa…
#> 7 6                93 "Ook het werkveld is binnen verschillende overleggen\n en…
```
