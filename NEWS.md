# pdftextclusteR 0.0.0.9011 (development version)

## Object model

* The package is now built around an S7 object model: `pdf_read()` returns
  a `PdfDocument` containing `PdfPage` objects; `pdf_detect_clusters()`
  turns these into `PdfClusters` objects.
* `pdf_read()` reads a PDF (path or URL) with font information and page
  dimensions; `pdf_detect_clusters()` also accepts a path directly.
* Standard generics on the objects: `print()`, `summary()`, `plot()`,
  `ggplot2::autoplot()`, `as_tibble()`, `as.data.frame()`, `length()`,
  `[[` and `[`.
* The `npo` and `cibap` datasets are now `PdfDocument` objects, read with
  font information and page dimensions.

## Other changes

* Implemented cluster algorithm, plot function and text extraction
  function; project and pkgdown-website deployed.
* `pdf_extract_clusters()` returns one combined tibble by default
  (`combine = TRUE`) and excludes noise words unless `include_noise = TRUE`.
* Progress bars and messages can be suppressed with the `verbose` argument
  or `options(pdftextclusteR.verbose = FALSE)`.
* Word distances are computed with matrix operations: ~11x faster and ~4x
  less memory.
