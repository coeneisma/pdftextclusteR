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

## Reading order

* Clusters are numbered in reading order using a recursive XY-cut over
  the cluster bounding boxes: groups separated by whitespace are read top
  to bottom, columns within a group left to right. The `tolerance_factor`
  argument is deprecated in favour of `min_gap_factor` and `prefer`.
* The text of each cluster is built in visual reading order (lines top to
  bottom, words left to right within a line), independent of the word
  order in the PDF. An optional `dehyphenate` argument merges words
  hyphenated across line breaks.
* `pdf_plot_clusters(show_order = TRUE)` draws arrows between clusters in
  reading order.

## Text types

* `pdf_classify_clusters()` assigns a text type to every cluster (`body`,
  `heading` with level, `caption`, `figure_text`, `page_header`,
  `page_footer`, `page_number`), using document-level signals: repeated
  (digit-masked) text in the page margins and page-number progression.
  Page furniture is moved to the end of the reading order. Thresholds are
  tunable via `pdf_type_rules()`.
* `pdf_extract_clusters()` gains an `exclude` argument to drop types from
  the extraction; the output includes `.type`/`.type_level` columns after
  classification.
* `pdf_plot_clusters(color_by = ".type")` colors clusters by text type.
* `pdf_extract_text()` runs the whole pipeline (read, detect, classify,
  order, extract) in one step, excluding page furniture by default.

## Other changes

* Implemented cluster algorithm, plot function and text extraction
  function; project and pkgdown-website deployed.
* `pdf_extract_clusters()` returns one combined tibble by default
  (`combine = TRUE`) and excludes noise words unless `include_noise = TRUE`.
* Progress bars and messages can be suppressed with the `verbose` argument
  or `options(pdftextclusteR.verbose = FALSE)`.
* Word distances are computed with matrix operations: ~11x faster and ~4x
  less memory.
