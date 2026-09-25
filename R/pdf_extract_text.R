#' Extract Clean Text from a PDF in One Step
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' The one-stop pipeline: reads the document (when given a path), detects
#' text clusters, classifies their types, orders them in reading order and
#' extracts the text — excluding page headers, footers and page numbers by
#' default.
#'
#' Equivalent to `pdf_read()` + [pdf_detect_clusters()] +
#' [pdf_classify_clusters()] + [pdf_extract_clusters()]. Use the
#' individual steps when you want to inspect or tune intermediate results.
#'
#' @param x a path/URL to a PDF file, or a [PdfDocument].
#' @param exclude character vector of text types to leave out. Defaults to
#'   the page furniture: header, footer and page number.
#' @param verbose logical; if `FALSE`, progress bars and informational
#'   messages are suppressed.
#' @param ... passed on to [pdf_detect_clusters()].
#'
#' @return A tibble with one row per cluster: `page`, `.cluster`, `.type`,
#'   `.type_level`, `word_count` and `text`, in reading order.
#' @export
#'
#' @examples
#' npo[1:3] |>
#'   pdf_extract_text()
pdf_extract_text <- function(x,
                             exclude = c("page_header", "page_footer",
                                         "page_number"),
                             verbose = getOption("pdftextclusteR.verbose", TRUE),
                             ...) {
  if (is.character(x)) {
    x <- pdf_read(x)
  }
  x |>
    pdf_detect_clusters(verbose = verbose, ...) |>
    pdf_classify_clusters(verbose = verbose) |>
    pdf_extract_clusters(exclude = exclude, verbose = verbose)
}
