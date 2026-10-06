#' Extract Clean Text from a PDF in One Step
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' The one-stop pipeline: reads the document (when given a path), detects
#' text clusters, classifies their types, orders them in reading order and
#' extracts the text — excluding page headers, footers and page numbers by
#' default.
#'
#' Equivalent to [pdf_read()] + [pdf_detect_clusters()] +
#' [pdf_classify_clusters()] + [pdf_extract_clusters()]. Use the
#' individual steps when you want to inspect or tune intermediate results.
#'
#' @section Multiple documents:
#'
#' A vector of paths, or the path of a directory (its `*.pdf` files are
#' used), processes every document through the full pipeline. The result
#' is one tibble with a `document` column (the file name) in front of the
#' `page` column. A document that cannot be processed is skipped with a
#' warning instead of failing the batch.
#'
#' @param x a path/URL to a PDF file, a vector of paths, the path of a
#'   directory containing PDF files, or a [PdfDocument].
#' @param exclude character vector of text types to leave out. Defaults to
#'   the page furniture: header, footer and page number.
#' @param ocr,ocr_language,ocr_dpi how pages without a text layer are
#'   handled when `x` is a path; see [pdf_read()].
#' @param verbose logical; if `FALSE`, progress bars and informational
#'   messages are suppressed.
#' @param ... passed on to [pdf_detect_clusters()].
#'
#' @return A tibble with one row per cluster: `page`, `.cluster`, `.type`,
#'   `.type_level`, `word_count` and `text`, in reading order. For
#'   multiple documents, a `document` column is added in front.
#' @export
#'
#' @examples
#' burgerschap[1:3] |>
#'   pdf_extract_text()
#'
#' \dontrun{
#' # A whole directory of PDF files
#' texts <- pdf_extract_text("path/to/folder")
#' }
pdf_extract_text <- function(x,
                             exclude = c("page_header", "page_footer",
                                         "page_number"),
                             ocr = c("auto", "never", "always"),
                             ocr_language = getOption("pdftextclusteR.ocr_language", "eng"),
                             ocr_dpi = 300,
                             verbose = getOption("pdftextclusteR.verbose", TRUE),
                             ...) {
  ocr <- match.arg(ocr)
  if (is.character(x)) {
    paths <- x
    if (length(paths) == 1 && dir.exists(paths)) {
      paths <- list.files(paths, pattern = "\\.pdf$", full.names = TRUE,
                          ignore.case = TRUE)
      if (length(paths) == 0) {
        cli::cli_abort("No PDF files found in {.path {x}}.")
      }
    }
    if (length(paths) > 1) {
      return(pdf_extract_text_batch(paths, exclude = exclude, ocr = ocr,
                                    ocr_language = ocr_language,
                                    ocr_dpi = ocr_dpi, verbose = verbose,
                                    ...))
    }
    x <- pdf_read(paths, ocr = ocr, ocr_language = ocr_language,
                  ocr_dpi = ocr_dpi, verbose = verbose)
  }
  x |>
    pdf_detect_clusters(verbose = verbose, ...) |>
    pdf_classify_clusters(verbose = verbose) |>
    pdf_extract_clusters(exclude = exclude, verbose = verbose)
}

#' Run the one-step pipeline over multiple files
#'
#' @param paths character vector of PDF paths
#' @noRd
pdf_extract_text_batch <- function(paths, exclude, ocr, ocr_language,
                                   ocr_dpi, verbose, ...) {
  n <- length(paths)
  show_progress <- verbose && n > 1
  if (show_progress) {
    cli::cli_alert_info("Processing {n} documents")
    pdf_progress_bar("Documents", n)
  }

  results <- vector("list", n)
  for (i in seq_len(n)) {
    results[[i]] <- tryCatch(
      pdf_extract_text(paths[i], exclude = exclude, ocr = ocr,
                       ocr_language = ocr_language, ocr_dpi = ocr_dpi,
                       verbose = FALSE, ...),
      error = function(e) {
        cli::cli_warn(c(
          "Skipping {.path {paths[i]}}: it could not be processed.",
          x = conditionMessage(e)
        ))
        NULL
      })
    if (show_progress) cli::cli_progress_update()
  }
  if (show_progress) cli::cli_progress_done()

  names(results) <- basename(paths)
  processed <- sum(!vapply(results, is.null, logical(1)))
  if (verbose) {
    cli::cli_alert_success("Extracted text from {processed} of {n} document{?s}.")
  }
  dplyr::bind_rows(results, .id = "document")
}
