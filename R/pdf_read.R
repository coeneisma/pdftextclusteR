#' Read a PDF Document
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Reads a PDF file into a [PdfDocument]: one [PdfPage] per page with the
#' word data from [pdftools::pdf_data()] (including font information when
#' available) and the page dimensions from [pdftools::pdf_pagesize()].
#'
#' @section Scanned pages and OCR:
#'
#' Scanned PDFs contain images of text and no text layer, so
#' [pdftools::pdf_data()] finds no words. By default (`ocr = "auto"`),
#' pages without a text layer are run through OCR with
#' [pdftools::pdf_ocr_data()]. This requires the `tesseract` package
#' (listed under Suggests); when it is not installed, the affected pages
#' stay empty and a warning explains how to install it.
#'
#' OCR-ed words have the same geometry columns as regular words (in
#' points), plus an `ocr_confidence` column (0-100). Font information is
#' not available for OCR-ed pages, so font-based classification rules do
#' not apply there.
#'
#' @param path path or URL of the PDF file.
#' @param font_info logical; include the font name and font size of each
#'   word. Defaults to `TRUE`.
#' @param ocr `"auto"` (default): OCR pages that have no text layer;
#'   `"never"`: no OCR; `"always"`: OCR every page, ignoring an existing
#'   text layer (useful when a PDF carries a bad text layer).
#' @param ocr_language language passed to the OCR engine, e.g. `"eng"` or
#'   `"nld"`. Defaults to the package option `pdftextclusteR.ocr_language`,
#'   or `"eng"` when that option is not set — set
#'   `options(pdftextclusteR.ocr_language = "nld")` in your `.Rprofile`
#'   when you mostly read Dutch documents. The corresponding tesseract
#'   training data must be installed; see
#'   [tesseract::tesseract_download()].
#' @param ocr_dpi resolution at which pages are rendered for OCR. Higher
#'   is more accurate but slower.
#' @param verbose logical; if `FALSE`, informational messages are
#'   suppressed. Defaults to the package option `pdftextclusteR.verbose`,
#'   or `TRUE` when that option is not set.
#'
#' @return A [PdfDocument].
#' @export
#'
#' @examples
#' \dontrun{
#' doc <- pdf_read("path/to/document.pdf")
#' doc |>
#'   pdf_detect_clusters() |>
#'   pdf_extract_clusters()
#' }
pdf_read <- function(path, font_info = TRUE,
                     ocr = c("auto", "never", "always"),
                     ocr_language = getOption("pdftextclusteR.ocr_language", "eng"),
                     ocr_dpi = 300,
                     verbose = getOption("pdftextclusteR.verbose", TRUE)) {
  ocr <- match.arg(ocr)
  words_list <- pdftools::pdf_data(path, font_info = font_info)
  sizes <- pdftools::pdf_pagesize(path)

  empty <- vapply(words_list, nrow, integer(1)) == 0
  need_ocr <- switch(ocr,
    never = rep(FALSE, length(words_list)),
    auto = empty,
    always = rep(TRUE, length(words_list))
  )

  if (any(need_ocr)) {
    if (!requireNamespace("tesseract", quietly = TRUE)) {
      if (ocr == "always") {
        cli::cli_abort(c(
          "{.code ocr = \"always\"} requires the {.pkg tesseract} package.",
          i = "Install it with {.code install.packages(\"tesseract\")}."
        ))
      }
      if (verbose) {
        cli::cli_warn(c(
          "{sum(need_ocr)} page{?s} contain{?s/} no text layer (scanned pages?).",
          i = "Install the {.pkg tesseract} package to read {?it/them} with OCR: {.code install.packages(\"tesseract\")}."
        ))
      }
    } else {
      if (verbose) {
        cli::cli_alert_info("Running OCR on {sum(need_ocr)} page{?s} (language: {ocr_language}).")
      }
      ocr_result <- tryCatch(
        pdftools::pdf_ocr_data(path, pages = which(need_ocr),
                               language = ocr_language, dpi = ocr_dpi),
        error = function(e) {
          n_ocr <- sum(need_ocr)
          cli::cli_warn(c(
            "OCR failed; {n_ocr} page{?s} {?is/are} read without a text layer.",
            x = conditionMessage(e),
            i = "Is the {.val {ocr_language}} training data installed? See {.fn tesseract::tesseract_download}."
          ))
          NULL
        })
      if (!is.null(ocr_result)) {
        words_list[need_ocr] <- lapply(ocr_result, ocr_to_words, dpi = ocr_dpi)
      }
    }
  }

  pages <- lapply(seq_along(words_list), function(i) {
    PdfPage(
      words  = words_list[[i]],
      number = i,
      width  = sizes$width[i],
      height = sizes$height[i]
    )
  })
  PdfDocument(pages = pages, source = path)
}

#' Convert one page of pdf_ocr_data() output to the pdf_data() format
#'
#' The OCR bounding boxes are in pixels at `dpi`; coordinates are
#' converted to points (1/72 inch) to match pdftools::pdf_data().
#'
#' @param ocr_page data frame with word, confidence, bbox ("x1,y1,x2,y2")
#' @param dpi the resolution the page was rendered at for OCR
#' @noRd
ocr_to_words <- function(ocr_page, dpi) {
  if (nrow(ocr_page) == 0) {
    return(tibble::tibble(
      width = numeric(), height = numeric(), x = numeric(), y = numeric(),
      space = logical(), text = character(), ocr_confidence = numeric()
    ))
  }
  bbox <- do.call(rbind, lapply(strsplit(ocr_page$bbox, ","), as.numeric))
  scale <- 72 / dpi
  tibble::tibble(
    width = (bbox[, 3] - bbox[, 1]) * scale,
    height = (bbox[, 4] - bbox[, 2]) * scale,
    x = bbox[, 1] * scale,
    y = bbox[, 2] * scale,
    space = NA,
    text = ocr_page$word,
    ocr_confidence = ocr_page$confidence
  )
}
