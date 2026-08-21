#' Read a PDF Document
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Reads a PDF file into a [PdfDocument]: one [PdfPage] per page with the
#' word data from [pdftools::pdf_data()] (including font information when
#' available) and the page dimensions from [pdftools::pdf_pagesize()].
#'
#' @param path path or URL of the PDF file.
#' @param font_info logical; include the font name and font size of each
#'   word. Defaults to `TRUE`.
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
pdf_read <- function(path, font_info = TRUE) {
  words_list <- pdftools::pdf_data(path, font_info = font_info)
  sizes <- pdftools::pdf_pagesize(path)
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
