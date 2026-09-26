#' Wetsevaluatie Burgerschap Report
#'
#' The report `Wetsevaluatie burgerschap`, published on the Rijksoverheid
#' website on February 12, 2026, read with [pdf_read()]. It was chosen for
#' its two-column layout with numbered headings, footnotes, quote blocks
#' and standalone page numbers.
#'
#' @format ## `burgerschap`
#' A [PdfDocument] with 46 pages. Each page is a [PdfPage] whose `words`
#' tibble contains one row per word:
#' \describe{
#'   \item{width, height}{Width and height of a word}
#'   \item{x, y}{The x and y coordinates of a word. The y-coordinate is measured from the top of the page.}
#'   \item{space}{Indicates whether there is a space after the word. This indicates a line break.}
#'   \item{text}{The word that the metadata refers to.}
#'   \item{font_name, font_size}{The font name and font size of the word.}
#' }
#' @source <https://www.rijksoverheid.nl/documenten/2026/02/12/rapport-wetsevaluatie-burgerschap>
"burgerschap"


#' CIBAP Report
#'
#' The report `Kwaliteitsagenda 2024-2027 Cibap`, published on the
#' Rijksoverheid website on September 16, 2024, read with [pdf_read()]. It
#' was chosen because it contains several pages with columns, infographics
#' and different layouts.
#'
#' @format ## `cibap`
#' A [PdfDocument] with 73 pages; see [burgerschap] for the word columns.
#' @source <https://www.rijksoverheid.nl/documenten/rapporten/2024/09/16/kwaliteitsagenda-2024-2027-cibap>
"cibap"


#' The EU in 2024 Report
#'
#' The report `The EU in 2024 — General Report on the Activities of the
#' European Union` (European Commission, 2025), read with [pdf_read()]. It
#' was chosen for its rich magazine-style layout: two-column text with
#' colored text boxes, quote blocks and photos.
#'
#' @format ## `eu2024`
#' A [PdfDocument] with 180 pages; see [burgerschap] for the word columns.
#' @source <https://op.europa.eu/en/publication-detail/-/publication/9d1a7eec-fb41-11ef-b7db-01aa75ed71a1/language-en>
"eu2024"


#' Eurostat Key Figures on Europe
#'
#' The publication `Key figures on Europe — 2025 edition` (Eurostat), read
#' with [pdf_read()]. It was chosen for its statistical layout:
#' visualisations and charts next to text columns.
#'
#' @format ## `eurostat`
#' A [PdfDocument] with 84 pages; see [burgerschap] for the word columns.
#' @source <https://ec.europa.eu/eurostat/web/products-key-figures/w/ks-01-25-003>
"eurostat"
