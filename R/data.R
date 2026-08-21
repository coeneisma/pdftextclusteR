#' NPO Report
#'
#' This dataset is the result of reading the report `NPO Terugblik 2023`
#' with [pdf_read()]. The report was posted on the Rijksoverheid website on
#' November 20, 2024, as an annex to the Media Budget 2025. It was chosen
#' because it contains several pages with columns and different layouts.
#'
#' @format ## `npo`
#' A [PdfDocument] with 99 pages. Each page is a [PdfPage] whose `words`
#' tibble contains one row per word:
#' \describe{
#'   \item{width, height}{Width and height of a word}
#'   \item{x, y}{The x and y coordinates of a word. The y-coordinate is measured from the top of the page.}
#'   \item{space}{Indicates whether there is a space after the word. This indicates a line break.}
#'   \item{text}{The word that the metadata refers to.}
#'   \item{font_name, font_size}{The font name and font size of the word.}
#' }
#' @source <https://www.rijksoverheid.nl/documenten/rapporten/2024/11/20/bijlage-3-npo-terugblik-2023>
#'   (retrieved via the Internet Archive; the original download link is no
#'   longer available)
"npo"


#' CIBAP Report
#'
#' This dataset is the result of reading the report
#' `Kwaliteitsagenda 2024-2027 Cibap` with [pdf_read()]. The report was
#' posted on the Rijksoverheid website on September 16, 2024. It was chosen
#' because it contains several pages with columns and different layouts.
#'
#' @format ## `cibap`
#' A [PdfDocument] with 73 pages. Each page is a [PdfPage] whose `words`
#' tibble contains one row per word:
#' \describe{
#'   \item{width, height}{Width and height of a word}
#'   \item{x, y}{The x and y coordinates of a word. The y-coordinate is measured from the top of the page.}
#'   \item{space}{Indicates whether there is a space after the word. This indicates a line break.}
#'   \item{text}{The word that the metadata refers to.}
#'   \item{font_name, font_size}{The font name and font size of the word.}
#' }
#' @source <https://www.rijksoverheid.nl/documenten/rapporten/2024/09/16/kwaliteitsagenda-2024-2027-cibap>
"cibap"
