#' NPO Report
#'
#' This dataset is the result of reading the report
#' `NPO Terugblik 2023`, which was posted on the Rijksoverheid website
#' on November 20, 2024, as an annex to the Media Budget 2025.
#'
#' This document was chosen because it contains several pages with
#' columns and different layouts.
#'
#' @format ## `npo`
#' A large list object with 73 elements. Each element contains a tibble with 6 columns and one record for each word on the page.
#' \describe{
#'   \item{width, height}{Width and height of a word}
#'   \item{x, y}{The x and y coordinates of a word. The x-coordinate is measured from the top.}
#'   \item{space}{Indicates whether there is a space after the word. This indicates a line break.}
#'   \item{text}{The word that the metadata refers to.}
#' }
#' @source <https://www.rijksoverheid.nl/documenten/rapporten/2024/11/20/bijlage-3-npo-terugblik-2023>
"npo"


#' CIBAP Report
#'
#' This dataset is the result of reading the report
#' `Kwaliteitsagenda 2024-2027 Cibap`, which was posted on the Rijksoverheid website
#' on September 16, 2024.
#'
#' This document was chosen because it contains several pages with
#' columns and different layouts.
#'
#' @format ## `cibap`
#' A large list object with 73 elements. Each element contains a tibble with 6 columns and one record for each word on the page.
#' \describe{
#'   \item{width, height}{Width and height of a word}
#'   \item{x, y}{The x and y coordinates of a word. The x-coordinate is measured from the top.}
#'   \item{space}{Indicates whether there is a space after the word. This indicates a line break.}
#'   \item{text}{The word that the metadata refers to.}
#' }
#' @source <https://www.rijksoverheid.nl/documenten/rapporten/2024/09/16/kwaliteitsagenda-2024-2027-cibap>
"cibap"
