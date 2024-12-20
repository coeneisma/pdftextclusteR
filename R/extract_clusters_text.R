#' Export the Text Per Cluster
#'
#' As an end user, you mainly want to work with the text from the
#' different detected text clusters. This function combines the text
#' from each cluster into a character string and places it in a Tibble.
#'
#' @param pdf_data_page_clusters the result of the function
#'   [detect_clusters_page()]
#'
#' @return a Tibble with the same number of records as the number
#'   of detected clusters on the page
#' @export
#'
#' @examples
#' npo[[1]] |>
#'    detect_clusters_page() |>
#'    extract_clusters_text()
extract_clusters_text <- function(pdf_data_pagina_clusters){

  clusters_text <- pdf_data_pagina_clusters |>
    dplyr::group_by(.cluster) |>
    dplyr::mutate(text = paste0(text, collapse = " ")) |>
    dplyr::select(.cluster, text) |>
    dplyr::distinct()

  return(clusters_text)

}
