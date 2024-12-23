#' Extract the Text Per Cluster from the [detect_clusters()] object
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' As an end user, you mainly want to work with the text from the different
#' detected text clusters. This function combines the text from each cluster
#' into a character string and places it in a Tibble.
#'
#' @param pdf_data result of the [detect_clusters()]-function or a page of this
#'   result
#'
#' @return If the input is a list of tibbles, a list-object is returned, where
#'   each element is a tibble containing the text extracted from each cluster on
#'   a page. If the input is a single tibble, a tibble is returned directly,
#'   containing the text extracted from each cluster on that page.
#' @export
#'
#' @examples
#' npo[[1]] |>
#'    detect_clusters() |>
#'    extract_clusters()
extract_clusters <- function(pdf_data){
  if(!is.data.frame(pdf_data)){
    purrr::map(pdf_data, ~ extract_clusters_text_page(.x))
  }
  else{
    extract_clusters_text_page(pdf_data)
  }
}

#' Export the Text Per Cluster on a single page
#'
#' @param pdf_data the result of [detect_clusters()]
#'
#' @return a Tibble with the same number of records as the number of detected
#'   clusters on the page
#' @noRd
extract_clusters_text_page <- function(pdf_data){

  clusters_text <- pdf_data |>
    dplyr::group_by(.cluster) |>
    dplyr::mutate(text = paste0(text, collapse = " ")) |>
    dplyr::select(.cluster, text) |>
    dplyr::distinct()

  return(clusters_text)

}
