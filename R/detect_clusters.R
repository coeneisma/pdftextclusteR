#' Detect Columns and Text Boxes in PDF Document
#'
#' `r lifecycle::badge('experimental')` This function detects columns
#' and text boxes in a PDF file. To do this, you first need to read the file
#' using the [pdftools::pdf_data()]-function from the [pdftools] package.
#'
#' In the background, the function
#' [detect_clusters_page()] is used
#'
#' @param pdf_data list-object resulting from the
#'   [pdftools::pdf_data()]-function
#' @param algorithm the algorithm to be used to detect text columns
#'   or text boxes
#' @param ... algorithm-specific arguments
#'
#' @return A list-object, where each page contains a tibble and each
#'   word is assigned to a cluster.
#' @export
#'
#' @examples
#' npo |>
#'    detect_clusters()
detect_clusters <- function(pdf_data, algoritme = "dbscan", ...){
  purrr::map(pdf_data, detect_clusters_page)
}


#' Detect Columns and Text Boxes in PDF Page
#'
#' `r lifecycle::badge('experimental')` This function detects columns
#' and text boxes in a PDF page. To do this, you first need to read the file
#' using the [pdftools::pdf_data()]-function from the [pdftools] package.
#'
#' @param pdf_data_page Tibble that can be extracted from each element of [pdf_data()]
#' @param algorithm algorithm to be used to detect text columns or
#'   text boxes
#' @param ... algorithm-specific arguments
#'
#' @return Tibble indicating which cluster each word belongs to.
#' @export
#'
#' @examples
#' npo[[10]] |>
#'    detect_clusters_page()
detect_clusters_page <- function(pdf_data_pagina, algoritme = "dbscan", ...){

  coords <- pdf_data_pagina |>
    tibble::rowid_to_column() |>
    dplyr::mutate(
      x_min = x,
      x_max = x + width,
      y_min = y,
      y_max = y + height
    )

  # Calculate distances between all words
  afstanden <- tidyr::expand_grid(word1 = coords, word2 = coords) |>
    dplyr::mutate(
      # Calculate horizontal distance
      x_dist = pmax(0, pmax(word1$x_min - word2$x_max,
                            word2$x_min - word1$x_max)),
      # Calculate vertical distance
      y_dist = pmax(0, pmax(word1$y_min - word2$y_max,
                            word2$y_min - word1$y_max)),
      # Total minimum distance
      distance = sqrt(x_dist^2 + y_dist^2)
    ) |>
    tidyr::unnest(word1, names_sep = "_") |>
    tidyr::unnest(word2, names_sep = "_") |>
    dplyr::select(word1 = word1_rowid, word2 = word2_rowid, distance)

  # Determine the most common height
  max_n_height <- pdf_data_pagina |>
    dplyr::count(height, sort = TRUE) |>
    dplyr::slice(1) |>
    dplyr::pull(height)


  # Generate distance matrix
  distance_matrix <- afstanden  |>
    tidyr::pivot_wider(names_from = word2,
                       values_from = distance,
                       values_fill = Inf) |>
    tibble::column_to_rownames(var = "word1") |>
    as.dist()

  # Determine default values for arguments
  default_args <- switch(
    algoritme,
    dbscan = list(eps = max_n_height * 1.5, minPts = 2),
    jpclust = list(k = 20, kt = 10),
    sNNclust = list(k = 5, eps = 2, minPts = 3),
    hdbscan = list(minPts = 2),
    stop("Invalid algorithm specified.")
  )

  # User-specified values
  user_args <- list(...)

  # Combine default argument values with user-specified values
  final_args <- modifyList(default_args, user_args)

  # Compute cluster based on the chosen algorithm
  cluster <- switch(
    algoritme,
    dbscan = do.call(dbscan::dbscan, c(list(distance_matrix), final_args)),
    jpclust = do.call(dbscan::jpclust, c(list(distance_matrix), final_args)),
    sNNclust = do.call(dbscan::sNNclust, c(list(distance_matrix), final_args)),
    hdbscan = do.call(dbscan::hdbscan, c(list(distance_matrix), final_args))
  )


  return(broom::augment(cluster, pdf_data_pagina))

}


