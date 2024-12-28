#' Detect Columns and Text Boxes in PDF Document
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function detects columns and text boxes in a PDF file. To do this, you
#' first need to read the file using the [pdftools::pdf_data()]-function from
#' the [pdftools] package.
#'
#' The function works on both a list of pages, as returned by the
#' [pdftools::pdf_data()] function, and individual pages extracted from that
#' list. This makes it flexible for use on either the entire document or
#' specific pages within it.
#'
#' @param pdf_data result of the [pdftools::pdf_data()]-function or a page of
#'   this result.
#' @param algorithm the algorithm to be used to detect text columns or text
#'   boxes
#' @param ... algorithm-specific arguments.
#'
#' @return If the input is a list of pages, a list-object is returned, where
#'   each page contains a tibble and each word is assigned to a cluster. If the
#'   input is a single page, a tibble is returned directly, with each word
#'   assigned to a cluster.
#' @export
#'
#' @examples
#' # First 3 pages
#' head(npo, 3) |>
#'    pdf_detect_clusters()
#'
#' # 3th page
#' npo[[3]] |>
#'    pdf_detect_clusters()
pdf_detect_clusters <- function(pdf_data, algorithm = "dbscan", ...){
  # Check if input is a data.frame or list
  if(!is.data.frame(pdf_data)){
    purrr::map(pdf_data, ~ pdf_detect_clusters_page(.x, algorith = algorithm, ...))
  }
  else{
    pdf_detect_clusters_page(pdf_data, algorithm = "dbscan", ...)
  }

}


#' Detect Columns and Text Boxes in PDF Document
#'
#' `r lifecycle::badge('experimental')` This function detects columns and text
#' boxes in a PDF file. To do this, you first need to read the file using the
#' [pdftools::pdf_data()]-function from the [pdftools] package.
#'
#' In the background, the function [pdf_detect_clusters_page()] is used
#'
#' @param pdf_data result of the [pdftools::pdf_data()]-function
#' @param algorithm the algorithm to be used to detect text columns or text
#'   boxes
#' @param ... algorithm-specific arguments
#' @noRd
#' @return A list-object, where each page contains a tibble and each word is
#'   assigned to a cluster.
pdf_detect_clusters_list <- function(pdf_data, algorithm = "dbscan", ...){
  purrr::map(pdf_data, ~ pdf_detect_clusters_page(.x, algorithm = algorithm, ...))
}


#' Detect Columns and Text Boxes in PDF Page
#'
#' `r lifecycle::badge('experimental')` This function detects columns and text
#' boxes in a PDF page. To do this, you first need to read the file using the
#' [pdftools::pdf_data()]-function from the [pdftools] package.
#'
#' @param pdf_data_page list item of the result of the
#'   [pdftools::pdf_data()]-function
#' @param algorithm algorithm to be used to detect text columns or text boxes
#' @param ... algorithm-specific arguments
#' @noRd
#' @return a tibble is returned, with each word assigned to a cluster.
pdf_detect_clusters_page <- function(pdf_data_page, algorithm = "dbscan", ...){

  # Check if all required variables are present
  required_vars <- c("width", "height", "x", "y", "space", "text")
  missing_vars <- setdiff(required_vars, colnames(pdf_data_page))

  if (length(missing_vars) > 0) {
    stop(
      sprintf(
        "The data.frame is missing the following required variable(s): %s",
        paste(missing_vars, collapse = ", ")
      )
    )
  }

  # Calculate the coordinates of the bounding box of each word
  coords <- pdf_data_page |>
    tibble::rowid_to_column() |>
    dplyr::mutate(
      x_min = x,
      x_max = x + width,
      y_min = y,
      y_max = y + height
    )

  # Calculate distances between all words/bounding boxes
  word_distances <- tidyr::expand_grid(word1 = coords, word2 = coords) |>
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

  # Determine the most common height to use for standard `eps`-value
  max_n_height <- pdf_data_page |>
    dplyr::count(height, sort = TRUE) |>
    dplyr::slice(1) |>
    dplyr::pull(height)


  # Generate distance matrix
  distance_matrix <- word_distances  |>
    tidyr::pivot_wider(names_from = word2,
                       values_from = distance,
                       values_fill = Inf) |>
    tibble::column_to_rownames(var = "word1") |>
    as.dist()

  # Determine default values for arguments
  default_args <- switch(
    algorithm,
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
    algorithm,
    dbscan = do.call(dbscan::dbscan, c(list(distance_matrix), final_args)),
    jpclust = do.call(dbscan::jpclust, c(list(distance_matrix), final_args)),
    sNNclust = do.call(dbscan::sNNclust, c(list(distance_matrix), final_args)),
    hdbscan = do.call(dbscan::hdbscan, c(list(distance_matrix), final_args))
  )

  return(broom::augment(cluster, pdf_data_page))

}

utils::globalVariables(c(".cluster", "distance", "height", "width",
                         "word1", "word1_rowid", "word2", "word2_rowid",
                         "x", "x_center", "x_dist", "xmax", "xmin",
                         "y", "y_center", "y_dist", "ymax", "ymin"))
