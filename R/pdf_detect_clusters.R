#' Detect Columns and Text Boxes in PDF Document
#'
#' @description `r lifecycle::badge('experimental')`
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
#' This package directly utilizes the clustering algorithms implemented in the
#' [dbscan] package. For this a [stats::dist()] object is created.
#'
#' @param pdf_data result of the [pdftools::pdf_data()]-function or a page of
#'   this result.
#' @param algorithm the algorithm to be used to detect text columns or text
#'   boxes
#' @param tolerance_factor numeric; factor used for column detection when renumbering.
#'   Higher values allow more variation in x-coordinates. Default is 0.1 (10% of page width).
#' @param verbose logical; if `FALSE`, progress bars and informational
#'   messages are suppressed. Defaults to the package option
#'   `pdftextclusteR.verbose`, or `TRUE` when that option is not set.
#' @param ... algorithm-specific arguments. See [dbscan::dbscan()], [dbscan::jpclust()], [dbscan::sNNclust()] and [dbscan::hdbscan()] for more information
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
#' # 3th page with sNNclust algorithm with minPts = 5
#' npo[[3]] |>
#'    pdf_detect_clusters(algorithm = "sNNclust", minPts = 5)
pdf_detect_clusters <- function(pdf_data, algorithm = "dbscan",
                                tolerance_factor = 0.1,
                                verbose = getOption("pdftextclusteR.verbose", TRUE),
                                ...) {
  # Check if input is a data.frame or list
  if (!is.data.frame(pdf_data)) {

    # Count total number of pages
    total_pages <- length(pdf_data)

    # Create progress bar for multiple pages
    show_progress <- verbose && total_pages > 1
    if (show_progress) {
      cli::cli_alert_info("Processing {total_pages} pages")
      pdf_progress_bar("Processing", total_pages)
    }

    # Process all pages and check if they are empty
    results <- vector("list", total_pages)

    for (i in seq_len(total_pages)) {
      page_data <- pdf_data[[i]]

      if (nrow(page_data) == 0) {
        results[i] <- list(NULL)
      } else {
        # Detect clusters
        clusters <- pdf_detect_clusters_page(page_data, algorithm, ...)

        # Always renumber clusters
        if (!is.null(clusters)) {
          clusters <- pdf_renumber_clusters_page(clusters, tolerance_factor)
        }

        results[[i]] <- clusters
      }

      # Update progress after each page is fully processed
      if (show_progress) {
        cli::cli_progress_update()
      }
    }

    # Close progress bar
    if (show_progress) {
      cli::cli_progress_done()
    }

    # Count successful and failed detections
    successful_pages <- sum(!purrr::map_lgl(results, is.null))
    failed_pages <- total_pages - successful_pages

    # CLI message
    if (verbose) {
      cli::cli_alert_success("Clusters successfully detected and renumbered on {successful_pages} page{?s}.")
      if (failed_pages > 0) {
        cli::cli_alert_danger("{failed_pages} page{?s} contain no text and could not be processed.")
      }
    }

    return(results)

  } else {

    # Check if the single page is empty
    if (nrow(pdf_data) == 0) {
      if (verbose) {
        cli::cli_alert_danger("The provided page contains no text. No clusters detected.")
      }
      return(NULL)
    }

    # Detect clusters
    clusters <- pdf_detect_clusters_page(pdf_data, algorithm, ...)

    # Always renumber clusters
    clusters <- pdf_renumber_clusters_page(clusters, tolerance_factor)

    # Count the number of clusters
    num_clusters <- length(unique(clusters$.cluster[clusters$.cluster != 0]))

    # CLI message
    if (verbose) {
      cli::cli_alert_info("Clusters detected and renumbered: {num_clusters} on this page.")
    }

    return(clusters)
  }
}

#' Renumber clusters in logical reading order
#'
#' @param pdf_page_clusters a tibble with clusters from pdf_detect_clusters_page
#' @param tolerance_factor tolerance factor for column detection
#' @noRd
#' @return a tibble with renumbered clusters
pdf_renumber_clusters_page <- function(pdf_page_clusters, tolerance_factor = 0.1) {
  # Check if there are clusters to renumber
  if (length(unique(pdf_page_clusters$.cluster)) <= 1) {
    return(pdf_page_clusters)  # No clusters to renumber
  }

  # Calculate the left edge and vertical center of each cluster
  cluster_positions <- pdf_page_clusters |>
    dplyr::group_by(.cluster) |>
    dplyr::summarise(
      x_left = min(x),  # Left edge of the cluster
      y_center = (min(y) + max(y + height)) / 2,  # Vertical center for ordering within columns
      .groups = 'drop'
    ) |>
    dplyr::filter(.cluster != 0)  # Ignore noise (cluster 0)

  # Determine page properties
  page_width <- max(pdf_page_clusters$x + pdf_page_clusters$width) - min(pdf_page_clusters$x)
  tolerance <- page_width * tolerance_factor

  # Detect columns (group by x-coordinate with tolerance)
  cluster_columns <- cluster_positions |>
    dplyr::mutate(
      # Round x_left to nearest multiple of tolerance to group into columns
      column_approx = round(x_left / tolerance) * tolerance
    ) |>
    dplyr::arrange(column_approx, y_center) |>
    dplyr::group_by(column_approx) |>
    dplyr::mutate(column_number = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::arrange(column_number, y_center) |>
    dplyr::mutate(new_cluster = dplyr::row_number())

  # Create mapping from old to new cluster numbers
  cluster_mapping <- cluster_columns |>
    dplyr::select(.cluster, new_cluster) |>
    tibble::deframe()

  # First, convert to numeric for the mapping operations
  numeric_clusters <- pdf_page_clusters |>
    dplyr::mutate(.cluster_num = as.numeric(as.character(.cluster)))

  # Apply mapping to get new numeric cluster values
  mapped_clusters <- numeric_clusters |>
    dplyr::mutate(
      .cluster_new = dplyr::if_else(
        .cluster_num == 0,
        0,
        as.numeric(cluster_mapping[as.character(.cluster_num)])
      )
    )

  # Convert back to factor with the same levels structure as original
  max_cluster <- max(mapped_clusters$.cluster_new)
  renumbered_clusters <- mapped_clusters |>
    dplyr::mutate(
      .cluster = factor(.cluster_new, levels = 0:max_cluster),
      # Remove temporary columns
      .cluster_num = NULL,
      .cluster_new = NULL
    )

  return(renumbered_clusters)
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

  # Check if pdf_data_page is not empty
  if (is.null(pdf_data_page) || nrow(pdf_data_page) == 0) {
    return(tibble::tibble())  # Lege tibble teruggeven als er geen tekst is
  }

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

  # Determine the most common height to use for standard `eps`-value
  max_n_height <- pdf_data_page |>
    dplyr::count(height, sort = TRUE) |>
    dplyr::slice(1) |>
    dplyr::pull(height)

  # Bounding box of each word
  x_min <- pdf_data_page$x
  x_max <- pdf_data_page$x + pdf_data_page$width
  y_min <- pdf_data_page$y
  y_max <- pdf_data_page$y + pdf_data_page$height

  # Gap distance between all bounding boxes: the horizontal and vertical
  # gap between two boxes is 0 when they overlap on that axis
  x_gap <- pmax(outer(x_min, x_max, "-"), t(outer(x_min, x_max, "-")), 0)
  y_gap <- pmax(outer(y_min, y_max, "-"), t(outer(y_min, y_max, "-")), 0)

  distance_matrix <- stats::as.dist(sqrt(x_gap^2 + y_gap^2))

  # Determine default values for arguments
  default_args <- switch(
    algorithm,
    dbscan = list(eps = max_n_height * 1, minPts = 2),
    jpclust = list(k = 20, kt = 10),
    sNNclust = list(k = 5, eps = 2, minPts = 3),
    hdbscan = list(minPts = 2),
    stop("Invalid algorithm specified.")
  )

  # User-specified values
  user_args <- list(...)

  # Combine default argument values with user-specified values
  final_args <- utils::modifyList(default_args, user_args)

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

utils::globalVariables(c(".cluster", "height", "width",
                         "page", "text",
                         "x", "x_center", "xmax", "xmin",
                         "y", "y_center", "ymax", "ymin",
                         "word_count", "x_left", "column_approx",
                         "column_number", "new_cluster", ".cluster_num",
                         ".cluster_new"))
