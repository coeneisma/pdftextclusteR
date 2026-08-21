#' Detect Columns and Text Boxes in PDF Document
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Detects columns and text boxes by clustering words based on the distance
#' between their bounding boxes. Accepts a [PdfDocument] (as returned by
#' [pdf_read()]), a single [PdfPage], or a path/URL to a PDF file (which is
#' read with [pdf_read()] first).
#'
#' This package directly utilizes the clustering algorithms implemented in
#' the [dbscan] package. Detected clusters are renumbered in reading
#' order using a recursive XY-cut: groups of clusters separated by
#' whitespace are read top to bottom, and columns within such a group left
#' to right.
#'
#' @param x a [PdfDocument], a [PdfPage], or a path/URL to a PDF file.
#' @param algorithm the algorithm used to detect text columns or text
#'   boxes: `"dbscan"` (default), `"jpclust"`, `"sNNclust"` or `"hdbscan"`.
#' @param min_gap_factor numeric; the minimal whitespace band used to
#'   separate groups of clusters when ordering them in reading order,
#'   expressed as a multiple of the most common word height on the page.
#'   Default 1.
#' @param prefer `"rows"` (default) or `"columns"`: when a group of
#'   clusters can be split both horizontally and vertically, which cut
#'   direction wins. With `"rows"`, a full-width element above two columns
#'   is read before those columns.
#' @param tolerance_factor `r lifecycle::badge('deprecated')` ignored; use
#'   `min_gap_factor` and `prefer` instead.
#' @param verbose logical; if `FALSE`, progress bars and informational
#'   messages are suppressed. Defaults to the package option
#'   `pdftextclusteR.verbose`, or `TRUE` when that option is not set.
#' @param ... algorithm-specific arguments. See [dbscan::dbscan()],
#'   [dbscan::jpclust()], [dbscan::sNNclust()] and [dbscan::hdbscan()].
#'
#' @return A [PdfDocument] whose pages are [PdfClusters] objects when the
#'   input is a document or path; a single [PdfClusters] object when the
#'   input is a [PdfPage].
#' @export
#'
#' @examples
#' # A single page
#' npo[[3]] |>
#'   pdf_detect_clusters()
#'
#' # The first 3 pages, with the sNNclust algorithm
#' npo[1:3] |>
#'   pdf_detect_clusters(algorithm = "sNNclust", minPts = 5)
pdf_detect_clusters <- S7::new_generic(
  "pdf_detect_clusters", "x",
  function(x, algorithm = "dbscan", min_gap_factor = 1, prefer = "rows",
           verbose = getOption("pdftextclusteR.verbose", TRUE),
           tolerance_factor = lifecycle::deprecated(), ...) {
    S7::S7_dispatch()
  }
)

S7::method(pdf_detect_clusters, S7::class_character) <- function(
    x, algorithm = "dbscan", min_gap_factor = 1, prefer = "rows",
    verbose = getOption("pdftextclusteR.verbose", TRUE),
    tolerance_factor = lifecycle::deprecated(), ...) {
  pdf_detect_clusters(pdf_read(x), algorithm = algorithm,
                      min_gap_factor = min_gap_factor, prefer = prefer,
                      verbose = verbose,
                      tolerance_factor = tolerance_factor, ...)
}

S7::method(pdf_detect_clusters, PdfDocument) <- function(
    x, algorithm = "dbscan", min_gap_factor = 1, prefer = "rows",
    verbose = getOption("pdftextclusteR.verbose", TRUE),
    tolerance_factor = lifecycle::deprecated(), ...) {
  warn_tolerance_factor(tolerance_factor)

  total_pages <- length(x@pages)
  show_progress <- verbose && total_pages > 1
  if (show_progress) {
    cli::cli_alert_info("Processing {total_pages} pages")
    pdf_progress_bar("Processing", total_pages)
  }

  pages <- vector("list", total_pages)
  for (i in seq_len(total_pages)) {
    pages[[i]] <- detect_clusters_on_page(x@pages[[i]], algorithm,
                                          min_gap_factor, prefer, ...)
    if (show_progress) cli::cli_progress_update()
  }
  if (show_progress) cli::cli_progress_done()

  successful_pages <- sum(vapply(pages, function(p) nrow(p@words) > 0, logical(1)))
  failed_pages <- total_pages - successful_pages
  if (verbose) {
    cli::cli_alert_success("Clusters successfully detected and renumbered on {successful_pages} page{?s}.")
    if (failed_pages > 0) {
      cli::cli_alert_danger("{failed_pages} page{?s} contain no text and could not be processed.")
    }
  }

  PdfDocument(pages = pages, source = x@source)
}

S7::method(pdf_detect_clusters, PdfPage) <- function(
    x, algorithm = "dbscan", min_gap_factor = 1, prefer = "rows",
    verbose = getOption("pdftextclusteR.verbose", TRUE),
    tolerance_factor = lifecycle::deprecated(), ...) {
  warn_tolerance_factor(tolerance_factor)
  result <- detect_clusters_on_page(x, algorithm, min_gap_factor, prefer, ...)
  if (verbose) {
    if (nrow(x@words) == 0) {
      cli::cli_alert_danger("The provided page contains no text. No clusters detected.")
    } else {
      num_clusters <- n_clusters(result)
      cli::cli_alert_info("Clusters detected and renumbered: {num_clusters} on this page.")
    }
  }
  result
}

#' Detect and renumber clusters on one page, wrapping the result
#'
#' @param page a PdfPage
#' @noRd
detect_clusters_on_page <- function(page, algorithm, min_gap_factor, prefer, ...) {
  words <- page@words
  if (nrow(words) > 0) {
    words <- pdf_detect_clusters_page(words, algorithm, ...)
    words <- order_clusters_page(words, min_gap_factor = min_gap_factor,
                                 prefer = prefer)
  }
  PdfClusters(
    words = words, number = page@number,
    width = page@width, height = page@height,
    algorithm = algorithm, params = list(...)
  )
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

utils::globalVariables(c(".cluster", "height", "width", "font_name", "words",
                         "page", "text",
                         "x", "x_center", "xmax", "xmin",
                         "y", "y_center", "ymax", "ymin",
                         "word_count"))
