#' Extract the Text Per Cluster
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   As an end user, you mainly want to work with the text from the
#'   different detected text clusters. This function combines the text from
#'   each cluster into a character string and places it in a tibble, with a
#'   word count per cluster.
#'
#' @param x a [PdfDocument] whose pages have been clustered with
#'   [pdf_detect_clusters()], or a single [PdfClusters] page.
#' @param combine logical; if `TRUE` (default) and the input is a document,
#'   one combined tibble is returned with a `page` column. If `FALSE`, a
#'   list of tibbles is returned, one per page.
#' @param include_noise logical; if `FALSE` (default), words that were not
#'   assigned to any cluster (noise, `.cluster == 0`) are excluded from the
#'   output. Set to `TRUE` to include them as cluster 0.
#' @param verbose logical; if `FALSE`, progress bars and informational
#'   messages are suppressed. Defaults to the package option
#'   `pdftextclusteR.verbose`, or `TRUE` when that option is not set.
#' @param ... not used.
#'
#' @return A tibble with one row per cluster (`.cluster`, `word_count`,
#'   `text`), for a document preceded by a `page` column. With
#'   `combine = FALSE` a list of such tibbles, one per page.
#' @export
#'
#' @examples
#' # A single page
#' npo[[1]] |>
#'   pdf_detect_clusters() |>
#'   pdf_extract_clusters()
#'
#' # Multiple pages combined into one tibble
#' npo[1:3] |>
#'   pdf_detect_clusters() |>
#'   pdf_extract_clusters()
pdf_extract_clusters <- S7::new_generic(
  "pdf_extract_clusters", "x",
  function(x, combine = TRUE, include_noise = FALSE,
           verbose = getOption("pdftextclusteR.verbose", TRUE), ...) {
    S7::S7_dispatch()
  }
)

S7::method(pdf_extract_clusters, PdfClusters) <- function(
    x, combine = TRUE, include_noise = FALSE,
    verbose = getOption("pdftextclusteR.verbose", TRUE), ...) {
  pdf_extract_clusters_text_page(x@words, include_noise = include_noise,
                                 verbose = verbose)
}

S7::method(pdf_extract_clusters, PdfDocument) <- function(
    x, combine = TRUE, include_noise = FALSE,
    verbose = getOption("pdftextclusteR.verbose", TRUE), ...) {

  if (!all(vapply(x@pages, S7::S7_inherits, logical(1), class = PdfClusters))) {
    cli::cli_abort("No clusters detected yet. Run {.fn pdf_detect_clusters} first.")
  }

  total_pages <- length(x@pages)
  show_progress <- verbose && total_pages > 1
  if (show_progress) {
    cli::cli_alert_info("Extracting text from {total_pages} pages")
    pdf_progress_bar("Extracting", total_pages)
  }

  results <- vector("list", total_pages)
  for (i in seq_len(total_pages)) {
    results[[i]] <- pdf_extract_clusters_text_page(
      x@pages[[i]]@words, include_noise = include_noise, verbose = FALSE)
    if (show_progress) cli::cli_progress_update()
  }
  if (show_progress) cli::cli_progress_done()

  if (combine) {
    numbers <- vapply(x@pages, function(p) p@number, integer(1))
    combined <- dplyr::bind_rows(Map(function(res, page_num) {
      if (nrow(res) > 0) dplyr::mutate(res, page = page_num, .before = 1) else NULL
    }, results, numbers))

    if (nrow(combined) == 0) {
      if (verbose) cli::cli_alert_warning("No valid text clusters found on any page.")
      return(tibble::tibble(page = integer(), .cluster = factor(),
                            word_count = integer(), text = character()))
    }
    if (verbose) {
      cli::cli_alert_success("Combined text from {total_pages} page{?s} into a single tibble.")
    }
    combined
  } else {
    successful_pages <- sum(vapply(results, nrow, integer(1)) > 0)
    empty_pages <- total_pages - successful_pages
    if (verbose) {
      cli::cli_alert_success("Text successfully extracted from {successful_pages} page{?s}.")
      if (empty_pages > 0) {
        cli::cli_alert_warning("{empty_pages} page{?s} contain no text clusters.")
      }
    }
    results
  }
}

#' Export the Text Per Cluster on a single page
#'
#' @param pdf_data the result of [pdf_detect_clusters()]
#'
#' @return a Tibble with the same number of records as the number of detected
#'   clusters on the page
#' @noRd
pdf_extract_clusters_text_page <- function(pdf_data, include_noise = FALSE,
                                           verbose = TRUE){
  # Return empty tibble if input is empty or NULL
  if(is.null(pdf_data) || nrow(pdf_data) == 0) {
    if (verbose) {
      cli::cli_alert_warning("Empty page data provided, returning empty tibble.")
    }
    return(tibble::tibble(.cluster = factor(), word_count = integer(), text = character()))
  }

  # Exclude words that were not assigned to any cluster (noise)
  if (!include_noise) {
    pdf_data <- pdf_data |>
      dplyr::filter(.cluster != 0)
  }

  # Binding variable to function to prevent "Undefined global functions or
  # variables:" note from devtools::check()
  word_count <- NA

  clusters_text <- pdf_data |>
    dplyr::mutate(text = dplyr::case_when(space == FALSE ~ paste0(text, "\n"),
                                          TRUE ~ text)) |>
    dplyr::group_by(.cluster) |>
    dplyr::mutate(text = paste0(text, collapse = " ")) |>
    dplyr::select(.cluster, text) |>
    dplyr::distinct() |>
    dplyr::mutate(word_count = stringr::str_count(text, "\\b\\w+\\b")) |>
    dplyr::ungroup() |>
    dplyr::select(.cluster, word_count, text)

  return(clusters_text)
}
