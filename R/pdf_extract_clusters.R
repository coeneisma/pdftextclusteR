#' Extract the Text Per Cluster from the [pdf_detect_clusters()] object
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   As an end user, you mainly want to work with the text from the different
#'   detected text clusters. This function combines the text from each cluster
#'   into a character string and places it in a Tibble. A word count is added
#'   per cluster.
#'
#' @param pdf_data result of the [pdf_detect_clusters()]-function or a page of
#'   this result
#' @param combine logical; if TRUE (default) and input is a list of tibbles, the
#'   function returns one combined tibble with a page number column added. If
#'   FALSE, returns a list of tibbles.
#'
#' @return If the input is a list of tibbles and combine=TRUE (default), a
#'   single tibble is returned containing the text of all clusters with an added
#'   page number column. If the input is a list of tibbles and combine=FALSE, a
#'   list-object is returned, where each element is a tibble containing the text
#'   extracted from each cluster on a page. If the input is a single tibble, a
#'   tibble is returned directly, containing the text extracted from each
#'   cluster on that page, including a word count per cluster.
#' @export
#'
#' @examples
#' # Extract text from a single page's clusters
#' npo[[1]] |>
#'    pdf_detect_clusters() |>
#'    pdf_extract_clusters()
#'
#' # Extract text from multiple pages as a single combined tibble
#' head(npo, 3) |>
#'    pdf_detect_clusters() |>
#'    pdf_extract_clusters()
#'
#' # Extract text from multiple pages as a list of tibbles
#' head(npo, 3) |>
#'    pdf_detect_clusters() |>
#'    pdf_extract_clusters(combine = FALSE)
pdf_extract_clusters <- function(pdf_data, combine = FALSE){
  if(!is.data.frame(pdf_data)){
    # Count total number of pages
    total_pages <- length(pdf_data)

    # Create progress bar for multiple pages
    if (total_pages > 1) {
      cli::cli_alert_info("Extracting text from {total_pages} pages")

      # Create a horizontal progress bar with ETA
      cli::cli_progress_bar(
        format = paste0(
          "Extracting: ",
          "{cli::pb_spin} [{cli::pb_current}/{cli::pb_total}] ",
          "[{cli::pb_bar}] {cli::pb_percent}% ",
          "ETA: {cli::pb_eta}"
        ),
        total = total_pages,
        clear = FALSE
      )
    }

    # Process all pages with progress updates
    results <- vector("list", total_pages)
    for (i in seq_len(total_pages)) {
      results[[i]] <- pdf_extract_clusters_text_page(pdf_data[[i]])

      # Update progress bar
      if (total_pages > 1) {
        cli::cli_progress_update()
      }
    }

    # Close progress bar
    if (total_pages > 1) {
      cli::cli_progress_done()
    }

    # If combine=TRUE, merge all tibbles into one with a page column
    if(combine){
      if(length(results) > 0){
        # Create page numbers
        page_numbers <- seq_along(results)

        # Create a list of tibbles with page numbers added
        results_with_page <- purrr::map2(results, page_numbers, function(res, page_num){
          if(!is.null(res) && nrow(res) > 0){
            res |> dplyr::mutate(page = page_num)
          } else {
            NULL
          }
        })

        # Filter out NULLs and combine
        valid_results <- purrr::compact(results_with_page)

        if(length(valid_results) > 0){
          combined <- dplyr::bind_rows(valid_results)

          # Reorder columns to place page at the beginning
          combined <- combined |> dplyr::select(page, .cluster, word_count, text)

          # Report success via CLI
          total_pages <- length(results)
          cli::cli_alert_success("Combined text from {total_pages} page{?s} into a single tibble.")

          return(combined)
        } else {
          cli::cli_alert_warning("No valid text clusters found on any page.")
          return(tibble::tibble(page = integer(), .cluster = factor(), word_count = integer(), text = character()))
        }
      } else {
        cli::cli_alert_warning("Empty list provided, returning empty tibble.")
        return(tibble::tibble(page = integer(), .cluster = factor(), word_count = integer(), text = character()))
      }
    } else {
      # Count successful and empty pages
      successful_pages <- sum(!purrr::map_lgl(results, function(x) is.null(x) || nrow(x) == 0))
      empty_pages <- total_pages - successful_pages

      # CLI message
      cli::cli_alert_success("Text successfully extracted from {successful_pages} page{?s}.")
      if (empty_pages > 0) {
        cli::cli_alert_warning("{empty_pages} page{?s} contain no text clusters.")
      }

      # Return the list as before when combine=FALSE
      return(results)
    }
  } else {
    # Single page processing (unchanged)
    return(pdf_extract_clusters_text_page(pdf_data))
  }
}

#' Export the Text Per Cluster on a single page
#'
#' @param pdf_data the result of [pdf_detect_clusters()]
#'
#' @return a Tibble with the same number of records as the number of detected
#'   clusters on the page
#' @noRd
pdf_extract_clusters_text_page <- function(pdf_data){
  # Return empty tibble if input is empty or NULL
  if(is.null(pdf_data) || nrow(pdf_data) == 0) {
    cli::cli_alert_warning("Empty page data provided, returning empty tibble.")
    return(tibble::tibble(.cluster = factor(), word_count = integer(), text = character()))
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
