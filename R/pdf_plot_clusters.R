#' Plot the [pdf_detect_clusters()] Object
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function plots the clusters that are detected using the
#' [pdf_detect_clusters()] function. Each cluster is assigned a unique color and
#' number, making them easy to visually detect and compare with the original
#' PDF.
#'
#' The function works on both a single page (as a list item from the result of
#' [pdf_detect_clusters()]) and a list of pages (the entire output of
#' [pdf_detect_clusters()]). When applied to a list of pages, the function
#' returns a list of ggplot2 objects, one for each page.
#'
#' This flexibility allows users to visualize clusters for specific pages or for
#' the entire document.
#'
#' @param pdf_data_clusters A single list item from the result of
#'   [pdf_detect_clusters()], or the full list of pages returned by
#'   [pdf_detect_clusters()].
#'
#' @return A ggplot2 rectangle plot when applied to a single page. When applied
#'   to a list of pages, a list of ggplot2 rectangle plots is returned.
#' @export
#'
#' @examples
#' # Example for a single page
#' npo[[12]] |>
#'   pdf_detect_clusters() |>
#'   pdf_plot_clusters()
#'
#' # Example for a list of pages
#' npo |>
#'   head(3) |>
#'   pdf_detect_clusters() |>
#'   pdf_plot_clusters()
pdf_plot_clusters <- function(pdf_data_clusters)
{
  # Check if input is a list or single data.frame
  if (!is.data.frame(pdf_data_clusters)) {

    # Count total number of pages
    total_pages <- length(pdf_data_clusters)

    # Process all pages safely
    plots <- purrr::map(pdf_data_clusters, function(page_data) {
      if (is.null(page_data) || !is.data.frame(page_data) || nrow(page_data) == 0) {
        return(NULL)  # Return NULL for empty pages
      } else {
        return(pdf_plot_clusters_page(page_data))
      }
    })

    # Count successful and failed plots
    successful_plots <- sum(!purrr::map_lgl(plots, is.null))
    failed_plots <- total_pages - successful_plots

    # CLI messages
    cli::cli_alert_info("Total pages provided: {total_pages}")
    cli::cli_alert_success("Successfully plotted {successful_plots} page{?s}.")
    if (failed_plots > 0) {
      cli::cli_alert_danger("{failed_plots} page{?s} could not be plotted because they contain no text.")
    }

    return(plots)

  } else {

    # Check if the single page is empty or NULL
    if (is.null(pdf_data_clusters) || nrow(pdf_data_clusters) == 0) {
      cli::cli_abort("The provided page contains no text and cannot be plotted.")
    }

    return(pdf_plot_clusters_page(pdf_data_clusters))
  }
}


#' Plot one page of the [pdf_detect_clusters()] Object
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function plots a page where the clusters are detected using the
#' [pdf_detect_clusters()] function. Each cluster is assigned a unique color and
#' number, making them easy to visually detect and compare with the original
#' PDF.
#'
#' @param pdf_data_page_clusters a single list-item from the result of
#'   [pdf_detect_clusters()]
#'
#' @return a ggplot2 rectangle plot.
#' @noRd
#'
#' @examples
#' npo[[12]] |>
#'   pdf_detect_clusters() |>
#'   pdf_plot_clusters()
pdf_plot_clusters_page <- function(pdf_data_page_clusters){

  # Check if the page is empty
  if (nrow(pdf_data_page_clusters) == 0) {
    cli::cli_alert_danger("This page contains no text and cannot be plotted.")
    return(NULL)
  }

  # Data for outlines
  merged_data <- pdf_data_page_clusters |>
    dplyr::group_by(.cluster) |>
    dplyr::summarise(
      xmin = min(x),
      xmax = max(x + width),
      ymin = min(y),
      ymax = max(y + height),
      .groups = 'drop'
    ) |>
    dplyr::mutate(
      width = xmax - xmin,
      height = ymax - ymin,
      x_center = (xmin + xmax) / 2,  # X-coordinate for the label
      y_center = (ymin + ymax) / 2  # Y-coordinate for the label
    )

  # Combined plot
  ggplot2::ggplot() +
    # Outline layer
    ggplot2::geom_rect(
      data = merged_data |>
        dplyr::filter(.cluster != 0),
      ggplot2::aes(
        fill = .cluster,
        xmin = xmin - 5,
        xmax = xmax + 5,
        ymin = ymin - 5,
        ymax = ymax + 5
      ),
      colour = "black",
      alpha = 0.3  # Make transparent to distinguish layers
    ) +
    # Detail layer
    ggplot2::geom_rect(
      data = pdf_data_page_clusters,
      ggplot2::aes(
        fill = .cluster,
        xmin = x,
        xmax = x + width,
        ymin = y,
        ymax = y + height
      ),
      colour = "black"
    ) +
    # Cluster numbers
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed() +
    ggplot2::geom_text(
      data = merged_data |>
        dplyr::filter(.cluster != 0),
      ggplot2::aes(
        x = x_center,
        y = y_center,
        label = .cluster
      ),
      color = "red", size = 8
    ) +
    ggplot2::labs(x = "X-axis",
                  y = "Y-axis",
                  title = "Detected clusters on page") +
    ggplot2::theme_bw()
}
