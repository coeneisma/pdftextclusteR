#' Plot Detected Clusters
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Plots the clusters detected with [pdf_detect_clusters()]. Each cluster
#' is assigned a unique color and number, making it easy to visually
#' compare the result with the original PDF.
#'
#' @param x a [PdfDocument] whose pages have been clustered with
#'   [pdf_detect_clusters()], or a single [PdfClusters] page.
#' @param verbose logical; if `FALSE`, informational messages are
#'   suppressed. Defaults to the package option `pdftextclusteR.verbose`,
#'   or `TRUE` when that option is not set.
#' @param ... not used.
#'
#' @return A ggplot2 plot for a single page; a list of ggplot2 plots (one
#'   per page, `NULL` for pages without text) for a document.
#' @export
#'
#' @examples
#' # A single page
#' npo[[12]] |>
#'   pdf_detect_clusters() |>
#'   pdf_plot_clusters()
#'
#' # A list of pages
#' npo[1:3] |>
#'   pdf_detect_clusters() |>
#'   pdf_plot_clusters()
pdf_plot_clusters <- S7::new_generic(
  "pdf_plot_clusters", "x",
  function(x, verbose = getOption("pdftextclusteR.verbose", TRUE), ...) {
    S7::S7_dispatch()
  }
)

S7::method(pdf_plot_clusters, PdfClusters) <- function(
    x, verbose = getOption("pdftextclusteR.verbose", TRUE), ...) {
  if (nrow(x@words) == 0) {
    cli::cli_abort("The provided page contains no text and cannot be plotted.")
  }
  pdf_plot_clusters_page(x@words, number = x@number)
}

S7::method(pdf_plot_clusters, PdfDocument) <- function(
    x, verbose = getOption("pdftextclusteR.verbose", TRUE), ...) {

  if (!all(vapply(x@pages, S7::S7_inherits, logical(1), class = PdfClusters))) {
    cli::cli_abort("No clusters detected yet. Run {.fn pdf_detect_clusters} first.")
  }

  total_pages <- length(x@pages)
  plots <- lapply(x@pages, function(page) {
    if (nrow(page@words) == 0) NULL else pdf_plot_clusters_page(page@words, number = page@number)
  })

  successful_plots <- sum(!vapply(plots, is.null, logical(1)))
  failed_plots <- total_pages - successful_plots
  if (verbose) {
    cli::cli_alert_success("Successfully plotted {successful_plots} page{?s}.")
    if (failed_plots > 0) {
      cli::cli_alert_danger("{failed_plots} page{?s} could not be plotted because they contain no text.")
    }
  }
  plots
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
pdf_plot_clusters_page <- function(pdf_data_page_clusters, number = NA){

  # Check if the page is empty
  if (nrow(pdf_data_page_clusters) == 0) {
    cli::cli_alert_danger("This page contains no text and cannot be plotted.")
    return(NULL)
  }

  plot_title <- if (is.na(number)) "Detected clusters on page" else
    sprintf("Detected clusters on page %s", number)

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
                  title = plot_title) +
    ggplot2::theme_bw()
}
