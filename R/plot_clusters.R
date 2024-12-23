#' Plot the [detect_clusters()] Object
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function plots a page where the clusters are detected using the
#' [detect_clusters()] function. Each cluster is assigned a unique color and
#' number, making them easy to visually detect and compare with the original
#' PDF.
#'
#' @param pdf_data_page_clusters a single list-item from the result of
#'   [detect_clusters()]
#'
#' @return a ggplot2 rectangle plot.
#' @export
#'
#' @examples
#' npo[[12]] |>
#'   detect_clusters() |>
#'   plot_clusters()
plot_clusters <- function(pdf_data_page_clusters){

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

