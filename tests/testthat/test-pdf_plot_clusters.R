test_that("ggplot output is created", {
  expect_error(pdf_plot_clusters())
  expect_error(pdf_plot_clusters(burgerschap[1:2]), "pdf_detect_clusters")
  expect_s3_class(burgerschap[[12]] |>
                    pdf_detect_clusters(verbose = FALSE) |>
                    pdf_plot_clusters(),
                  c("gg", "ggplot"))
})

test_that("a document returns a list of plots", {
  plots <- burgerschap[1:2] |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_plot_clusters(verbose = FALSE)
  expect_type(plots, "list")
  expect_length(plots, 2)
})

test_that("show_order adds a path layer", {
  clusters <- pdf_detect_clusters(burgerschap[[12]], verbose = FALSE)
  p_plain <- pdf_plot_clusters(clusters)
  p_order <- pdf_plot_clusters(clusters, show_order = TRUE)
  expect_length(p_order$layers, length(p_plain$layers) + 2)
})
