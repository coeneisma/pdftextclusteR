test_that("ggplot output is created", {
  expect_error(pdf_plot_clusters())
  expect_error(pdf_plot_clusters(npo[1:2]), "pdf_detect_clusters")
  expect_s3_class(npo[[12]] |>
                    pdf_detect_clusters(verbose = FALSE) |>
                    pdf_plot_clusters(),
                  c("gg", "ggplot"))
})

test_that("a document returns a list of plots", {
  plots <- npo[1:2] |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_plot_clusters(verbose = FALSE)
  expect_type(plots, "list")
  expect_length(plots, 2)
})
