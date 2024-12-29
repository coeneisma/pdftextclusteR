test_that("ggplot output is created", {
  expect_error(pdf_plot_clusters())
  expect_error(pdf_plot_clusters(npo))
  expect_s3_class(npo[[12]] |>
                    pdf_detect_clusters() |>
                    pdf_plot_clusters(),
                  c("gg", "ggplot"))
})
