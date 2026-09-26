test_that("verbose = FALSE suppresses all messages", {
  expect_no_message(pdf_detect_clusters(burgerschap[[12]], verbose = FALSE))
  expect_no_message(pdf_detect_clusters(burgerschap[1:2], verbose = FALSE))

  clusters <- pdf_detect_clusters(burgerschap[1:2], verbose = FALSE)
  expect_no_message(pdf_extract_clusters(clusters, verbose = FALSE))
  expect_no_message(pdf_plot_clusters(clusters, verbose = FALSE))
})

test_that("messages are shown by default", {
  expect_message(pdf_detect_clusters(burgerschap[[12]]))
  expect_message(pdf_detect_clusters(burgerschap[1:2]))
})

test_that("the pdftextclusteR.verbose option suppresses messages", {
  old <- options(pdftextclusteR.verbose = FALSE)
  on.exit(options(old))
  expect_no_message(pdf_detect_clusters(burgerschap[[12]]))
})

test_that("a verbose argument overrides the package option", {
  old <- options(pdftextclusteR.verbose = FALSE)
  on.exit(options(old))
  expect_message(pdf_detect_clusters(burgerschap[[12]], verbose = TRUE))
})
