test_that("result is a tibble with .cluster, word_count and text per cluster", {
  expect_named(
    npo[[12]] |>
      pdf_detect_clusters(verbose = FALSE) |>
      pdf_extract_clusters(verbose = FALSE),
    c(".cluster", "word_count", "text"))
})

test_that("extracting from an unclustered document errors", {
  expect_error(pdf_extract_clusters(npo[1:2]), "pdf_detect_clusters")
})

test_that("noise words are excluded by default and included with include_noise = TRUE", {
  clusters <- pdf_detect_clusters(npo[[12]], verbose = FALSE)
  expect_true(any(clusters@words$.cluster == 0))

  extracted_default <- pdf_extract_clusters(clusters, verbose = FALSE)
  expect_false(any(extracted_default$.cluster == 0))

  extracted_noise <- pdf_extract_clusters(clusters, include_noise = TRUE, verbose = FALSE)
  expect_true(any(extracted_noise$.cluster == 0))
  expect_equal(nrow(extracted_noise), nrow(extracted_default) + 1)
})

test_that("combine = TRUE returns a single tibble with the real page numbers", {
  extracted <- npo[2:4] |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_extract_clusters(verbose = FALSE)
  expect_s3_class(extracted, "tbl_df")
  expect_named(extracted, c("page", ".cluster", "word_count", "text"))
  expect_setequal(unique(extracted$page), 2:4)
})

test_that("combine = FALSE returns a list of tibbles", {
  extracted <- npo[1:3] |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_extract_clusters(combine = FALSE, verbose = FALSE)
  expect_type(extracted, "list")
  expect_length(extracted, 3)
  expect_s3_class(extracted[[1]], "tbl_df")
})
