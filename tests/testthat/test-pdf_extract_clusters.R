test_that("result is tibble with variables .cluster, wordcount and text per cluster", {
  expect_named(
    npo[[12]] |>
      pdf_detect_clusters() |>
      pdf_extract_clusters(), c(".cluster", "word_count", "text"))
})

test_that("noise words are excluded by default and included with include_noise = TRUE", {
  clusters <- pdf_detect_clusters(npo[[12]], verbose = FALSE)
  # npo page 12 contains noise words (.cluster == 0)
  expect_true(any(clusters$.cluster == 0))

  extracted_default <- pdf_extract_clusters(clusters, verbose = FALSE)
  expect_false(any(extracted_default$.cluster == 0))

  extracted_noise <- pdf_extract_clusters(clusters, include_noise = TRUE, verbose = FALSE)
  expect_true(any(extracted_noise$.cluster == 0))
  expect_equal(nrow(extracted_noise), nrow(extracted_default) + 1)
})

test_that("combine = TRUE returns a single tibble with a page column", {
  extracted <- head(npo, 3) |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_extract_clusters(verbose = FALSE)
  expect_s3_class(extracted, "tbl_df")
  expect_named(extracted, c("page", ".cluster", "word_count", "text"))
  expect_setequal(unique(extracted$page), 1:3)
})

test_that("combine = FALSE returns a list of tibbles", {
  extracted <- head(npo, 3) |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_extract_clusters(combine = FALSE, verbose = FALSE)
  expect_type(extracted, "list")
  expect_length(extracted, 3)
  expect_s3_class(extracted[[1]], "tbl_df")
})
