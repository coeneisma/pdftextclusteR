test_that("result is tibble with variables .cluster, wordcount and text per cluster", {
  expect_named(
    npo[[12]] |>
      pdf_detect_clusters() |>
      pdf_extract_clusters(), c(".cluster", "word_count", "text"))
})
