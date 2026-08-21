# Snapshot tests: change-detection net for the detect -> extract pipeline.
# A compact, readable summary is snapshotted instead of the full text.
summarise_extraction <- function(extracted) {
  extracted$text <- paste0(substr(extracted$text, 1, 60), "...")
  as.data.frame(extracted)
}

test_that("extraction of npo page 7 is stable", {
  res <- npo[[7]] |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_extract_clusters(verbose = FALSE)
  expect_snapshot(summarise_extraction(res))
})

test_that("extraction of npo page 12 is stable", {
  res <- npo[[12]] |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_extract_clusters(verbose = FALSE)
  expect_snapshot(summarise_extraction(res))
})

test_that("extraction of cibap page 5 is stable", {
  res <- cibap[[5]] |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_extract_clusters(verbose = FALSE)
  expect_snapshot(summarise_extraction(res))
})

test_that("an empty page in the middle of a document is handled", {
  pages <- list(npo[[7]], npo[[7]][0, ], npo[[12]])
  detected <- suppressMessages(pdf_detect_clusters(pages, verbose = FALSE))
  expect_length(detected, 3)
  expect_null(detected[[2]])

  extracted <- suppressMessages(pdf_extract_clusters(detected, verbose = FALSE))
  expect_setequal(unique(extracted$page), c(1, 3))
})
