test_that("an error is thrown for unsupported input", {
  expect_error(pdf_detect_clusters())
  expect_error(pdf_detect_clusters(42))
  expect_error(pdf_detect_clusters(list(1, 2)))
})

test_that("an error is thrown when required word columns are missing", {
  broken <- npo[[12]]
  broken@words <- dplyr::select(broken@words, -x)
  expect_error(pdf_detect_clusters(broken, verbose = FALSE),
               "missing the following required variable")
})

test_that("a document input returns a PdfDocument of PdfClusters pages", {
  result <- pdf_detect_clusters(npo[1:3], verbose = FALSE)
  expect_true(S7::S7_inherits(result, PdfDocument))
  expect_length(result, 3)
  expect_true(all(vapply(result@pages, S7::S7_inherits, logical(1),
                         class = PdfClusters)))
})

test_that("a page input returns a PdfClusters object", {
  result <- pdf_detect_clusters(npo[[12]], verbose = FALSE)
  expect_true(S7::S7_inherits(result, PdfClusters))
  expect_true(".cluster" %in% names(result@words))
  expect_equal(result@algorithm, "dbscan")
  expect_equal(result@number, 12L)
})
