test_that("PdfDocument supports length, [[ and [", {
  expect_equal(length(npo), 99L)
  expect_true(S7::S7_inherits(npo[[12]], PdfPage))
  sub <- npo[3:5]
  expect_true(S7::S7_inherits(sub, PdfDocument))
  expect_length(sub, 3)
  expect_equal(sub[[1]]@number, 3L)
})

test_that("as_tibble works on pages and documents", {
  words <- tibble::as_tibble(npo[[12]])
  expect_s3_class(words, "tbl_df")
  expect_true(all(c("x", "y", "text", "font_name", "font_size") %in% names(words)))

  doc_words <- tibble::as_tibble(npo[1:2])
  expect_equal(names(doc_words)[1], "page")
})

test_that("pages carry number and dimensions", {
  p <- npo[[12]]
  expect_equal(p@number, 12L)
  expect_false(is.na(p@width))
  expect_false(is.na(p@height))
})

test_that("PdfDocument validates its pages", {
  expect_error(PdfDocument(pages = list(1, 2)), "PdfPage")
})

test_that("print methods give a compact summary", {
  expect_output(print(npo), "<PdfDocument> 99 pages")
  expect_output(print(npo[[12]]), "<PdfPage> page 12")
  clusters <- pdf_detect_clusters(npo[[12]], verbose = FALSE)
  expect_output(print(clusters), "<PdfClusters> page 12")
})

test_that("a clustered document prints its algorithm", {
  clustered <- pdf_detect_clusters(npo[1:2], verbose = FALSE)
  expect_output(print(clustered), "algorithm: dbscan")
  expect_output(print(npo[1:2]), "<PdfDocument>")
})

test_that("plot, autoplot, summary and as.data.frame methods work", {
  clusters <- pdf_detect_clusters(npo[[12]], verbose = FALSE)
  expect_s3_class(plot(clusters), c("gg", "ggplot"))
  expect_s3_class(ggplot2::autoplot(clusters), c("gg", "ggplot"))

  doc <- pdf_detect_clusters(npo[1:2], verbose = FALSE)
  expect_type(plot(doc, verbose = FALSE), "list")

  overview <- expect_output(summary(doc), "clusters per page")
  expect_named(overview, c("page", "words", "clusters"))

  df <- as.data.frame(npo[[12]])
  expect_s3_class(df, "data.frame")
  expect_equal(names(as.data.frame(npo[1:2]))[1], "page")
})

test_that("summary works on pages and clustered pages", {
  fonts <- summary(npo[[12]])
  expect_named(fonts, c("font_name", "font_size", "words"))

  clusters <- pdf_detect_clusters(npo[[12]], verbose = FALSE)
  per_cluster <- summary(clusters)
  expect_named(per_cluster, c(".cluster", "words", "text"))
  expect_false(any(per_cluster$.cluster == 0))
})

test_that("summary of an empty document does not warn", {
  expect_no_warning(summary(PdfDocument()))
})
