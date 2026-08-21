# A small PDF generated with the pdf() graphics device, so pdf_read() can be
# tested without bundled files or internet access.
make_test_pdf <- function() {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = 8, height = 11)
  graphics::plot.new()
  graphics::text(0.5, 0.9, "Hello world example")
  graphics::text(0.5, 0.5, "A second line of text")
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Second page")
  grDevices::dev.off()
  path
}

test_that("pdf_read reads a PDF into a PdfDocument", {
  path <- make_test_pdf()
  doc <- pdf_read(path)
  expect_true(S7::S7_inherits(doc, PdfDocument))
  expect_length(doc, 2)
  expect_equal(doc[[1]]@number, 1L)
  expect_false(is.na(doc[[1]]@width))
  expect_true("Hello" %in% doc[[1]]@words$text)
  expect_true("font_name" %in% names(doc[[1]]@words))
  expect_equal(doc@source, path)
})

test_that("a file path can be passed directly to pdf_detect_clusters", {
  path <- make_test_pdf()
  result <- pdf_detect_clusters(path, verbose = FALSE)
  expect_true(S7::S7_inherits(result, PdfDocument))
  expect_true(S7::S7_inherits(result[[1]], PdfClusters))
})
