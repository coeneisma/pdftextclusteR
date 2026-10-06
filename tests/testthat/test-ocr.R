scanned_pdf <- testthat::test_path("fixtures", "scanned.pdf")

# Pick an installed tesseract language (CI images differ); NULL when the
# engine cannot start at all (e.g. no training data installed)
ocr_lang <- function() {
  for (lang in c("eng", "nld")) {
    ok <- tryCatch({
      tesseract::tesseract(lang)
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(lang)
  }
  NULL
}

test_that("ocr = 'never' leaves scanned pages empty", {
  doc <- pdf_read(scanned_pdf, ocr = "never")
  expect_equal(nrow(doc[[1]]@words), 0)
})

test_that("scanned pages are read through OCR", {
  skip_if_not_installed("tesseract")
  lang <- ocr_lang()
  skip_if(is.null(lang), "no tesseract training data installed")

  doc <- suppressMessages(pdf_read(scanned_pdf, ocr_language = lang))
  words <- doc[[1]]@words
  expect_gt(nrow(words), 3)
  expect_true(all(c("x", "y", "width", "height", "text", "ocr_confidence")
                  %in% names(words)))
  # coordinates are converted to points and fall inside the page
  expect_true(all(words$x + words$width <= doc[[1]]@width + 1))
  expect_true(all(words$y + words$height <= doc[[1]]@height + 1))

  # the rest of the pipeline works on OCR-ed pages
  clusters <- pdf_detect_clusters(doc[[1]], verbose = FALSE)
  extracted <- pdf_extract_clusters(clusters, verbose = FALSE)
  expect_gt(nrow(extracted), 0)
})

test_that("ocr = 'always' without tesseract is an error", {
  skip_if(requireNamespace("tesseract", quietly = TRUE),
          "tesseract is installed")
  expect_error(pdf_read(scanned_pdf, ocr = "always"), "tesseract")
})

test_that("a failing OCR engine warns instead of erroring", {
  skip_if_not_installed("tesseract")
  expect_warning(
    doc <- pdf_read(scanned_pdf, ocr_language = "xx_nonexistent"),
    "OCR failed")
  expect_equal(nrow(doc[[1]]@words), 0)
})

test_that("the OCR language can be set via a package option", {
  skip_if_not_installed("tesseract")
  withr::local_options(pdftextclusteR.ocr_language = "nld")
  expect_message(
    suppressWarnings(pdf_read(scanned_pdf)),
    "language: nld")
})
