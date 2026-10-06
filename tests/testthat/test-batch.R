# Two small PDFs in a temporary directory, for batch processing tests
make_pdf_dir <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  for (name in c("eerste", "tweede")) {
    grDevices::pdf(file.path(dir, paste0(name, ".pdf")), width = 8, height = 11)
    graphics::plot.new()
    graphics::text(0.5, 0.9, paste("Dit is document", name))
    graphics::text(0.5, 0.5, "Nog een regel tekst hier")
    grDevices::dev.off()
  }
  dir
}

test_that("a directory of PDFs is processed into one tibble", {
  dir <- make_pdf_dir()
  res <- pdf_extract_text(dir, verbose = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(names(res)[1:2], c("document", "page"))
  expect_setequal(unique(res$document), c("eerste.pdf", "tweede.pdf"))
})

test_that("a vector of paths is processed into one tibble", {
  dir <- make_pdf_dir()
  paths <- list.files(dir, full.names = TRUE)
  res <- pdf_extract_text(paths, verbose = FALSE)
  expect_setequal(unique(res$document), basename(paths))
})

test_that("an unreadable file is skipped with a warning", {
  dir <- make_pdf_dir()
  writeLines("dit is geen pdf", file.path(dir, "kapot.pdf"))
  expect_warning(res <- pdf_extract_text(dir, verbose = FALSE),
                 "could not be processed")
  expect_setequal(unique(res$document), c("eerste.pdf", "tweede.pdf"))
})

test_that("an empty directory is a clear error", {
  dir <- withr::local_tempdir()
  expect_error(pdf_extract_text(dir), "No PDF files")
})

test_that("OCR arguments reach pdf_read instead of the cluster algorithm", {
  skip_if_not_installed("tesseract")
  # Regression: ocr_language used to fall through ... into dbscan and crash
  expect_no_error(
    suppressWarnings(pdf_extract_text(
      testthat::test_path("fixtures", "scanned.pdf"),
      ocr = "never", ocr_language = "nld", verbose = FALSE))
  )
})
