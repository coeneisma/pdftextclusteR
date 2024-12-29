test_that("error is trown when no or wrong pdf_data-object is provided", {
  expect_error(pdf_detect_clusters())
  expect_error(pdf_detect_clusters(npo[[12]] |>
                                     dplyr::select(width, height, x, y, space)),
               "The data.frame is missing the following required variable")
  expect_error(pdf_detect_clusters(npo[[12]] |>
                                     dplyr::select(width, height, x, y, text)),
               "The data.frame is missing the following required variable")
  expect_error(pdf_detect_clusters(npo[[12]] |>
                                     dplyr::select(width, height, x, space, text)),
               "The data.frame is missing the following required variable")
  expect_error(pdf_detect_clusters(npo[[12]] |>
                                     dplyr::select(width, height, y, space, text)),
               "The data.frame is missing the following required variable")
  expect_error(pdf_detect_clusters(npo[[12]] |>
                                     dplyr::select(width, x, y, space, text)),
               "The data.frame is missing the following required variable")
  expect_error(pdf_detect_clusters(npo[[12]] |>
                                     dplyr::select(height, x, y, space, text)),
               "The data.frame is missing the following required variable")
  })

test_that("list is returned when list with pdf-data-ojbects is provided", {
  expect_true(is.list(pdf_detect_clusters(head(npo, 3))))
  })

test_that("data.frame is returned when a pdf-data-object-page is provided", {
  expect_true(is.data.frame(pdf_detect_clusters(npo[[12]])))
  })
