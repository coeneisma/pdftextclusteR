# Helper: build a synthetic clustered page. Each cluster is a block of words
# starting at (x, y); coordinates follow the pdftools convention (y measured
# from the top of the page).
make_page <- function(clusters, n_words = 4, word_width = 40) {
  rows <- lapply(clusters, function(cl) {
    tibble::tibble(
      width = word_width,
      height = 10,
      x = cl$x + (seq_len(n_words) - 1) * (word_width + 5),
      y = cl$y,
      space = TRUE,
      text = paste0("cl", cl$id, "w", seq_len(n_words)),
      .cluster = cl$id
    )
  })
  page <- dplyr::bind_rows(rows)
  page$.cluster <- factor(page$.cluster, levels = 0:max(page$.cluster))
  page
}

# Reading order assigned to each original cluster id
new_order <- function(renumbered, old_ids) {
  vapply(old_ids, function(old_id) {
    as.integer(as.character(unique(
      renumbered$.cluster[grepl(paste0("^cl", old_id, "w"), renumbered$text)]
    )))
  }, integer(1))
}

test_that("a 2x2 grid with aligned gaps is read row by row (Z-order)", {
  # With whitespace bands aligned in both directions the geometry is a
  # grid of blocks; prefer = "rows" reads it in Z-order
  page <- make_page(list(
    list(id = 3, x = 10,  y = 10),
    list(id = 1, x = 10,  y = 200),
    list(id = 4, x = 400, y = 10),
    list(id = 2, x = 400, y = 200)
  ))
  expect_equal(new_order(order_clusters_page(page), 1:4), c(3, 4, 1, 2))
})

test_that("two text columns with unaligned blocks are read column-wise", {
  # Real text columns: the paragraph gaps in the two columns do not line
  # up, so no full-width horizontal band exists and the column cut wins
  page <- make_page(list(
    list(id = 3, x = 10,  y = 10),
    list(id = 1, x = 10,  y = 210),
    list(id = 4, x = 400, y = 110),
    list(id = 2, x = 400, y = 310)
  ))
  expect_equal(new_order(order_clusters_page(page), 1:4), c(2, 4, 1, 3))
})

test_that("a single column is renumbered top to bottom", {
  page <- make_page(list(
    list(id = 2, x = 10, y = 300),
    list(id = 1, x = 10, y = 150),
    list(id = 3, x = 10, y = 10)
  ))
  expect_equal(new_order(order_clusters_page(page), 1:3), c(2, 3, 1))
})

test_that("a full-width banner above two columns is read first", {
  page <- make_page(list(
    list(id = 1, x = 10,  y = 100),  # left column
    list(id = 2, x = 400, y = 100),  # right column
    list(id = 3, x = 10,  y = 10)    # banner across the full width
  ), n_words = 8)
  page$x[page$.cluster == 1] <- seq(10, 200, length.out = 8)
  page$x[page$.cluster == 2] <- seq(400, 590, length.out = 8)
  expect_equal(new_order(order_clusters_page(page), 1:3), c(2, 3, 1))
})

test_that("stacked column blocks are read block by block", {
  # Two columns A|B above, two columns C|D below a horizontal gap:
  # reading order A, B, C, D (column sorting would give A, C, B, D)
  page <- make_page(list(
    list(id = 1, x = 10,  y = 10),   # A
    list(id = 2, x = 400, y = 10),   # B
    list(id = 3, x = 10,  y = 200),  # C
    list(id = 4, x = 400, y = 200)   # D
  ))
  expect_equal(new_order(order_clusters_page(page), 1:4), 1:4)
})

test_that("clusters at slightly different x stay in one column", {
  # x-positions differ by a few points; there is no real whitespace band,
  # so this is one column ordered by y (the old rounding-based binning
  # could split these)
  page <- make_page(list(
    list(id = 2, x = 100, y = 10),
    list(id = 1, x = 106, y = 200)
  ))
  expect_equal(new_order(order_clusters_page(page), 1:2), c(2, 1))
})

test_that("overlapping clusters in one column sort by top, not center", {
  # A is tall (center 205), B small but its center (225) is below A's:
  # center-based sorting would swap them
  page <- make_page(list(
    list(id = 2, x = 10, y = 10),   # A: tall cluster, words at y 10..400
    list(id = 1, x = 10, y = 220)   # B: small cluster inside A's x-range
  ))
  page$y[page$text == "cl2w2"] <- 150
  page$y[page$text == "cl2w3"] <- 280
  page$y[page$text == "cl2w4"] <- 400
  expect_equal(new_order(order_clusters_page(page), 1:2), c(2, 1))
})

test_that("noise words keep cluster 0 after ordering", {
  page <- make_page(list(
    list(id = 0, x = 500, y = 700),
    list(id = 2, x = 10, y = 10),
    list(id = 1, x = 10, y = 200)
  ))
  renumbered <- order_clusters_page(page)
  expect_true(all(renumbered$.cluster[grepl("^cl0w", renumbered$text)] == 0))
})

test_that("prefer = 'columns' reads columns before rows", {
  page <- make_page(list(
    list(id = 1, x = 10,  y = 10),
    list(id = 2, x = 400, y = 10),
    list(id = 3, x = 10,  y = 200),
    list(id = 4, x = 400, y = 200)
  ))
  expect_equal(new_order(order_clusters_page(page, prefer = "columns"), 1:4),
               c(1, 3, 2, 4))
})

test_that("tolerance_factor is deprecated", {
  expect_warning(pdf_detect_clusters(npo[[12]], tolerance_factor = 0.1,
                                     verbose = FALSE),
                 class = "lifecycle_warning_deprecated")
})
