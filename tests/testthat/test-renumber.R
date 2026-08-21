# Helper: build a synthetic clustered page. Each cluster is a block of words
# starting at (x, y); coordinates follow the pdftools convention (y measured
# from the top of the page).
make_page <- function(clusters) {
  rows <- lapply(seq_along(clusters), function(i) {
    cl <- clusters[[i]]
    n_words <- 4
    tibble::tibble(
      width = 40,
      height = 10,
      x = cl$x + (seq_len(n_words) - 1) * 45,
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

test_that("clusters in two columns are renumbered column by column, top to bottom", {
  # Detection order (cluster ids) deliberately differs from reading order:
  #   left column: id 3 (top), id 1 (bottom); right column: id 4 (top), id 2 (bottom)
  page <- make_page(list(
    list(id = 3, x = 10,  y = 10),
    list(id = 1, x = 10,  y = 200),
    list(id = 4, x = 400, y = 10),
    list(id = 2, x = 400, y = 200)
  ))
  renumbered <- pdf_renumber_clusters_page(page)
  mapping <- unique(renumbered[, c("text", ".cluster")])
  get_new <- function(old_id) {
    unique(renumbered$.cluster[grepl(paste0("^cl", old_id, "w"), renumbered$text)])
  }
  expect_equal(as.character(get_new(3)), "1")  # left top
  expect_equal(as.character(get_new(1)), "2")  # left bottom
  expect_equal(as.character(get_new(4)), "3")  # right top
  expect_equal(as.character(get_new(2)), "4")  # right bottom
})

test_that("a single column is renumbered top to bottom", {
  page <- make_page(list(
    list(id = 2, x = 10, y = 300),
    list(id = 1, x = 10, y = 150),
    list(id = 3, x = 10, y = 10)
  ))
  renumbered <- pdf_renumber_clusters_page(page)
  get_new <- function(old_id) {
    unique(as.character(renumbered$.cluster[grepl(paste0("^cl", old_id, "w"), renumbered$text)]))
  }
  expect_equal(get_new(3), "1")
  expect_equal(get_new(1), "2")
  expect_equal(get_new(2), "3")
})

test_that("noise words keep cluster 0 after renumbering", {
  page <- make_page(list(
    list(id = 0, x = 500, y = 700),
    list(id = 2, x = 10, y = 10),
    list(id = 1, x = 10, y = 200)
  ))
  renumbered <- pdf_renumber_clusters_page(page)
  noise <- renumbered[grepl("^cl0w", renumbered$text), ]
  expect_true(all(noise$.cluster == 0))
})

# Documents current behaviour of a full-width banner above two columns.
# The banner shares its x-position with the left column and is therefore
# sorted within that column. A future reading-order implementation should
# place the banner before both columns; update this snapshot then.
test_that("banner above two columns: current column-based renumbering", {
  page <- make_page(list(
    list(id = 1, x = 10,  y = 10),   # full-width banner (starts at left margin)
    list(id = 2, x = 10,  y = 100),  # left column
    list(id = 3, x = 400, y = 100)   # right column
  ))
  renumbered <- pdf_renumber_clusters_page(page)
  mapping <- vapply(1:3, function(old_id) {
    unique(as.character(renumbered$.cluster[grepl(paste0("^cl", old_id, "w"), renumbered$text)]))
  }, character(1))
  expect_snapshot(mapping)
})
