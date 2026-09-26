test_that("extraction is independent of the row order of the words", {
  clusters <- pdf_detect_clusters(burgerschap[[7]], verbose = FALSE)
  shuffled <- clusters
  set.seed(42)
  shuffled@words <- clusters@words[sample(nrow(clusters@words)), ]
  expect_equal(pdf_extract_clusters(clusters, verbose = FALSE),
               pdf_extract_clusters(shuffled, verbose = FALSE))
})

test_that("words are read line by line, left to right", {
  words <- tibble::tibble(
    x = c(60, 10, 110, 10, 60),      # deliberately out of order
    y = c(10, 10, 11, 30, 30),       # line 2 slightly jittered
    width = 40, height = 10,
    space = TRUE,
    text = c("b", "a", "c", "d", "e"),
    .cluster = factor(1, levels = 0:1)
  )
  result <- pdf_extract_clusters_text_page(words)
  expect_equal(result$text, "a b c\nd e")
})

test_that("dehyphenate merges words hyphenated across lines", {
  words <- tibble::tibble(
    x = c(10, 60, 10, 60),
    y = c(10, 10, 30, 30),
    width = 40, height = 10,
    space = TRUE,
    text = c("an", "exam-", "ple", "here"),
    .cluster = factor(1, levels = 0:1)
  )
  expect_equal(pdf_extract_clusters_text_page(words)$text,
               "an exam-\nple here")
  expect_equal(pdf_extract_clusters_text_page(words, dehyphenate = TRUE)$text,
               "an example here")
})
