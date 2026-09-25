# Synthetic 5-page document with header, heading, body and page number
make_typed_page <- function(n) {
  word_row <- function(cluster, text, x, y, size = 10, font = "Test-Regular") {
    tibble::tibble(width = nchar(text) * size * 0.5, height = size,
                   x = x, y = y, space = TRUE, text = text,
                   font_name = font, font_size = size, .cluster = cluster)
  }
  body_words <- dplyr::bind_rows(lapply(1:30, function(i) {
    word_row(3, paste0("woord", i),
             x = 50 + (i %% 6) * 80, y = 340 + (i %/% 6) * 20)
  }))
  words <- dplyr::bind_rows(
    word_row(1, "Jaarrapport", 50, 20),
    word_row(1, "Testdocument", 120, 20),
    word_row(2, "Sectie", 50, 300, size = 20, font = "Test-Bold"),
    word_row(2, as.character(n), 130, 300, size = 20, font = "Test-Bold"),
    body_words,
    word_row(4, as.character(n), 300, 780)
  )
  words$.cluster <- factor(words$.cluster, levels = 0:4)
  PdfClusters(words = words, number = as.integer(n), width = 600, height = 800,
              algorithm = "dbscan", params = list())
}

make_typed_doc <- function(n_pages = 5) {
  PdfDocument(pages = lapply(seq_len(n_pages), make_typed_page))
}

test_that("headers, headings, body and page numbers are classified", {
  classified <- pdf_classify_clusters(make_typed_doc(), verbose = FALSE)
  w <- classified[[2]]@words
  types <- unique(w[, c(".cluster", ".type")])
  type_of <- function(txt) as.character(w$.type[w$text == txt][1])
  expect_equal(type_of("Jaarrapport"), "page_header")
  expect_equal(type_of("Sectie"), "heading")
  expect_equal(type_of("woord1"), "body")
  expect_equal(w$.type_level[w$text == "Sectie"][1], 1L)
  # standalone page number at the bottom, tracking the page index
  expect_equal(as.character(w$.type[w$y == 780][1]), "page_number")
})

test_that("page furniture is ordered after the reading flow", {
  classified <- pdf_classify_clusters(make_typed_doc(), verbose = FALSE)
  w <- classified[[1]]@words
  cluster_of <- function(txt) as.integer(as.character(w$.cluster[w$text == txt][1]))
  expect_equal(cluster_of("Sectie"), 1)      # heading first
  expect_equal(cluster_of("woord1"), 2)      # body second
  expect_gt(cluster_of("Jaarrapport"), 2)    # furniture after the flow
})

test_that("exclude drops types from the extraction and adds .type columns", {
  classified <- pdf_classify_clusters(make_typed_doc(), verbose = FALSE)
  all_text <- pdf_extract_clusters(classified, verbose = FALSE)
  expect_true(all(c(".type", ".type_level") %in% names(all_text)))

  clean <- pdf_extract_clusters(
    classified, exclude = c("page_header", "page_number"), verbose = FALSE)
  expect_false(any(clean$.type %in% c("page_header", "page_number")))
  expect_true(any(all_text$.type == "page_header"))
})

test_that("exclude without classification is a clear error", {
  clusters <- pdf_detect_clusters(npo[[12]], verbose = FALSE)
  expect_error(pdf_extract_clusters(clusters, exclude = "page_footer"),
               "pdf_classify_clusters")
})

test_that("classifying a single page warns about missing document signals", {
  page <- make_typed_page(1)
  expect_message(pdf_classify_clusters(page), "single page")
  result <- pdf_classify_clusters(page, verbose = FALSE)
  expect_true(S7::S7_inherits(result, PdfClusters))
  expect_true(".type" %in% names(result@words))
})

test_that("color_by .type works after classification and errors before", {
  classified <- pdf_classify_clusters(make_typed_doc(), verbose = FALSE)
  p <- pdf_plot_clusters(classified[[1]], color_by = ".type")
  expect_s3_class(p, c("gg", "ggplot"))

  clusters <- pdf_detect_clusters(npo[[12]], verbose = FALSE)
  expect_error(pdf_plot_clusters(clusters, color_by = ".type"),
               "pdf_classify_clusters")
})

test_that("footers in a real document are detected via repetition", {
  res <- cibap[15:20] |>
    pdf_detect_clusters(verbose = FALSE) |>
    pdf_classify_clusters(verbose = FALSE)
  types <- unlist(lapply(res@pages, function(p) as.character(p@words$.type)))
  expect_true("page_footer" %in% types)
})

test_that("pdf_extract_text runs the whole pipeline", {
  res <- pdf_extract_text(make_typed_doc(), verbose = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("page", ".cluster", ".type", "word_count", "text") %in% names(res)))
  expect_false(any(res$.type %in% c("page_header", "page_footer", "page_number")))
})

test_that("auto margins find footers outside the fixed bands", {
  # Footer at 81% of the page height: outside the fixed bottom band
  # (12%), inside the wide auto band (30%) at a stable position
  add_footer <- function(page, n) {
    footer <- tibble::tibble(width = 60, height = 10, x = 50, y = 650,
                             space = TRUE, text = c("Rapportage", "vergezicht"),
                             font_name = "Test-Regular", font_size = 10,
                             .cluster = factor(5, levels = 0:5))
    words <- dplyr::bind_rows(dplyr::mutate(page@words,
                                            .cluster = factor(.cluster, levels = 0:5)),
                              footer)
    PdfClusters(words = words, number = page@number, width = 600, height = 800,
                algorithm = "dbscan", params = list())
  }
  doc <- make_typed_doc()
  doc@pages <- lapply(seq_along(doc@pages),
                      function(i) add_footer(doc@pages[[i]], i))

  auto <- pdf_classify_clusters(doc, verbose = FALSE)
  w <- auto[[1]]@words
  expect_equal(as.character(w$.type[w$text == "Rapportage"][1]), "page_footer")

  fixed <- pdf_classify_clusters(doc, rules = pdf_type_rules(margins = "fixed"),
                                 verbose = FALSE)
  w <- fixed[[1]]@words
  expect_equal(as.character(w$.type[w$text == "Rapportage"][1]), "body")
})

test_that("auto margins ignore repeated text at unstable positions", {
  # The same masked text recurs in the top band, but at a different
  # height on every page: not furniture
  make_wandering_page <- function(n) {
    words <- dplyr::bind_rows(
      tibble::tibble(width = 60, height = 20, x = 50, y = 60 + n * 30,
                     space = TRUE, text = c("Sectie", as.character(n)),
                     font_name = "Test-Bold", font_size = 20,
                     .cluster = factor(1, levels = 0:2)),
      dplyr::bind_rows(lapply(1:20, function(i) {
        tibble::tibble(width = 40, height = 10,
                       x = 50 + (i %% 5) * 80, y = 400 + (i %/% 5) * 20,
                       space = TRUE, text = paste0("woord", i),
                       font_name = "Test-Regular", font_size = 10,
                       .cluster = factor(2, levels = 0:2))
      }))
    )
    PdfClusters(words = words, number = as.integer(n), width = 600,
                height = 800, algorithm = "dbscan", params = list())
  }
  doc <- PdfDocument(pages = lapply(1:5, make_wandering_page))
  classified <- pdf_classify_clusters(doc, verbose = FALSE)
  w <- classified[[3]]@words
  expect_equal(as.character(w$.type[w$text == "Sectie"][1]), "heading")
})

test_that("custom rules are respected", {
  # An extreme heading threshold: nothing qualifies as heading
  rules <- pdf_type_rules(heading_min_size_ratio = 10,
                          heading_bold_size_ratio = 10)
  classified <- pdf_classify_clusters(make_typed_doc(), rules = rules,
                                      verbose = FALSE)
  w <- classified[[1]]@words
  expect_false("heading" %in% as.character(w$.type))
})

test_that("isolated page numbers (noise words) are detected and promoted", {
  # A standalone page number is a single isolated word: dbscan labels it
  # noise (.cluster = 0), so classification must consider noise words too
  add_noise_number <- function(page, n) {
    number <- tibble::tibble(width = 15, height = 10, x = 300, y = 780,
                             space = FALSE, text = as.character(n),
                             font_name = "Test-Regular", font_size = 10,
                             .cluster = factor(0, levels = 0:4))
    words <- page@words[page@words$y != 780, ]  # drop the clustered number
    words <- dplyr::bind_rows(words, number)
    PdfClusters(words = words, number = page@number, width = 600, height = 800,
                algorithm = "dbscan", params = list())
  }
  doc <- make_typed_doc()
  doc@pages <- lapply(seq_along(doc@pages),
                      function(i) add_noise_number(doc@pages[[i]], i))

  classified <- pdf_classify_clusters(doc, verbose = FALSE)
  w <- classified[[2]]@words
  number_row <- w[w$y == 780, ]
  expect_equal(as.character(number_row$.type), "page_number")
  expect_true(number_row$.cluster != 0)
  # promoted cluster sits after the reading flow
  expect_equal(as.integer(as.character(number_row$.cluster)),
               max(as.integer(as.character(w$.cluster))))
  # and the default pipeline excludes it
  text <- pdf_extract_text(doc, verbose = FALSE)
  expect_false(any(text$text == "2"))
})
