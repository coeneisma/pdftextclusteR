#' PdfPage class
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' A single page of a PDF document: a tibble of words (as returned by
#' [pdftools::pdf_data()]) plus the page number and the page dimensions in
#' points. Created by [pdf_read()]; not usually constructed directly.
#'
#' @param words data frame with one row per word (`x`, `y`, `width`,
#'   `height`, `space`, `text` and, when available, `font_name` and
#'   `font_size`).
#' @param number the page number within the document.
#' @param width,height the page dimensions in points (`NA` when unknown).
#'
#' @export
PdfPage <- S7::new_class(
  "PdfPage",
  properties = list(
    words  = S7::new_property(S7::class_data.frame, default = quote(data.frame())),
    number = S7::new_property(S7::class_integer, default = NA_integer_),
    width  = S7::new_property(S7::class_numeric, default = NA_real_),
    height = S7::new_property(S7::class_numeric, default = NA_real_)
  )
)

#' PdfClusters class
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' A [PdfPage] whose words have been assigned to clusters by
#' [pdf_detect_clusters()]: the `words` tibble gains a `.cluster` column.
#' Cluster 0 is noise (words not assigned to any cluster).
#'
#' @param words,number,width,height see [PdfPage].
#' @param algorithm the clustering algorithm that was used.
#' @param params user-specified algorithm parameters.
#'
#' @export
PdfClusters <- S7::new_class(
  "PdfClusters",
  parent = PdfPage,
  properties = list(
    algorithm = S7::new_property(S7::class_character, default = NA_character_),
    params    = S7::class_list
  )
)

#' PdfDocument class
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' A PDF document: a list of [PdfPage] (or, after
#' [pdf_detect_clusters()], [PdfClusters]) objects plus the source path or
#' URL. Created by [pdf_read()].
#'
#' Pages can be accessed with `x[[i]]`, subsets with `x[i]`; `length(x)`
#' returns the number of pages.
#'
#' @param pages list of [PdfPage] objects.
#' @param source path or URL the document was read from.
#'
#' @export
PdfDocument <- S7::new_class(
  "PdfDocument",
  properties = list(
    pages  = S7::class_list,
    source = S7::new_property(S7::class_character, default = NA_character_)
  ),
  validator = function(self) {
    if (!all(vapply(self@pages, S7::S7_inherits, logical(1), class = PdfPage))) {
      "@pages must be a list of PdfPage objects"
    }
  }
)

# Number of clusters on a page (excluding noise); NA when not clustered
n_clusters <- function(page) {
  cl <- page@words$.cluster
  if (is.null(cl)) return(NA_integer_)
  length(unique(cl[cl != 0]))
}

S7::method(print, PdfPage) <- function(x, ...) {
  cat(sprintf("<PdfPage> page %s: %d words (%s x %s pt)\n",
              x@number, nrow(x@words),
              round(x@width), round(x@height)))
  invisible(x)
}

S7::method(print, PdfClusters) <- function(x, ...) {
  cat(sprintf("<PdfClusters> page %s: %d words in %d clusters (algorithm: %s)\n",
              x@number, nrow(x@words), n_clusters(x), x@algorithm))
  invisible(x)
}

S7::method(print, PdfDocument) <- function(x, ...) {
  n_words <- sum(vapply(x@pages, function(p) nrow(p@words), integer(1)))
  cat(sprintf("<PdfDocument> %d pages, %d words\n", length(x@pages), n_words))
  if (!is.na(x@source)) cat("  source:", x@source, "\n")
  if (length(x@pages) > 0 &&
      all(vapply(x@pages, S7::S7_inherits, logical(1), class = PdfClusters))) {
    algorithms <- unique(vapply(x@pages, function(p) p@algorithm, character(1)))
    cat("  algorithm:", paste(algorithms, collapse = ", "), "\n")
  }
  invisible(x)
}

S7::method(length, PdfDocument) <- function(x) length(x@pages)

S7::method(`[[`, PdfDocument) <- function(x, i) x@pages[[i]]

S7::method(`[`, PdfDocument) <- function(x, i) {
  PdfDocument(pages = x@pages[i], source = x@source)
}

as_tibble_external <- S7::new_external_generic("tibble", "as_tibble", "x")

S7::method(as_tibble_external, PdfPage) <- function(x, ...) {
  tibble::as_tibble(x@words)
}

S7::method(as_tibble_external, PdfDocument) <- function(x, ...) {
  pages <- lapply(x@pages, function(p) {
    dplyr::mutate(tibble::as_tibble(p@words), page = p@number, .before = 1)
  })
  dplyr::bind_rows(pages)
}

as_data_frame_external <- S7::new_external_generic("base", "as.data.frame", "x")

S7::method(as_data_frame_external, PdfPage) <- function(x, ...) {
  as.data.frame(x@words)
}

S7::method(as_data_frame_external, PdfDocument) <- function(x, ...) {
  as.data.frame(tibble::as_tibble(x))
}

autoplot_external <- S7::new_external_generic("ggplot2", "autoplot", "object")

S7::method(autoplot_external, PdfClusters) <- function(object, ...) {
  pdf_plot_clusters(object, ...)
}

S7::method(plot, PdfClusters) <- function(x, ...) {
  pdf_plot_clusters(x, ...)
}

S7::method(plot, PdfDocument) <- function(x, ...) {
  pdf_plot_clusters(x, ...)
}

S7::method(summary, PdfDocument) <- function(object, ...) {
  if (length(object@pages) == 0) {
    print(object)
    return(invisible(tibble::tibble(page = integer(), words = integer())))
  }
  words_per_page <- vapply(object@pages, function(p) nrow(p@words), integer(1))
  clustered <- all(vapply(object@pages, S7::S7_inherits, logical(1),
                          class = PdfClusters))
  print(object)
  cat(sprintf("  words per page: min %d, median %s, max %d\n",
              min(words_per_page), stats::median(words_per_page),
              max(words_per_page)))
  overview <- tibble::tibble(
    page  = vapply(object@pages, function(p) p@number, integer(1)),
    words = words_per_page
  )
  if (clustered) {
    overview$clusters <- vapply(object@pages, n_clusters, integer(1))
    cat(sprintf("  clusters per page: min %d, median %s, max %d\n",
                min(overview$clusters), stats::median(overview$clusters),
                max(overview$clusters)))
  }
  invisible(overview)
}

S7::method(summary, PdfPage) <- function(object, ...) {
  print(object)
  if (!all(c("font_name", "font_size") %in% names(object@words))) {
    return(invisible(NULL))
  }
  fonts <- object@words |>
    dplyr::count(font_name, font_size, name = "words") |>
    dplyr::arrange(dplyr::desc(words))
  invisible(fonts)
}

S7::method(summary, PdfClusters) <- function(object, ...) {
  print(object)
  if (is.null(object@words$.cluster)) return(invisible(NULL))
  clusters <- object@words |>
    dplyr::filter(.cluster != 0) |>
    dplyr::group_by(.cluster) |>
    dplyr::summarise(
      words = dplyr::n(),
      text  = paste0(substr(paste(text, collapse = " "), 1, 40), "..."),
      .groups = "drop"
    )
  invisible(clusters)
}
