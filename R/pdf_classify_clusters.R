#' Rules for Text Type Classification
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' The thresholds used by [pdf_classify_clusters()]. Pass a modified copy
#' to tune the classification without changing code.
#'
#' @param top_margin,bottom_margin fraction of the page height that counts
#'   as the top/bottom band in which page headers, footers and page
#'   numbers live.
#' @param repeat_min_share minimal fraction of pages on which a
#'   (digit-masked) text must repeat in the same band to count as a page
#'   header/footer.
#' @param repeat_min_pages minimal absolute number of pages for the same.
#' @param heading_min_size_ratio minimal font size relative to the body
#'   text for a cluster to be a heading.
#' @param heading_bold_size_ratio as `heading_min_size_ratio`, but for
#'   bold clusters.
#' @param heading_max_words,heading_max_lines maximal size of a heading.
#' @param caption_pattern regular expression (case-insensitive) that
#'   identifies captions.
#' @param figure_max_words maximal number of words for a cluster to be
#'   considered chart/figure text.
#' @param figure_numeric_share minimal fraction of numeric words for
#'   chart/figure text.
#' @param figure_max_size_ratio maximal font size relative to the body
#'   text for chart/figure text.
#' @param bold_pattern regular expression that identifies bold fonts by
#'   their font name.
#'
#' @return A named list of rules.
#' @export
#'
#' @examples
#' # Wider top band for documents with tall headers
#' rules <- pdf_type_rules(top_margin = 0.2)
pdf_type_rules <- function(top_margin = 0.15,
                           bottom_margin = 0.12,
                           repeat_min_share = 0.3,
                           repeat_min_pages = 3,
                           heading_min_size_ratio = 1.15,
                           heading_bold_size_ratio = 1.0,
                           heading_max_words = 20,
                           heading_max_lines = 3,
                           caption_pattern = "^(figuur|tabel|figure|table|afbeelding|grafiek)\\b",
                           figure_max_words = 4,
                           figure_numeric_share = 0.5,
                           figure_max_size_ratio = 0.85,
                           bold_pattern = "bold|black|heavy|semibold|[-_][789]00") {
  as.list(environment())
}

pdf_types <- c("body", "heading", "caption", "figure_text",
               "page_header", "page_footer", "page_number")

#' Features of the clusters on one page
#'
#' @param page a PdfClusters object
#' @param rules result of pdf_type_rules()
#' @noRd
cluster_features_page <- function(page, rules) {
  words <- dplyr::filter(page@words, .cluster != 0)
  if (nrow(words) == 0) {
    return(NULL)
  }
  has_font <- all(c("font_name", "font_size") %in% names(words))
  page_height <- if (!is.na(page@height)) page@height else
    max(words$y + words$height)

  words |>
    dplyr::group_by(.cluster) |>
    dplyr::group_modify(function(w, key) {
      size <- if (has_font) modal_value(w$font_size) else modal_value(w$height)
      lines <- assign_lines(w)
      full_text <- cluster_text(w)
      tibble::tibble(
        n_words = nrow(w),
        n_lines = max(lines$.line),
        y_min = min(w$y),
        y_max = max(w$y + w$height),
        size = size,
        bold_share = if (has_font) {
          mean(grepl(rules$bold_pattern, w$font_name, ignore.case = TRUE))
        } else 0,
        numeric_share = mean(grepl("^[0-9.,/%-]+$", w$text)),
        text = full_text
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      page = page@number,
      in_top = y_max <= rules$top_margin * page_height,
      in_bottom = y_min >= (1 - rules$bottom_margin) * page_height,
      masked_text = gsub("[0-9]+", "#", text)
    )
}

#' The most common value of a vector
#' @noRd
modal_value <- function(x) {
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

#' Classify the clusters of a document into text types
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Assigns a text type to each detected cluster: running text (`body`),
#' headings (`heading`, with a level), captions, chart/figure label
#' fragments (`figure_text`), and page furniture (`page_header`,
#' `page_footer`, `page_number`). The types are added to the words as a
#' `.type` column (plus `.type_level` for headings), and the clusters are
#' reordered so that page furniture comes after the reading flow.
#'
#' Page headers and footers are recognized by repetition: (digit-masked)
#' text that recurs in the top or bottom band on a substantial share of
#' pages. Page numbers are recognized as short numeric clusters whose
#' value increases in step with the page number. These document-level
#' signals need multiple pages; on a single page only the position and
#' font based rules apply.
#'
#' @param x a [PdfDocument] whose pages have been clustered with
#'   [pdf_detect_clusters()], or a single [PdfClusters] page.
#' @param rules the classification thresholds; see [pdf_type_rules()].
#' @param verbose logical; if `FALSE`, informational messages are
#'   suppressed. Defaults to the package option `pdftextclusteR.verbose`,
#'   or `TRUE` when that option is not set.
#' @param ... not used.
#'
#' @return The input with a `.type` factor column (and `.type_level` for
#'   headings) added to the words of every page.
#' @export
#'
#' @examples
#' classified <- npo[1:5] |>
#'   pdf_detect_clusters() |>
#'   pdf_classify_clusters()
#'
#' # Extract only the running text
#' classified |>
#'   pdf_extract_clusters(exclude = c("page_header", "page_footer", "page_number"))
pdf_classify_clusters <- S7::new_generic(
  "pdf_classify_clusters", "x",
  function(x, rules = pdf_type_rules(),
           verbose = getOption("pdftextclusteR.verbose", TRUE), ...) {
    S7::S7_dispatch()
  }
)

S7::method(pdf_classify_clusters, PdfDocument) <- function(
    x, rules = pdf_type_rules(),
    verbose = getOption("pdftextclusteR.verbose", TRUE), ...) {

  if (!all(vapply(x@pages, S7::S7_inherits, logical(1), class = PdfClusters))) {
    cli::cli_abort("No clusters detected yet. Run {.fn pdf_detect_clusters} first.")
  }

  features <- dplyr::bind_rows(lapply(x@pages, cluster_features_page, rules = rules))
  if (nrow(features) == 0) {
    return(x)
  }

  # Body font size: the size that covers the most words in the document
  size_counts <- stats::aggregate(n_words ~ size, data = features, sum)
  body_size <- size_counts$size[which.max(size_counts$n_words)]
  features$size_ratio <- features$size / body_size

  n_pages <- length(x@pages)
  features$type <- classify_types(features, rules, n_pages)
  features <- add_heading_levels(features)

  pages <- lapply(x@pages, function(page) {
    apply_types_page(page, features[features$page == page@number, , drop = FALSE])
  })

  if (verbose) {
    counts <- table(features$type)
    counts <- counts[counts > 0]
    cli::cli_alert_success(
      "Classified {nrow(features)} cluster{?s} on {n_pages} page{?s}: {paste(names(counts), counts, sep = ' ', collapse = ', ')}."
    )
  }

  PdfDocument(pages = pages, source = x@source)
}

S7::method(pdf_classify_clusters, PdfClusters) <- function(
    x, rules = pdf_type_rules(),
    verbose = getOption("pdftextclusteR.verbose", TRUE), ...) {
  if (verbose) {
    cli::cli_alert_warning(
      "Classifying a single page: repetition-based signals (page headers/footers, page numbers) need multiple pages and are skipped."
    )
  }
  doc <- pdf_classify_clusters(
    PdfDocument(pages = list(x)), rules = rules, verbose = FALSE)
  doc@pages[[1]]
}

#' Assign a type to every cluster (document level)
#' @noRd
classify_types <- function(features, rules, n_pages) {
  type <- rep("body", nrow(features))

  # Page headers/footers: digit-masked text repeating in the same band.
  # Purely numeric clusters are exempt: they are page-number candidates
  # (their masks would be identical on every page by construction).
  has_letters <- grepl("[[:alpha:]]", features$masked_text)
  for (band in c("in_top", "in_bottom")) {
    in_band <- features[[band]] & has_letters
    if (!any(in_band)) next
    repeats <- stats::aggregate(
      page ~ masked_text, data = features[in_band, , drop = FALSE],
      FUN = function(p) length(unique(p)))
    recurring <- repeats$masked_text[
      repeats$page >= max(rules$repeat_min_pages, rules$repeat_min_share * n_pages)]
    hit <- in_band & features$masked_text %in% recurring
    type[hit] <- if (band == "in_top") "page_header" else "page_footer"
  }

  # Page numbers: short numeric clusters whose value tracks the page number
  candidate <- type == "body" & (features$in_top | features$in_bottom) &
    features$n_words <= 2 & grepl("^[0-9]{1,4}$", trimws(features$text))
  if (sum(candidate) >= rules$repeat_min_pages) {
    offsets <- as.integer(features$text[candidate]) - features$page[candidate]
    common <- as.integer(names(which.max(table(offsets))))
    matching <- candidate
    matching[candidate] <- offsets == common
    if (sum(matching) >= rules$repeat_min_pages) {
      type[matching] <- "page_number"
    }
  }

  # Captions
  caption <- type == "body" &
    grepl(rules$caption_pattern, features$text, ignore.case = TRUE)
  type[caption] <- "caption"

  # Headings: larger or bold, and short
  heading <- type == "body" &
    (features$size_ratio >= rules$heading_min_size_ratio |
       (features$bold_share >= 0.6 &
          features$size_ratio >= rules$heading_bold_size_ratio)) &
    features$n_words <= rules$heading_max_words &
    features$n_lines <= rules$heading_max_lines
  type[heading] <- "heading"

  # Chart/figure label fragments
  figure <- type == "body" &
    features$n_words <= rules$figure_max_words &
    (features$numeric_share >= rules$figure_numeric_share |
       features$size_ratio <= rules$figure_max_size_ratio)
  type[figure] <- "figure_text"

  type
}

#' Heading levels from distinct heading sizes (document level)
#' @noRd
add_heading_levels <- function(features) {
  features$type_level <- NA_integer_
  is_heading <- features$type == "heading"
  if (any(is_heading)) {
    sizes <- sort(unique(round(features$size[is_heading], 1)), decreasing = TRUE)
    features$type_level[is_heading] <-
      match(round(features$size[is_heading], 1), sizes)
  }
  features
}

#' Write the types back to a page and reorder its clusters
#' @noRd
apply_types_page <- function(page, page_features) {
  words <- page@words
  if (nrow(words) == 0 || nrow(page_features) == 0) {
    return(page)
  }
  mapping <- stats::setNames(page_features$type,
                             as.character(page_features$.cluster))
  level_mapping <- stats::setNames(page_features$type_level,
                                   as.character(page_features$.cluster))
  cl <- as.character(words$.cluster)
  words$.type <- factor(unname(mapping[cl]), levels = pdf_types)
  words$.type_level <- unname(level_mapping[cl])

  furniture <- page_features$.cluster[
    page_features$type %in% c("page_header", "page_footer", "page_number")]
  words <- order_clusters_page(words, exclude = furniture)

  PdfClusters(words = words, number = page@number,
              width = page@width, height = page@height,
              algorithm = page@algorithm, params = page@params)
}
