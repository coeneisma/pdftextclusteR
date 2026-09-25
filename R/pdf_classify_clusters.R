#' Rules for Text Type Classification
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' The thresholds used by [pdf_classify_clusters()]. Pass a modified copy
#' to tune the classification without changing code.
#'
#' @param margins `"auto"` (default) or `"fixed"`. With `"auto"`, page
#'   headers/footers are searched in wide bands (top and bottom 30% of
#'   the page) and must additionally sit at a *stable position* across
#'   pages (see `position_tolerance`) — this adapts to each document's
#'   actual margins. With `"fixed"`, the bands are exactly `top_margin`
#'   and `bottom_margin` and no positional stability is required.
#' @param top_margin,bottom_margin fraction of the page height that counts
#'   as the top/bottom band when `margins = "fixed"`.
#' @param position_tolerance maximal variation (as a fraction of the page
#'   height) in the vertical position of a repeated text across pages for
#'   it to count as a page header/footer when `margins = "auto"`.
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
pdf_type_rules <- function(margins = c("auto", "fixed"),
                           top_margin = 0.15,
                           bottom_margin = 0.12,
                           position_tolerance = 0.02,
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
  rules <- as.list(environment())
  rules$margins <- match.arg(margins)
  rules
}

pdf_types <- c("body", "heading", "caption", "figure_text",
               "page_header", "page_footer", "page_number")

#' Features of the clusters on one page
#'
#' @param page a PdfClusters object
#' @param rules result of pdf_type_rules()
#' @noRd
cluster_features_page <- function(page, rules) {
  all_words <- page@words
  words <- dplyr::filter(all_words, .cluster != 0)
  if (nrow(all_words) == 0) {
    return(NULL)
  }
  has_font <- all(c("font_name", "font_size") %in% names(all_words))
  page_height <- if (!is.na(page@height)) page@height else
    max(all_words$y + all_words$height)

  auto <- identical(rules$margins, "auto")
  top_m <- if (auto) 0.30 else rules$top_margin
  bottom_m <- if (auto) 0.30 else rules$bottom_margin

  cluster_part <- if (nrow(words) == 0) NULL else words |>
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
    dplyr::mutate(is_noise_word = FALSE, word_id = NA_integer_)

  # Individual noise words that look like page numbers: a standalone page
  # number is a single isolated word, which dbscan (minPts >= 2) can only
  # label as noise. They take part in the page-number progression check.
  noise_idx <- which(all_words$.cluster == 0 &
                       grepl("^[0-9]{1,4}$", all_words$text))
  noise_part <- if (length(noise_idx) == 0) NULL else {
    w <- all_words[noise_idx, ]
    tibble::tibble(
      .cluster = factor(0, levels = levels(all_words$.cluster)),
      n_words = 1L,
      n_lines = 1L,
      y_min = w$y,
      y_max = w$y + w$height,
      size = if (has_font) w$font_size else w$height,
      bold_share = 0,
      numeric_share = 1,
      text = w$text,
      is_noise_word = TRUE,
      word_id = noise_idx
    )
  }

  features <- dplyr::bind_rows(cluster_part, noise_part)
  if (is.null(features) || nrow(features) == 0) {
    return(NULL)
  }
  features |>
    dplyr::mutate(
      page = page@number,
      page_height = page_height,
      in_top = y_max <= top_m * page_height,
      in_bottom = y_min >= (1 - bottom_m) * page_height,
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
    counts <- table(features$type[!features$is_noise_word |
                                    (!is.na(features$type) &
                                       features$type == "page_number")])
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
  # With margins = "auto" the repeated text must also sit at a stable
  # vertical position across pages, so wide bands stay safe.
  auto <- identical(rules$margins, "auto")
  has_letters <- grepl("[[:alpha:]]", features$masked_text)
  min_pages <- max(rules$repeat_min_pages, rules$repeat_min_share * n_pages)
  for (band in c("in_top", "in_bottom")) {
    in_band <- features[[band]] & has_letters
    if (!any(in_band)) next
    sub <- features[in_band, , drop = FALSE]
    repeats <- stats::aggregate(
      cbind(pages = page, spread = y_min, height = page_height) ~ masked_text,
      data = data.frame(masked_text = sub$masked_text, page = sub$page,
                        y_min = sub$y_min, page_height = sub$page_height),
      FUN = identity, simplify = FALSE)
    n_unique <- vapply(repeats$pages, function(p) length(unique(p)), numeric(1))
    spread <- vapply(repeats$spread, function(y) diff(range(y)), numeric(1))
    mean_height <- vapply(repeats$height, mean, numeric(1))
    ok <- n_unique >= min_pages &
      (!auto | spread <= rules$position_tolerance * mean_height)
    recurring <- repeats$masked_text[ok]
    hit <- in_band & features$masked_text %in% recurring
    type[hit] <- if (band == "in_top") "page_header" else "page_footer"
  }

  # Page numbers: short numeric clusters (or isolated noise words) whose
  # value tracks the page number
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

  # Noise words are only ever page-number candidates
  type[features$is_noise_word & type != "page_number"] <- NA

  # Captions
  caption <- !is.na(type) & type == "body" &
    grepl(rules$caption_pattern, features$text, ignore.case = TRUE)
  type[caption] <- "caption"

  # Headings: larger or bold, and short
  heading <- !is.na(type) & type == "body" &
    (features$size_ratio >= rules$heading_min_size_ratio |
       (features$bold_share >= 0.6 &
          features$size_ratio >= rules$heading_bold_size_ratio)) &
    features$n_words <= rules$heading_max_words &
    features$n_lines <= rules$heading_max_lines
  type[heading] <- "heading"

  # Chart/figure label fragments
  figure <- !is.na(type) & type == "body" &
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
  is_heading <- !is.na(features$type) & features$type == "heading"
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
  cluster_features <- page_features[!page_features$is_noise_word, , drop = FALSE]
  mapping <- stats::setNames(cluster_features$type,
                             as.character(cluster_features$.cluster))
  level_mapping <- stats::setNames(cluster_features$type_level,
                                   as.character(cluster_features$.cluster))
  cl <- as.character(words$.cluster)
  words$.type <- factor(unname(mapping[cl]), levels = pdf_types)
  words$.type_level <- unname(level_mapping[cl])

  # Promote noise words recognized as page numbers to their own cluster
  promoted <- page_features[page_features$is_noise_word &
                              !is.na(page_features$type) &
                              page_features$type == "page_number", , drop = FALSE]
  cl_int <- as.integer(as.character(words$.cluster))
  next_id <- max(c(0L, cl_int), na.rm = TRUE)
  for (idx in promoted$word_id) {
    next_id <- next_id + 1L
    cl_int[idx] <- next_id
    words$.type[idx] <- "page_number"
  }
  words$.cluster <- factor(cl_int, levels = 0:next_id)

  furniture <- c(
    as.character(cluster_features$.cluster[
      !is.na(cluster_features$type) &
        cluster_features$type %in% c("page_header", "page_footer", "page_number")]),
    as.character(utils::tail(0:next_id, nrow(promoted)))
  )
  words <- order_clusters_page(words, exclude = furniture)

  PdfClusters(words = words, number = page@number,
              width = page@width, height = page@height,
              algorithm = page@algorithm, params = page@params)
}
