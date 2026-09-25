#' Assign words to visual lines
#'
#' Groups words into lines: words whose vertical centers are within half
#' the modal word height of each other belong to the same line. Returns
#' the words with a `.line` column added.
#'
#' @param words data frame with at least `x`, `y` and `height` columns
#' @noRd
assign_lines <- function(words) {
  if (nrow(words) == 0) {
    words$.line <- integer(0)
    return(words)
  }
  heights <- table(words$height)
  modal_height <- as.numeric(names(heights)[which.max(heights)])
  tolerance <- modal_height / 2

  y_center <- words$y + words$height / 2
  ord <- order(y_center, words$x)
  new_line <- c(TRUE, diff(y_center[ord]) > tolerance)
  line_id <- integer(nrow(words))
  line_id[ord] <- cumsum(new_line)
  words$.line <- line_id
  words
}

#' Build the text of one cluster in visual reading order
#'
#' Words are ordered into lines (top to bottom) and within each line from
#' left to right. Lines are separated by a newline.
#'
#' @param words the words of one cluster
#' @param dehyphenate logical; merge words that are hyphenated across lines
#' @noRd
cluster_text <- function(words, dehyphenate = FALSE) {
  words <- assign_lines(words)
  words <- words[order(words$.line, words$x), ]
  lines <- vapply(split(words$text, words$.line), paste, character(1),
                  collapse = " ")
  text <- paste(lines, collapse = "\n")
  if (dehyphenate) {
    # word ends in a hyphen at a line break, next word starts lowercase
    text <- gsub("-\n(\\p{Ll})", "\\1", text, perl = TRUE)
  }
  text
}

#' Find the largest uncovered gap in a set of intervals
#'
#' Projects the intervals `[lo, hi]` onto their axis and looks for gaps in
#' the coverage of at least `min_gap` wide.
#'
#' @return `NULL` when there is no such gap, otherwise a list with the gap
#'   midpoint (`at`) and its width (`size`).
#' @noRd
find_largest_gap <- function(lo, hi, min_gap) {
  ord <- order(lo)
  lo <- lo[ord]
  hi <- hi[ord]
  cover_end <- cummax(hi)
  gap_start <- cover_end[-length(cover_end)]
  gap_end <- lo[-1]
  size <- gap_end - gap_start
  ok <- size >= min_gap
  if (!any(ok)) {
    return(NULL)
  }
  i <- which(ok)[which.max(size[ok])]
  list(at = (gap_start[i] + gap_end[i]) / 2, size = size[i])
}

#' Recursive XY-cut over cluster bounding boxes
#'
#' Recursively splits the boxes along the largest whitespace band that does
#' not intersect any box. A horizontal cut (top/bottom) takes precedence
#' over a vertical cut (left/right) unless the vertical gap is at least
#' twice as wide (reversed when `prefer = "columns"`). Within a segment
#' that can no longer be split, boxes are ordered top to bottom, then left
#' to right.
#'
#' @param boxes data frame with `.cluster`, `x_min`, `x_max`, `y_min`, `y_max`
#' @return the cluster ids in reading order
#' @noRd
xy_cut <- function(boxes, min_gap, prefer = "rows") {
  if (nrow(boxes) <= 1) {
    return(boxes$.cluster)
  }

  h_gap <- find_largest_gap(boxes$y_min, boxes$y_max, min_gap)
  v_gap <- find_largest_gap(boxes$x_min, boxes$x_max, min_gap)

  if (is.null(h_gap) && is.null(v_gap)) {
    return(boxes$.cluster[order(boxes$y_min, boxes$x_min)])
  }

  use_horizontal <- if (prefer == "rows") {
    !is.null(h_gap) && (is.null(v_gap) || v_gap$size < 2 * h_gap$size)
  } else {
    !is.null(h_gap) && is.null(v_gap)
  }

  if (use_horizontal) {
    first <- boxes$y_min < h_gap$at
  } else {
    first <- boxes$x_min < v_gap$at
  }
  c(xy_cut(boxes[first, , drop = FALSE], min_gap, prefer),
    xy_cut(boxes[!first, , drop = FALSE], min_gap, prefer))
}

#' Renumber the clusters of one page in reading order via XY-cut
#'
#' @param words clustered words of one page (with `.cluster`)
#' @param min_gap_factor minimal whitespace band to split on, as a
#'   multiple of the modal word height
#' @param prefer `"rows"` or `"columns"`: which cut direction wins when
#'   both are possible
#' @noRd
order_clusters_page <- function(words, min_gap_factor = 1, prefer = "rows",
                                exclude = NULL) {
  boxes_all <- words |>
    dplyr::filter(.cluster != 0) |>
    dplyr::group_by(.cluster) |>
    dplyr::summarise(
      x_min = min(x), x_max = max(x + width),
      y_min = min(y), y_max = max(y + height),
      .groups = "drop"
    )
  exclude <- as.character(exclude)
  boxes <- boxes_all[!as.character(boxes_all$.cluster) %in% exclude, , drop = FALSE]

  ordered_ids <- if (nrow(boxes) <= 1) {
    boxes$.cluster
  } else {
    heights <- table(words$height)
    modal_height <- as.numeric(names(heights)[which.max(heights)])
    xy_cut(boxes, min_gap = modal_height * min_gap_factor, prefer = prefer)
  }

  # excluded clusters (e.g. headers/footers) come after the reading flow,
  # top to bottom
  excluded_boxes <- boxes_all[as.character(boxes_all$.cluster) %in% exclude, ,
                              drop = FALSE]
  ordered_ids <- c(ordered_ids,
                   excluded_boxes$.cluster[order(excluded_boxes$y_min,
                                                 excluded_boxes$x_min)])

  mapping <- stats::setNames(seq_along(ordered_ids), as.character(ordered_ids))
  old <- as.character(words$.cluster)
  new <- ifelse(old == "0", 0L, mapping[old])
  words$.cluster <- factor(new, levels = 0:length(mapping))
  words
}
