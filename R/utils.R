#' Create a standard progress bar in the caller's frame
#'
#' @param action label shown in front of the progress bar
#' @param total total number of steps
#' @noRd
pdf_progress_bar <- function(action, total) {
  cli::cli_progress_bar(
    format = paste0(
      action, ": ",
      "{cli::pb_spin} [{cli::pb_current}/{cli::pb_total}] ",
      "[{cli::pb_bar}] {cli::pb_percent}% ",
      "ETA: {cli::pb_eta}"
    ),
    total = total,
    clear = FALSE,
    .envir = parent.frame()
  )
}

#' Warn once when the deprecated tolerance_factor argument is supplied
#' @noRd
warn_tolerance_factor <- function(tolerance_factor) {
  if (lifecycle::is_present(tolerance_factor)) {
    lifecycle::deprecate_warn(
      "0.1.0", "pdf_detect_clusters(tolerance_factor)",
      details = "Cluster ordering now uses a recursive XY-cut; see `min_gap_factor` and `prefer`."
    )
  }
}
