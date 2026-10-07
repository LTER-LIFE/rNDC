#' Follow, and interrupt, long downloads
#'
#' Some functions make many requests: [get_meteo_for_long_period()] one per chunk of days,
#' [download_avg_ndvi_month()] and [download_avg_ndvi_stack()] one download per day. They report where they are
#' before each request, and give the caller the chance to stop, through two options that `ndc_with_progress()` sets
#' for the duration of an expression:
#'
#' * `rNDC.progress`: a `function(message, current, total)`, called before each request with a text
#'   (e.g. `"Downloading 2024-01-01 -> 2024-01-07 (1/10)"`), the number of the request that starts (integer) and
#'   the total number of requests (integer). Use it to show a progress bar. Errors in the function are ignored: a
#'   progress display never stops a download.
#' * `rNDC.interrupt`: a `function()` that returns `TRUE` when the caller wants the download to stop. It is asked
#'   at the same moments; the download then stops with an error of class `rNDC_interrupted`, before the next
#'   request is made (what was downloaded already is removed). Use it to cancel from a place that can set a flag,
#'   e.g. a Shiny app or a script with a time limit.
#'
#' Without these options nothing changes.
#'
#' @param expr expression to evaluate.
#' @param report `NULL`, or a `function(message, current, total)` (see above).
#' @param interrupt `NULL`, or a `function()` that returns `TRUE` to stop (see above).
#' @returns The value of `expr`.
#' @examples
#' \dontrun{
#' ndc_with_progress(
#'   get_meteo_for_long_period(310, "2020-01-01", "2020-12-31", token = Sys.getenv("ADC_TOKEN")),
#'   report = function(message, current, total) cat(sprintf("%d of %d\n", current, total))
#' )
#'
#' # stop after 30 seconds
#' start <- Sys.time()
#' res <- tryCatch(
#'   ndc_with_progress(
#'     download_avg_ndvi_stack(poly, 2023, 1, 2023, 12),
#'     interrupt = function() difftime(Sys.time(), start, units = "secs") > 30
#'   ),
#'   rNDC_interrupted = function(e) NULL
#' )
#' }
#' @export
ndc_with_progress <- function(expr, report = NULL, interrupt = NULL) {
  stopifnot(is.null(report) || is.function(report), is.null(interrupt) || is.function(interrupt))
  old <- options(rNDC.progress = report, rNDC.interrupt = interrupt)
  on.exit(options(old))
  expr
}

# Report that request `current` of `total` starts, and stop if the caller wants that (see ndc_with_progress()).
ndc_progress <- function(message, current = NA_integer_, total = NA_integer_) {
  report <- getOption("rNDC.progress")
  if (is.function(report)) {
    tryCatch(report(message, as.integer(current), as.integer(total)), error = function(e) NULL)
  }
  interrupt <- getOption("rNDC.interrupt")
  if (is.function(interrupt) && isTRUE(tryCatch(interrupt(), error = function(e) FALSE))) {
    stop(structure(class = c("rNDC_interrupted", "error", "condition"),
                   list(message = "The download was interrupted.", call = NULL)))
  }
  invisible(NULL)
}
