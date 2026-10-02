#' Date(s) in RFC 3339 format
#'
#' Convert dates to the RFC 3339 format (UTC).
#'
#' @param x Date, POSIXct or character. One or more dates.
#' @returns character. The dates in RFC 3339 format.
#' @export

as_rfc3339 <- function(x) {
  format(as_datetime(x), "%Y-%m-%dT%H:%M:%SZ")
}

#' Temporal range in RFC 3339 format
#'
#' Convert one or two dates to a STAC `datetime` value.
#'
#' @param x either a single date or a vector of two dates. `NA` on one side leaves the interval open on that side. A string that is already an RFC 3339 date or interval (such as the output of this function) is returned unchanged.
#' @returns character. Either one date or a range of start and end dates in RFC 3339 format, or `NA` if `x` is empty, all `NA`, or longer than two.
#' @export

ndc_trange <- function(x) {
  rfc3339 <- "(\\.\\.|[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z)"
  if (is.character(x) && length(x) == 1 && grepl(paste0("^", rfc3339, "(/", rfc3339, ")?$"), x)) {
    x  # already a STAC `datetime` value
  } else if (all(is.na(x)) || length(x) < 1 || length(x) > 2) {
    NA
  } else if (length(x) == 1) {
    as_rfc3339(x)
  } else if (length(x) == 2 && is.na(x[1])) {
    paste(c("..", as_rfc3339(x[2])), collapse = "/")
  } else if (length(x) == 2 && is.na(x[2])) {
    paste(c(as_rfc3339(x[1]), ".."), collapse = "/")
  } else {
    paste(lapply(sort(x), FUN = as_rfc3339), collapse = "/")
  }
}
