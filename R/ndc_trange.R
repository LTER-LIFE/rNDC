#' Date(s) in RFC 3339 format
#'
#' Convert one or more dates to the RFC 3339 format.
#'
#' @param x either a single date or a vector of two dates.
#' @returns character. Either one date or a range of start and end dates in RFC 3339 format. (Note: the interval can be open on one of the two sides.)
#' @export

as_rfc3339 <- function(x) {
  format(as_datetime(x), "%Y-%m-%dT%H:%M:%SZ")
}

ndc_trange <- function(x) {
  if(all(is.na(x)) || length(x) < 1 || length(x) > 2) {
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
