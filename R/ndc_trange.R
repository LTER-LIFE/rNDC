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

# The two ends of a period as POSIXct (`NA` for an open side), from anything that ndc_trange() accepts
trange_bounds <- function(trange) {
  if (is.null(trange)) return(as.POSIXct(c(NA, NA), tz = "UTC"))
  text <- ndc_trange(trange)
  if (is.na(text)) return(as.POSIXct(c(NA, NA), tz = "UTC"))
  parts <- strsplit(text, "/", fixed = TRUE)[[1]]
  if (length(parts) == 1) parts <- c(parts, parts)
  parts[parts == ".."] <- NA
  as.POSIXct(lubridate::as_datetime(parts), tz = "UTC")
}

# The two ends of a period as Dates, both required (the weather and monthly NDVI functions need a first and a last day).
# A text can be "YYYYMMDD" as well as "YYYY-MM-DD".
trange_dates <- function(trange, arg = "trange") {
  if (is.character(trange) && length(trange) == 2) {
    trange <- ifelse(grepl("^[0-9]{8}$", trange), format(as.Date(trange, "%Y%m%d")), trange)
  }
  bounds <- tryCatch(trange_bounds(trange), error = function(e) as.POSIXct(c(NA, NA), tz = "UTC"))
  if (length(bounds) != 2 || anyNA(bounds)) {
    stop("`", arg, "` must be a first and a last date (Date, or text like \"2024-01-31\").", call. = FALSE)
  }
  as.Date(bounds)
}
