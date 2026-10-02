#' Split a date range
#'
#' Split a date range into consecutive chunks.
#'
#' @param fromdate,todate Date or character. Start and end of the range.
#' @param by_days integer. Length (in days) of each chunk.
#' @returns A data frame with the `from` and `to` dates of each chunk.
#' @export
split_date_range <- function(fromdate, todate, by_days = 7) {
  fromdate <- as.Date(fromdate)
  todate   <- as.Date(todate)
  
  starts <- seq(fromdate, todate, by = paste(by_days, "days"))
  ends   <- pmin(starts + by_days - 1, todate)
  
  data.frame(
    from = starts,
    to   = ends
  )
}
