#' Get meteo data for a long period
#'
#' Fetch _AgroDataCube_ meteo data for a station over a long period, by splitting
#' it into chunks of `by_days` days (see [split_date_range()]) and combining the results.
#'
#' @inheritParams get_meteo_for_period
#' @param page_size integer. Maximum number of rows per request.
#' @param by_days integer. Length (in days) of each request.
#' @param sleep_sec numeric. Seconds to wait between requests.
#' @returns An sf object, or `NULL` (with a warning) if no data is returned.
#' @seealso [ndc_with_progress()] to follow the requests, or to stop them.
#' @export
get_meteo_for_long_period <- function(meteostation,
                                      fromdate,
                                      todate,
                                      token,
                                      by_days = 7,
                                      page_size = 500,
                                      output_epsg = "4326",
                                      sleep_sec = 0) {
  
  ranges <- split_date_range(fromdate, todate, by_days)
  results <- list()
  
  for (i in seq_len(nrow(ranges))) {
    
    msg <- sprintf(
      "Downloading %s -> %s (%d/%d)",
      ranges$from[i],
      ranges$to[i],
      i,
      nrow(ranges)
    )
    message(msg)
    ndc_progress(msg, i, nrow(ranges))
    
    res <- get_meteo_for_period(
      meteostation = meteostation,
      fromdate     = ranges$from[i],
      todate       = ranges$to[i],
      token        = token,
      page_size    = page_size,
      output_epsg  = output_epsg
    )
    
    if (!is.null(res)) {
      results[[length(results) + 1]] <- res
    }
    
    if (sleep_sec > 0) {
      Sys.sleep(sleep_sec)
    }
  }
  
  if (length(results) == 0) {
    warning("No meteo data returned for the full requested period")
    return(NULL)
  }
  
  # Combine all sf objects
  suppressWarnings(do.call(rbind, results)) # observations have no geometry (empty bounding box)
}
