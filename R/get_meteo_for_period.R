#' Get meteo data for a period
#'
#' Fetch _AgroDataCube_ meteo data for a station between two dates. Only one page
#' of results (`page_size` rows) is retrieved; for long periods use
#' [get_meteo_for_long_period()].
#'
#' @param meteostation character or numeric. Station identifier.
#' @param fromdate,todate Date or character. A `Date`, or a `"YYYYMMDD"` / `"YYYY-MM-DD"` string.
#' @param token character. _AgroDataCube_ API token.
#' @param page_size,page_offset integer. Paging of the results (`page_offset` is the page number, starting at 0).
#' @param output_epsg character. EPSG code of the returned geometries.
#' @returns An sf object, or `NULL` (with a warning) if no data is returned.
#' @export
get_meteo_for_period <- function(meteostation, fromdate, todate, token,
                                 page_size = 500, page_offset = 0, output_epsg = "4326") {
  # normalize dates
  fmt_date <- function(d) {
    if (inherits(d, "Date")) return(format(d, "%Y%m%d"))
    ds <- gsub("-", "", as.character(d))
    if (!grepl("^\\d{8}$", ds)) stop("Dates must be Date or 'YYYYMMDD'/'YYYY-MM-DD' strings")
    ds
  }
  from_str <- fmt_date(fromdate)
  to_str   <- fmt_date(todate)
  
  params <- c(output_epsg = output_epsg,
              meteostation = as.character(meteostation),
              fromdate = from_str,
              todate = to_str,
              page_size = format(page_size, scientific = FALSE, trim = TRUE),
              page_offset = format(page_offset, scientific = FALSE, trim = TRUE))
  myurl <- adc_url(option = "Meteo_data", params = params)
  
  myres <- adc_get(url = myurl, token = token)
  
  if (is.null(myres) || length(myres$features) == 0) {
    warning("No meteo data returned for station ", meteostation, " between ", from_str, " and ", to_str)
    return(NULL)
  }
  
  out_sf <- tryCatch(suppressWarnings(geojson_sf(toJSON(myres, auto_unbox = TRUE))), # observations have no geometry
                     error = function(e) stop("Failed to convert meteo period result: ", e$message))
  out_sf
}
