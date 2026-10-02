#' Get meteo data for one date
#'
#' Fetch _AgroDataCube_ meteo data for a single station and date.
#'
#' @param stationid character or numeric. Station identifier.
#' @param date Date or character. A `Date`, or a `"YYYYMMDD"` / `"YYYY-MM-DD"` string.
#' @param token character. _AgroDataCube_ API token.
#' @param page_size,page_offset integer. Paging of the results (`page_offset` is the page number, starting at 0).
#' @param output_epsg character. EPSG code of the returned geometries.
#' @returns An sf object, or `NULL` (with a warning) if no data is returned.
#' @export
get_meteo_for_date <- function(stationid, date, token, page_size = 500, page_offset = 0, output_epsg = "4326") {
  if (inherits(date, "Date")) {
    date_str <- format(date, "%Y%m%d")
  } else {
    # allow "YYYY-MM-DD" or "YYYYMMDD"
    date_try <- gsub("-", "", as.character(date))
    if (!grepl("^\\d{8}$", date_try)) stop("date must be Date or 'YYYYMMDD' / 'YYYY-MM-DD' string")
    date_str <- date_try
  }
  
  params <- c(output_epsg = output_epsg, stationid = as.character(stationid), date = date_str,
              page_size = format(page_size, scientific = FALSE, trim = TRUE), page_offset = format(page_offset, scientific = FALSE, trim = TRUE))
  myres <- adc_get(url = adc_url(option = "Meteo_data", params = params), token = token)

  if (length(myres$features) == 0) {
    warning("No meteo data returned for station ", stationid, " on date ", date_str)
    return(NULL)
  }

  out_sf <- tryCatch(suppressWarnings(geojson_sf(toJSON(myres, auto_unbox = TRUE))), # observations have no geometry
                     error = function(e) stop("Failed to convert meteo result: ", e$message))
  out_sf
}
