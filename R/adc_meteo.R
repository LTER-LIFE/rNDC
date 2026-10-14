# Weather data from the AgroDataCube (KNMI stations): the closest station, the observations of a day, a period or a long
# period (split in requests), and the split of a date range.

#' Find the closest meteorological station
#'
#' Find the _AgroDataCube_ meteorological station closest to the centroid of a study area.
#'
#' @param polygon_wkt The study area, whose centroid is used: either a WKT string (EPSG:4326), or anything that [ndc_roi()] accepts (an sf/sfc object in any CRS, a bounding box, or a path to a file). Several geometries are combined into one.
#' @param token character. _AgroDataCube_ API token (by default the environment variable `ADC_TOKEN`).
#' @param page_size,page_offset integer. Paging of the stations list. The default `page_size` is the maximum allowed by the API, which is more than the number of stations.
#' @param output_epsg character. EPSG code of the returned geometries.
#' @inheritParams ndc_get
#' @returns A list with `stations_sf` (all stations), `closest_station` (sf row of the closest one), `closest_id` (its identifier) and `distances` (distance in metres, in EPSG:28992, from the centroid to each station).
#' @export
get_closest_meteostation <- function(polygon_wkt,
                                     token = Sys.getenv("ADC_TOKEN"),
                                     page_size = 10000,
                                     page_offset = 0,
                                     output_epsg = "4326",
                                     params_file = NULL,
                                     save_params = NULL) {
  if (!is.null(params_file)) params_apply("get_closest_meteostation", params_file)
  if (!is.null(save_params)) params_save_when_done("get_closest_meteostation", save_params)
  stopifnot(!is.null(polygon_wkt), !(is.character(polygon_wkt) && !all(nzchar(polygon_wkt))))
  
  # Get the stations (a single request: the API limits `page_size` to 10000, far more than the number of stations)
  myurl <- adc_url(option = "Meteo_stations",
                   params = c(output_epsg = output_epsg,
                              page_size = format(page_size, scientific = FALSE, trim = TRUE),
                              page_offset = format(page_offset, scientific = FALSE, trim = TRUE)))
  myres <- tryCatch(adc_get(url = myurl, token = token),
                    error = function(e) stop("Failed to get meteostations: ", e$message))

  if (length(myres$features) == 0) {
    stop("No meteostations returned from API.")
  }
  if (length(myres$features) >= page_size) {
    warning("The number of stations equals `page_size`: the list may be incomplete.", call. = FALSE)
  }

  stations_sf <- tryCatch(geojson_sf(toJSON(myres, auto_unbox = TRUE)),
                          error = function(e) stop("Failed to parse stations geojson: ", e$message))
  
  # Compute centroid of input polygon_wkt
  if (is.character(polygon_wkt) && length(polygon_wkt) == 1 && !file.exists(polygon_wkt)) {
    poly_sfc <- tryCatch(st_as_sfc(polygon_wkt, crs = 4326),
                         error = function(e) stop("Invalid WKT polygon: ", e$message))
  } else {
    poly_sfc <- ndc_roi(polygon_wkt)
  }
  poly_centroid <- st_centroid(poly_sfc)
  
  # Distances
  suppressWarnings({ stations_m <- st_transform(stations_sf, 28992)
                     centroid_m <- st_transform(poly_centroid, 28992) })
  
  dists <- as.numeric(st_distance(centroid_m, stations_m)) # vector
  closest_idx <- which.min(dists)
  
  closest_station_sf <- stations_sf[closest_idx, , drop = FALSE]
  possible_names <- c("meteostationid", "meteostation_id", "stationid", "station_id", "id")
  closest_id <- NULL
  for (nm in possible_names) {
    if (nm %in% names(closest_station_sf)) {
      closest_id <- closest_station_sf[[nm]]
      break
    }
  }
  
  list(stations_sf = stations_sf,
       closest_station = closest_station_sf,
       closest_id = as.character(closest_id),
       distances = dists)
}

#' Get meteo data for one date
#'
#' Fetch _AgroDataCube_ meteo data for a single station and date.
#'
#' @param stationid character or numeric. Station identifier.
#' @param date Date or character. A `Date`, or a `"YYYYMMDD"` / `"YYYY-MM-DD"` string.
#' @param token character. _AgroDataCube_ API token (by default the environment variable `ADC_TOKEN`).
#' @param page_size,page_offset integer. Paging of the results (`page_offset` is the page number, starting at 0).
#' @param output_epsg character. EPSG code of the returned geometries.
#' @inheritParams ndc_get
#' @returns An sf object, or `NULL` (with a warning) if no data is returned.
#' @export
get_meteo_for_date <- function(stationid, date, token = Sys.getenv("ADC_TOKEN"), page_size = 500, page_offset = 0,
                              output_epsg = "4326", params_file = NULL, save_params = NULL) {
  if (!is.null(params_file)) params_apply("get_meteo_for_date", params_file)
  if (!is.null(save_params)) params_save_when_done("get_meteo_for_date", save_params)
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

#' Get meteo data for a period
#'
#' Fetch _AgroDataCube_ meteo data for a station between two dates. Only one page
#' of results (`page_size` rows) is retrieved; for long periods use
#' [get_meteo_for_long_period()].
#'
#' @param meteostation character or numeric. Station identifier.
#' @param fromdate,todate Date or character. A `Date`, or a `"YYYYMMDD"` / `"YYYY-MM-DD"` string.
#' @param token character. _AgroDataCube_ API token (by default the environment variable `ADC_TOKEN`).
#' @param page_size,page_offset integer. Paging of the results (`page_offset` is the page number, starting at 0).
#' @param output_epsg character. EPSG code of the returned geometries.
#' @inheritParams ndc_get
#' @returns An sf object, or `NULL` (with a warning) if no data is returned.
#' @export
get_meteo_for_period <- function(meteostation, fromdate, todate, token = Sys.getenv("ADC_TOKEN"),
                                 page_size = 500, page_offset = 0, output_epsg = "4326",
                                 params_file = NULL, save_params = NULL) {
  if (!is.null(params_file)) params_apply("get_meteo_for_period", params_file)
  if (!is.null(save_params)) params_save_when_done("get_meteo_for_period", save_params)
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
                                      token = Sys.getenv("ADC_TOKEN"),
                                      by_days = 7,
                                      page_size = 500,
                                      output_epsg = "4326",
                                      sleep_sec = 0,
                                      params_file = NULL,
                                      save_params = NULL) {
  if (!is.null(params_file)) params_apply("get_meteo_for_long_period", params_file)
  if (!is.null(save_params)) params_save_when_done("get_meteo_for_long_period", save_params)
  
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
    # without a progress function (see ndc_with_progress()) say where the download is on the console
    if (!is.function(getOption("rNDC.progress"))) message(msg)
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
