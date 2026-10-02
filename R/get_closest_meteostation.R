#' Find the closest meteorological station
#'
#' Find the _AgroDataCube_ meteorological station closest to the centroid of a study area.
#'
#' @param polygon_wkt character. WKT string (EPSG:4326) of the study area; its centroid is used.
#' @param token character. _AgroDataCube_ API token.
#' @param page_size,page_offset integer. Paging of the stations list. The default `page_size` is the maximum allowed by the API, which is more than the number of stations.
#' @param output_epsg character. EPSG code of the returned geometries.
#' @returns A list with `stations_sf` (all stations), `closest_station` (sf row of the closest one), `closest_id` (its identifier) and `distances` (distance in metres, in EPSG:28992, from the centroid to each station).
#' @export
get_closest_meteostation <- function(polygon_wkt,
                                     token,
                                     page_size = 10000,
                                     page_offset = 0,
                                     output_epsg = "4326") {
  stopifnot(is.character(polygon_wkt), nzchar(polygon_wkt))
  
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
  poly_sfc <- tryCatch(st_as_sfc(polygon_wkt, crs = 4326),
                       error = function(e) stop("Invalid WKT polygon: ", e$message))
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
