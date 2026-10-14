# The functions that were renamed in rNDC 0.5.2, kept for a while under their old names (and old arguments): they call the new
# ones and say so. The old and the new names:
#   get_landuse_raster -> ndc_landuse_raster          get_nitrogen_raster -> ndc_nitrogen_raster
#   get_ndvi_stats -> ndc_ndvi_stats                  download_avg_ndvi_month -> gm_ndvi_month
#   download_avg_ndvi_stack -> gm_ndvi_stack          get_closest_meteostation -> adc_weather_station
#   get_meteo_for_date -> adc_weather_date            get_meteo_for_period -> adc_weather_period
#   get_meteo_for_long_period -> adc_weather_long_period
# The arguments that were renamed: `aoi`, `poly` and `polygon_wkt` are `roi`; `from`/`to` and `fromdate`/`todate` are
# `trange`; `start_year`, `start_month`, `end_year` and `end_month` are a `trange` too; `meteostation` and `stationid` are
# `station`; `out_dir` is `output_dir`; the `collection` of the nitrogen raster is `collections`.

#' Deprecated functions
#'
#' These functions were renamed, and some of their arguments with them, so that the names follow the source of the data
#' (`ndc_`, `adc_`, `gm_`) and the same argument has the same name everywhere: the region is `roi`, a period is
#' `trange`, a weather station is `station`, a directory for the output is `output_dir`. The old names still work, with a
#' warning, and will be removed in a later version.
#'
#' | Deprecated | Use |
#' |---|---|
#' | `get_landuse_raster()` | [ndc_landuse_raster()] |
#' | `get_nitrogen_raster()` | [ndc_nitrogen_raster()] |
#' | `get_ndvi_stats()` | [ndc_ndvi_stats()] |
#' | `get_closest_meteostation()` | [adc_weather_station()] |
#' | `get_meteo_for_date()` | [adc_weather_date()] |
#' | `get_meteo_for_period()` | [adc_weather_period()] |
#' | `get_meteo_for_long_period()` | [adc_weather_long_period()] |
#' | `download_avg_ndvi_month()` | [gm_ndvi_month()] |
#' | `download_avg_ndvi_stack()` | [gm_ndvi_stack()] |
#'
#' @param aoi,poly,polygon_wkt Region of interest: now `roi`.
#' @param from,to,fromdate,todate First and last date: now the two dates of `trange`.
#' @param start_year,start_month,end_year,end_month First and last month: now the dates of `trange`.
#' @param stationid,meteostation Weather station: now `station`.
#' @param out_dir Directory for the output: now `output_dir`.
#' @param year,month,layers,token,endpoint,collection,overwrite,limit,file_prefix,subset_crs,min_file_size,ndc_id,epsg,date,page_size,page_offset,output_epsg,by_days,sleep_sec
#'   As in the new function.
#' @returns What the new function returns.
#' @name rNDC-deprecated
#' @keywords internal
NULL

# a date or a text, as an end of a period
deprecated_end <- function(x) if (inherits(x, "Date")) format(x) else x

#' @rdname rNDC-deprecated
#' @export
get_landuse_raster <- function(aoi, year = landuse_default_year, token = Sys.getenv("NDC_TOKEN"), endpoint = ndc_endpoint(),
                               collection = landuse_collection, out_dir = tempdir(), overwrite = TRUE, limit = 100,
                               file_prefix = NULL, subset_crs = landuse_subset_crs,
                               min_file_size = landuse_min_file_size) {
  .Deprecated("ndc_landuse_raster", package = "rNDC")
  ndc_landuse_raster(roi = aoi, year = year, token = token, endpoint = endpoint, collection = collection,
                     output_dir = out_dir, overwrite = overwrite, limit = limit, file_prefix = file_prefix,
                     subset_crs = subset_crs, min_file_size = min_file_size)
}

#' @rdname rNDC-deprecated
#' @export
get_nitrogen_raster <- function(aoi, year, layers = nitrogen_layer_choices, token = Sys.getenv("NDC_TOKEN"),
                                endpoint = ndc_endpoint(), collection = NULL, out_dir = tempdir(), overwrite = TRUE,
                                limit = 100, file_prefix = NULL) {
  .Deprecated("ndc_nitrogen_raster", package = "rNDC")
  ndc_nitrogen_raster(roi = aoi, year = year, layers = layers, token = token, endpoint = endpoint,
                      collections = collection, output_dir = out_dir, overwrite = overwrite, limit = limit,
                      file_prefix = file_prefix)
}

#' @rdname rNDC-deprecated
#' @export
get_ndvi_stats <- function(aoi, collection, from = NA, to = NA, ndc_id = NULL, token = Sys.getenv("NDC_TOKEN")) {
  .Deprecated("ndc_ndvi_stats", package = "rNDC")
  ndc_ndvi_stats(roi = aoi, collection = collection, trange = c(deprecated_end(from), deprecated_end(to)),
                 ndc_id = ndc_id, token = token)
}

#' @rdname rNDC-deprecated
#' @export
get_closest_meteostation <- function(polygon_wkt, token = Sys.getenv("ADC_TOKEN"), page_size = 10000, page_offset = 0,
                                     output_epsg = "4326") {
  .Deprecated("adc_weather_station", package = "rNDC")
  adc_weather_station(roi = polygon_wkt, token = token, page_size = page_size, page_offset = page_offset,
                      output_epsg = output_epsg)
}

#' @rdname rNDC-deprecated
#' @export
get_meteo_for_date <- function(stationid, date, token = Sys.getenv("ADC_TOKEN"), page_size = 500, page_offset = 0,
                               output_epsg = "4326") {
  .Deprecated("adc_weather_date", package = "rNDC")
  adc_weather_date(station = stationid, date = date, token = token, page_size = page_size,
                   page_offset = page_offset, output_epsg = output_epsg)
}

#' @rdname rNDC-deprecated
#' @export
get_meteo_for_period <- function(meteostation, fromdate, todate, token = Sys.getenv("ADC_TOKEN"), page_size = 500,
                                 page_offset = 0, output_epsg = "4326") {
  .Deprecated("adc_weather_period", package = "rNDC")
  adc_weather_period(station = meteostation, trange = c(deprecated_end(fromdate), deprecated_end(todate)), token = token,
                     page_size = page_size, page_offset = page_offset, output_epsg = output_epsg)
}

#' @rdname rNDC-deprecated
#' @export
get_meteo_for_long_period <- function(meteostation, fromdate, todate, token = Sys.getenv("ADC_TOKEN"), by_days = 7,
                                      page_size = 500, output_epsg = "4326", sleep_sec = 0) {
  .Deprecated("adc_weather_long_period", package = "rNDC")
  adc_weather_long_period(station = meteostation, trange = c(deprecated_end(fromdate), deprecated_end(todate)),
                          token = token, by_days = by_days, page_size = page_size, output_epsg = output_epsg,
                          sleep_sec = sleep_sec)
}

#' @rdname rNDC-deprecated
#' @export
download_avg_ndvi_month <- function(poly, year, month, epsg = 32631) {
  .Deprecated("gm_ndvi_month", package = "rNDC")
  gm_ndvi_month(roi = poly, year = year, month = month, epsg = epsg)
}

#' @rdname rNDC-deprecated
#' @export
download_avg_ndvi_stack <- function(poly, start_year, start_month, end_year, end_month, epsg = 32631) {
  .Deprecated("gm_ndvi_stack", package = "rNDC")
  gm_ndvi_stack(roi = poly, trange = c(sprintf("%04d-%02d-01", start_year, start_month),
                                       sprintf("%04d-%02d-01", end_year, end_month)), epsg = epsg)
}
