# NDVI from the AgroDataCube: the statistics (mean and standard deviation) of the NDVI of agricultural fields on the
# dates that satellite imagery exists for.

# The longest WKT (characters) that is sent to the AgroDataCube in the URL of a request
adc_max_wkt_chars <- 5000L

#' NDVI of agricultural fields from the AgroDataCube
#'
#' Retrieve the NDVI that the _AgroDataCube_ calculates for agricultural fields from satellite imagery: for each field
#' and date with an observation, the mean and the standard deviation of the NDVI of the pixels of the field. These are
#' statistics per field, not rasters (for rasters see [gm_ndvi_stack()] and [ndc_ndvi_stack()]).
#'
#' Give the fields as an area (`roi`), for the fields that intersect it, or as field ids (`fieldid`), but not both. The
#' AgroDataCube answers for one date at a time, so a request is made for every date of the period (and, with
#' `fieldid`, for every field): a long period or many fields means many requests. Dates without imagery have no rows.
#' A field id is valid for one year (it is unique per field per year, and the same parcel has another id in another
#' year), so ask for the dates of that year. The fields are not returned as geometries: [adc_get_all()] with
#' `option = "Fields"` gives them.
#'
#' @param roi character, numeric or sf. The area whose fields are wanted, see [ndc_roi()] (or a WKT string in EPSG:4326).
#'   Its outline is sent in the request, so a very detailed area is too long for the AgroDataCube (the limit is
#'   about 5000 characters of WKT): simplify it with `sf::st_simplify()` first.
#' @param fieldid character or numeric. One or more field ids, instead of `roi`.
#' @param trange Date or character. The first and the last day of the period (two dates, or `"YYYY-MM-DD"` texts); a
#'   single day is given twice.
#' @param page_size integer. Number of fields requested per page when `roi` is used (all pages are retrieved).
#' @param token character. _AgroDataCube_ API token (by default the environment variable `ADC_TOKEN`).
#' @inheritParams ndc_get
#' @returns A tibble with a row for every field and date with data: `fieldid`, `date`, `ndvi_avg` and `ndvi_stddev`. It
#'   has no rows if nothing matches.
#' @seealso [ndc_with_progress()] to follow the requests, or to stop them; [ndc_ndvi_stats()] for the NDVI statistics of
#'   the project areas of the NatureDataCube.
#' @export
adc_ndvi <- function(roi = NULL, fieldid = NULL, trange, token = Sys.getenv("ADC_TOKEN"), page_size = 1000L,
                     params_file = NULL, save_params = NULL) {
  if (!is.null(params_file)) params_apply("adc_ndvi", params_file)
  if (!is.null(save_params)) params_save_when_done("adc_ndvi", save_params)

  if (is.null(roi) == is.null(fieldid)) {
    stop("Give either `roi` (an area) or `fieldid` (field ids), not both and not neither.", call. = FALSE)
  }
  check_token(token, "AgroDataCube", "ADC_TOKEN")
  dates <- trange_dates(trange)
  days <- seq(dates[1], dates[2], by = "day")

  if (is.null(roi)) {
    fieldid <- unique(as.character(fieldid[!is.na(fieldid)]))
    if (length(fieldid) == 0) stop("`fieldid` has no field ids.", call. = FALSE)
    requests <- expand.grid(date = days, fieldid = fieldid, stringsAsFactors = FALSE)
    geometry <- NULL
  } else {
    requests <- data.frame(date = days, fieldid = NA_character_, stringsAsFactors = FALSE)
    geometry <- adc_roi_wkt(roi)
  }

  rows <- vector("list", nrow(requests))
  for (i in seq_len(nrow(requests))) {
    day <- format(requests$date[i], "%Y%m%d")
    ndc_progress(sprintf("Requesting NDVI %s%s (%d/%d)", requests$date[i],
                         if (is.null(geometry)) paste0(" of field ", requests$fieldid[i]) else "", i, nrow(requests)),
                 i, nrow(requests))
    features <- if (is.null(geometry)) {
      adc_get(option = "NDVI", params = c(fieldid = requests$fieldid[i], date = day), token = token)$features
    } else {
      adc_get_all("NDVI", c(geometry = geometry, epsg = "4326", date = day), token = token, page_size = page_size)$features
    }
    rows[[i]] <- adc_ndvi_rows(features)
  }

  result <- dplyr::bind_rows(rows)
  if (nrow(result) == 0) {
    return(tibble::tibble(fieldid = integer(), date = as.Date(character()), ndvi_avg = numeric(), ndvi_stddev = numeric()))
  }
  result[order(result$date, result$fieldid), , drop = FALSE]
}

# Internal helpers of adc_ndvi()

# The WKT of the area `roi` (one outline: several geometries are joined) in EPSG:4326, for the URL of a request. A text
# that is no file is a WKT already.
adc_roi_wkt <- function(roi) {
  if (is.character(roi) && length(roi) == 1 && !file.exists(roi)) {
    wkt <- roi
  } else {
    wkt <- sf::st_as_text(sf::st_union(ndc_roi(roi)), digits = 6)  # 6 decimals: about 0.1 m
  }
  if (nchar(wkt) > adc_max_wkt_chars) {
    stop("The outline of `roi` is too long for the AgroDataCube (", nchar(wkt), " characters; the limit is about ",
         adc_max_wkt_chars, "): simplify it first, for example with sf::st_simplify().", call. = FALSE)
  }
  wkt
}

# The features of a response of the NDVI service as a tibble (NULL if there are none)
adc_ndvi_rows <- function(features) {
  if (length(features) == 0) return(NULL)
  field <- function(name, type) vapply(features, function(f) f$properties[[name]] %||% type[NA], type)
  tibble::tibble(fieldid = field("fieldid", integer(1)),
                 date = as.Date(field("datum", character(1))),
                 ndvi_avg = field("ndvi_avg", numeric(1)),
                 ndvi_stddev = field("ndvi_stddev", numeric(1)))
}
