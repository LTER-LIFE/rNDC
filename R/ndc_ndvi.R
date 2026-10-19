# Monthly average NDVI rasters through the NatureDataCube STAC API: the items of the `ndvi` collection are the daily
# GroenMonitor coverages, so the search tells which days have data and the `wcs` asset of an item is the download.

# The collection with the daily NDVI coverages, and the CRS of the WCS subsets
ndvi_collection <- "ndvi"
ndvi_subset_crs <- 32631L

#' Average NDVI for a month, found through the NatureDataCube
#'
#' Search the `ndvi` collection of the NatureDataCube for the days of a month with NDVI data for an area, download
#' the rasters of those days (the `wcs` asset of the items, for the bounding box of the area) and average them. The
#' rasters are the daily _GroenMonitor_ coverages that [gm_ndvi_month()] downloads from the GroenMonitor directly; here
#' the NatureDataCube says which days exist (so no request is made for a day without data) and where the data is.
#'
#' @param roi character, numeric or sf. Region of interest, see [ndc_roi()]; its bounding box is requested.
#' @param year,month integer. Year and month.
#' @param collection character. STAC collection with the daily NDVI rasters.
#' @param subset_crs integer. EPSG code of the CRS in which the bounding box is requested.
#' @param limit integer. Number of items per page of the search.
#' @param clip logical. If `TRUE`, the result is clipped to the area (cells outside it are `NA`); by default it covers
#'   the bounding box, like [gm_ndvi_month()].
#' @param token character. API token.
#' @inheritParams ndc_get
#' @returns A single-layer `SpatRaster` named `ndvi_mean_YYYYMM`, or `NULL` if no data is available.
#' @seealso [ndc_ndvi_stack()] for several months, [gm_ndvi_month()] to download from the GroenMonitor,
#'   [ndc_ndvi_stats()] for the NDVI statistics of the project areas, [ndc_with_progress()] to follow the downloads, or
#'   to stop them.
#' @export
ndc_ndvi_month <- function(roi, year, month, token = Sys.getenv("NDC_TOKEN"), collection = ndvi_collection,
                           subset_crs = ndvi_subset_crs, limit = 100, clip = FALSE, params_file = NULL, save_params = NULL) {
  if (!is.null(params_file)) params_apply("ndc_ndvi_month", params_file)
  if (!is.null(save_params)) params_save_when_done("ndc_ndvi_month", save_params)

  r <- ndc_ndvi_months(roi, as.Date(sprintf("%04d-%02d-01", year, month)), token, collection, subset_crs, limit, clip)
  if (is.null(r)) NULL else r[[1]]
}

#' Monthly average NDVI over a period, found through the NatureDataCube
#'
#' Compute the monthly average NDVI (see [ndc_ndvi_month()]) for every month from the one of the first date to the one of
#' the last date of a period, and stack the results on a common grid. The area is searched once for the whole period.
#'
#' @param trange Date or character. The first and the last day of the period (two dates, or `"YYYY-MM-DD"` texts): the
#'   months of both are included.
#' @inheritParams ndc_ndvi_month
#' @returns A `SpatRaster` with one layer per month with data (named `ndvi_mean_YYYYMM`), or `NULL` if no data is
#'   available.
#' @seealso [ndc_ndvi_month()], [gm_ndvi_stack()] to download from the GroenMonitor, [ndc_with_progress()] to follow the
#'   downloads, or to stop them.
#' @export
ndc_ndvi_stack <- function(roi, trange, token = Sys.getenv("NDC_TOKEN"), collection = ndvi_collection,
                           subset_crs = ndvi_subset_crs, limit = 100, clip = FALSE, params_file = NULL, save_params = NULL) {
  if (!is.null(params_file)) params_apply("ndc_ndvi_stack", params_file)
  if (!is.null(save_params)) params_save_when_done("ndc_ndvi_stack", save_params)

  dates <- trange_dates(trange)
  month_seq <- seq(as.Date(format(dates[1], "%Y-%m-01")), as.Date(format(dates[2], "%Y-%m-01")), by = "month")
  ndc_ndvi_months(roi, month_seq, token, collection, subset_crs, limit, clip)
}

# Internal helpers shared by ndc_ndvi_month() and ndc_ndvi_stack()

# The monthly means of the months `month_seq` (the first day of each): one search for all of them, the rasters of the
# days with data, a stack with a layer per month with data (NULL if there is none).
ndc_ndvi_months <- function(roi, month_seq, token, collection, subset_crs, limit, clip) {
  last <- seq(max(month_seq), by = "month", length.out = 2)[2] - 1
  items <- ndc_ndvi_items(roi, min(month_seq), last, token, collection, limit)

  # the daily files are removed when the function ends, also when it is interrupted (see ndc_with_progress())
  tmpdir <- tempfile("ndvi_stac_")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  headers <- stac_make_headers(token)
  suffix <- stac_wcs_subset_suffix(roi, target_crs = subset_crs)
  done <- 0L
  stack <- ndvi_stack_months(month_seq, function(year_m, month_m) {
    rows <- items[format(items$date, "%Y%m") == sprintf("%04d%02d", year_m, month_m), , drop = FALSE]
    files <- ndc_ndvi_download(rows, suffix, headers, tmpdir, done = done, total = nrow(items))
    done <<- done + nrow(rows)
    message("Downloaded ", length(files), " NDVI layer(s) for the month")
    files
  })

  if (clip && !is.null(stack)) stack <- stac_clip_raster_to_aoi(stack, roi)
  stack
}

# The items of the NDVI collection for `roi` from the day `first` to the day `last`: a tibble with the `id`, the `href` of
# the `wcs` asset and the `date` of each, in order of date (no rows if there are none)
ndc_ndvi_items <- function(roi, first, last, token, collection, limit) {
  meta <- stac_collect_metadata(roi, token, ndc_endpoint(), collection, asset_name = "wcs", limit = limit,
                                trange = c(format(first, "%Y-%m-%dT00:00:00Z"), format(last, "%Y-%m-%dT23:59:59Z")))
  if (nrow(meta) == 0) return(tibble::tibble(id = character(), href = character(), date = as.Date(character())))

  meta$date <- as.Date(substr(meta$observation_date, 1, 10))
  meta <- meta[!is.na(meta$date) & !is.na(meta$href) & meta$date >= first & meta$date <= last, , drop = FALSE]
  meta[order(meta$date), c("id", "href", "date"), drop = FALSE]
}

# Download the `wcs` asset of each row of `rows` (see ndc_ndvi_items()) with the subset `suffix` to `dir`, and report each
# with ndc_progress() (`done` of `total` are done before). Returns the paths of the files.
ndc_ndvi_download <- function(rows, suffix, headers, dir, done = 0L, total = nrow(rows)) {
  files <- character(nrow(rows))
  for (k in seq_len(nrow(rows))) {
    ndc_progress(sprintf("Downloading NDVI %s (%d/%d)", rows$date[k], done + k, total), done + k, total)
    files[k] <- file.path(dir, paste0(gsub("[^A-Za-z0-9_.-]", "_", rows$id[k]), ".tif"))
    stac_download_one(paste0(rows$href[k], suffix), files[k], headers)
  }
  files
}
