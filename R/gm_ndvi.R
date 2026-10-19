# Monthly average NDVI from the GroenMonitor WCS: one month, a stack of months, and the helpers that they share.

#' Average NDVI for a month
#'
#' Download the daily _GroenMonitor_ NDVI rasters of a month for the bounding box
#' of an area of interest and average them. Days without data are skipped.
#'
#' @param roi character, numeric or sf. Region of interest, see [ndc_roi()]; its bounding box is requested.
#' @param year,month integer. Year and month.
#' @param epsg integer. EPSG code of the CRS in which the bounding box is requested.
#' @inheritParams ndc_get
#' @returns A single-layer `SpatRaster` named `ndvi_mean_YYYYMM`, or `NULL` if no data is available.
#' @seealso [ndc_with_progress()] to follow the downloads, or to stop them.
#' @export
gm_ndvi_month <- function(roi, year, month, epsg = 32631, params_file = NULL, save_params = NULL) {
  if (!is.null(params_file)) params_apply("gm_ndvi_month", params_file)
  if (!is.null(save_params)) params_save_when_done("gm_ndvi_month", save_params)

  message("Processing ", format(as.Date(sprintf("%04d-%02d-01", year, month)), "%B %Y"))

  tmpdir <- tempfile("ndvi_month_")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  files <- gm_download_month(year, month, gm_bbox(roi, epsg), tmpdir)
  message("Downloaded ", length(files), " NDVI layer(s) for the month")

  r_mean <- gm_month_mean(files, year, month)
  if (is.null(r_mean)) message("No NDVI data available for the selected month")
  r_mean
}

#' Monthly average NDVI over a period
#'
#' Compute the monthly average NDVI (see [gm_ndvi_month()]) for every
#' month from the one of the first date to the one of the last date of a period, and stack the results on a common grid.
#'
#' @param roi character, numeric or sf. Region of interest, see [ndc_roi()]; its bounding box is requested.
#' @param trange Date or character. The first and the last day of the period (two dates, or `"YYYY-MM-DD"` texts): the months of both are included.
#' @param epsg integer. EPSG code of the CRS in which the bounding box is requested.
#' @inheritParams ndc_get
#' @returns A `SpatRaster` with one layer per month with data (named `ndvi_mean_YYYYMM`), or `NULL` if no data is available.
#' @seealso [ndc_with_progress()] to follow the downloads, or to stop them.
#' @export
gm_ndvi_stack <- function(roi, trange, epsg = 32631, params_file = NULL, save_params = NULL) {
  if (!is.null(params_file)) params_apply("gm_ndvi_stack", params_file)
  if (!is.null(save_params)) params_save_when_done("gm_ndvi_stack", save_params)

  dates <- trange_dates(trange)
  start_date <- as.Date(format(dates[1], "%Y-%m-01"))
  end_date   <- as.Date(format(dates[2], "%Y-%m-01"))
  month_seq  <- seq(start_date, end_date, by = "month")
  bbox <- gm_bbox(roi, epsg)

  # the daily tiles are removed when the function ends, also when it is interrupted (see ndc_with_progress())
  tmpdir <- tempfile("ndvi_temp_")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  n_days <- sum(as.integer(ceiling_date(month_seq, "month") - month_seq))
  done_days <- 0L
  ndvi_stack_months(month_seq, function(year_m, month_m) {
    first <- as.Date(sprintf("%04d-%02d-01", year_m, month_m))
    files <- gm_download_month(year_m, month_m, bbox, tmpdir, done = done_days, total = n_days)
    done_days <<- done_days + as.integer(ceiling_date(first, "month") - first)
    files
  })
}

# Internal helpers shared by gm_ndvi_month() and gm_ndvi_stack() (gm_month_mean() also by ndc_ndvi_month() and ndc_ndvi_stack())

# Bounding box of the region `roi` in `epsg`, as the numbers of the WCS `subset` parameters
gm_bbox <- function(roi, epsg) {
  bb <- st_bbox(st_transform(ndc_roi(roi), epsg))
  vapply(c("xmin", "xmax", "ymin", "ymax"),
         function(k) format(unname(bb[k]), scientific = FALSE, digits = 15, trim = TRUE), "")
}

# Download the daily NDVI coverage of `date` for the bounding box `bbox` (see gm_bbox()) to `dir`. Returns the
# file path, or NULL when GroenMonitor has no coverage for that day (HTTP 404 `NoSuchCoverage`). Any other
# failure is retried (`retries` extra attempts, `pause` seconds apart) and then raised, with the message of the
# server, instead of being mistaken for a day without data.
gm_download_day <- function(date, bbox, dir, retries = 2L, pause = 1) {
  file <- file.path(dir, paste0("ndvi_", format(date, "%Y%m%d"), ".tif"))
  url <- gm_url("NDVI", params = c(date = format(date, "%Y%m%d"), bbox, format = "tiff"))

  for (attempt in seq_len(retries + 1L)) {
    result <- tryCatch({
      gm_get(url, output_path = file)
      file
    }, error = function(e) e)
    if (!inherits(result, "error")) return(result)

    if (inherits(result, "rNDC_http_error")) {
      if (result$status == 404) return(NULL)
      if (result$status < 500) stop(result)  # a request that will not work if repeated
    }
    if (attempt > retries) stop(result)
    Sys.sleep(pause)
  }
}

# Daily NDVI files of one month: reports each day with ndc_progress() (`done` days of `total` are done before
# this month) and returns the paths of the days with data.
gm_download_month <- function(year, month, bbox, dir, done = 0L, total = NA_integer_) {
  first <- as.Date(sprintf("%04d-%02d-01", year, month))
  dates <- seq(first, seq(first, length = 2, by = "month")[2] - 1, by = "day")
  if (is.na(total)) total <- length(dates)

  files <- character(0)
  for (k in seq_along(dates)) {
    ndc_progress(sprintf("Downloading NDVI %s (%d/%d)", dates[k], done + k, total), done + k, total)
    files <- c(files, gm_download_day(dates[k], bbox, dir))
  }
  files
}

# Read the daily rasters `files` and average them: a single layer named `ndvi_mean_YYYYMM`, or NULL if there is
# no data. Daily coverages can come on slightly different grids: they are aligned to the first one. The files are
# removed.
gm_month_mean <- function(files, year, month) {
  on.exit(unlink(files, force = TRUE), add = TRUE)
  if (length(files) == 0) return(NULL)

  daily <- tryCatch(rast(files), error = function(e) {
    rs <- Filter(Negate(is.null), lapply(files, function(f) tryCatch(rast(f), error = function(e2) NULL)))
    if (length(rs) == 0) stop("Failed to read any daily rasters", call. = FALSE)
    base <- rs[[1]]
    rs <- lapply(rs, function(r) {
      if (compareGeom(base, r, stopOnError = FALSE)) return(r)
      tryCatch(resample(r, base, method = "bilinear"), error = function(e3) extend(r, base))
    })
    do.call(c, rs)
  })

  r_mean <- app(daily, mean, na.rm = TRUE)
  r_mean[is.nan(r_mean)] <- NA
  if (all(is.na(values(r_mean)))) return(NULL)
  names(r_mean) <- paste0("ndvi_mean_", year, sprintf("%02d", month))
  r_mean
}
