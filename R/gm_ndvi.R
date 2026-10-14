# Monthly average NDVI from the GroenMonitor WCS: one month, a stack of months, and the helpers that they share.

#' Average NDVI for a month
#'
#' Download the daily _GroenMonitor_ NDVI rasters of a month for the bounding box
#' of an area of interest and average them. Days without data are skipped.
#'
#' @param poly sf. Area of interest (its bounding box is requested).
#' @param year,month integer. Year and month.
#' @param epsg integer. EPSG code of the CRS in which the bounding box is requested.
#' @inheritParams ndc_get
#' @returns A single-layer `SpatRaster` named `ndvi_mean_YYYYMM`, or `NULL` if no data is available.
#' @seealso [ndc_with_progress()] to follow the downloads, or to stop them.
#' @export
download_avg_ndvi_month <- function(poly, year, month, epsg = 32631, params_file = NULL, save_params = NULL) {
  if (!is.null(params_file)) params_apply("download_avg_ndvi_month", params_file)
  if (!is.null(save_params)) params_save_when_done("download_avg_ndvi_month", save_params)

  message("Processing ", format(as.Date(sprintf("%04d-%02d-01", year, month)), "%B %Y"))

  tmpdir <- tempfile("ndvi_month_")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  files <- gm_download_month(year, month, gm_bbox(poly, epsg), tmpdir)
  message("Downloaded ", length(files), " NDVI layer(s) for the month")

  r_mean <- gm_month_mean(files, year, month)
  if (is.null(r_mean)) message("No NDVI data available for the selected month")
  r_mean
}

#' Monthly average NDVI over a period
#'
#' Compute the monthly average NDVI (see [download_avg_ndvi_month()]) for every
#' month between two months, and stack the results on a common grid.
#'
#' @param poly sf. Area of interest (its bounding box is requested).
#' @param start_year,start_month,end_year,end_month integer. First and last month (both included).
#' @param epsg integer. EPSG code of the CRS in which the bounding box is requested.
#' @inheritParams ndc_get
#' @returns A `SpatRaster` with one layer per month with data (named `ndvi_mean_YYYYMM`), or `NULL` if no data is available.
#' @seealso [ndc_with_progress()] to follow the downloads, or to stop them.
#' @export
download_avg_ndvi_stack <- function(poly, start_year, start_month, end_year, end_month, epsg = 32631,
                                    params_file = NULL, save_params = NULL) {
  if (!is.null(params_file)) params_apply("download_avg_ndvi_stack", params_file)
  if (!is.null(save_params)) params_save_when_done("download_avg_ndvi_stack", save_params)

  start_date <- as.Date(sprintf("%04d-%02d-01", start_year, start_month))
  end_date   <- as.Date(sprintf("%04d-%02d-01", end_year, end_month))
  month_seq  <- seq(start_date, end_date, by = "month")
  bbox <- gm_bbox(poly, epsg)

  # the daily tiles are removed when the function ends, also when it is interrupted (see ndc_with_progress())
  tmpdir <- tempfile("ndvi_temp_")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  n_days <- sum(as.integer(ceiling_date(month_seq, "month") - month_seq))
  done_days <- 0L
  raster_list <- list()

  for (m in as.list(month_seq)) {
    message("Processing ", format(m, "%B %Y"))
    year_m <- year(m)
    month_m <- as.integer(format(m, "%m"))

    files <- gm_download_month(year_m, month_m, bbox, tmpdir, done = done_days, total = n_days)
    done_days <- done_days + as.integer(ceiling_date(m, "month") - m)

    r_mean <- gm_month_mean(files, year_m, month_m)
    if (is.null(r_mean)) {
      message("No NDVI data available for ", format(m, "%B %Y"))
    } else {
      raster_list[[length(raster_list) + 1]] <- r_mean
    }
  }

  if (length(raster_list) == 0) {
    message("No NDVI data available for the selected range")
    return(NULL)
  }

  # The WCS snaps each month's coverage to its own acquisition grid, so the monthly means can come back on
  # slightly different extents, which rast() cannot combine. Align them to the largest one (nothing is cropped).
  # The layer names (ndvi_mean_YYYYMM) are kept, also when some months had no data.
  if (length(raster_list) > 1) {
    ref <- raster_list[[which.max(vapply(raster_list, terra::ncell, numeric(1)))]]
    raster_list <- lapply(raster_list, function(r) {
      if (terra::compareGeom(ref, r, stopOnError = FALSE, messages = FALSE)) r
      else terra::resample(r, ref, method = "bilinear")
    })
  }

  terra::rast(raster_list)
}

# Internal helpers shared by download_avg_ndvi_month() and download_avg_ndvi_stack()

# Bounding box of `poly` in `epsg`, as the numbers of the WCS `subset` parameters
gm_bbox <- function(poly, epsg) {
  bb <- st_bbox(st_transform(poly, epsg))
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
      gm_get(url, out_path = file)
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
