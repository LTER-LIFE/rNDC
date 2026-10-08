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
