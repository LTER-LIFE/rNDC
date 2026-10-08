#' Monthly average NDVI over a period
#'
#' Compute the monthly average NDVI (see [download_avg_ndvi_month()]) for every
#' month between two months, and stack the results on a common grid.
#'
#' @param poly sf. Area of interest (its bounding box is requested).
#' @param start_year,start_month,end_year,end_month integer. First and last month (both included).
#' @param epsg integer. EPSG code of the CRS in which the bounding box is requested.
#' @returns A `SpatRaster` with one layer per month with data (named `ndvi_mean_YYYYMM`), or `NULL` if no data is available.
#' @seealso [ndc_with_progress()] to follow the downloads, or to stop them.
#' @export
download_avg_ndvi_stack <- function(poly, start_year, start_month, end_year, end_month, epsg = 32631) {

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
