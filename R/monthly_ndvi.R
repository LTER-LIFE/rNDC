#' Average NDVI for a month
#'
#' Download the daily _GroenMonitor_ NDVI rasters of a month for the bounding box
#' of an area of interest and average them. Days without data are skipped.
#'
#' @param poly sf. Area of interest (its bounding box is requested).
#' @param year,month integer. Year and month.
#' @param epsg integer. EPSG code of the CRS in which the bounding box is requested.
#' @returns A single-layer `SpatRaster` named `ndvi_mean_YYYYMM`, or `NULL` if no data is available.
#' @seealso [ndc_with_progress()] to follow the downloads, or to stop them.
#' @export
download_avg_ndvi_month <- function(poly, year, month, epsg = 32631) {

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
