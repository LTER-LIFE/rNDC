# STAC helpers: the rasters that were downloaded: clip them to the area, combine them, name their files. See
# `stac_make_headers()` for the documentation of the helpers.

#' @rdname stac_helpers
#' @export
stac_clip_raster_to_aoi <- function(r, roi, aoi = NULL) {
  if (!is.null(aoi)) roi <- deprecated_argument(aoi, "aoi", "roi", "stac_clip_raster_to_aoi")
  if (is.null(r)) return(NULL)

  aoi_vect <- terra::vect(ndc_roi(roi))
  r_crs <- terra::crs(r)
  if (is.na(r_crs) || !nzchar(r_crs)) {
    stop("Downloaded raster has no CRS, so it cannot be clipped safely.", call. = FALSE)
  }

  aoi_proj <- terra::project(aoi_vect, r_crs)
  r_clip <- terra::crop(r, aoi_proj)
  r_clip <- terra::mask(r_clip, aoi_proj)
  r_clip
}

#' @rdname stac_helpers
stac_make_file_prefix <- function(x) {
  if (is.null(x) || !nzchar(as.character(x))) return("")
  paste0(gsub("[^A-Za-z0-9_\\-]+", "_", as.character(x)), "_")
}

#' @rdname stac_helpers
stac_build_stack <- function(clipped) {
  if (length(clipped) == 0) return(NULL)
  if (length(clipped) == 1) clipped[[1]] else terra::rast(clipped)
}
