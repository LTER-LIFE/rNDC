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

# ---- internal helpers of the functions that download the rasters of items (ndc_ahn_raster(), ndc_soil_raster()) ----

# The cell size (m) that the title of an item states ("... 2019 (5m)", "... (2.5m, UTM31N)"), NA if it states none
stac_title_resolution <- function(title) {
  vapply(as.character(title), function(t) {
    found <- regmatches(t, regexec("\\(([0-9]+(\\.[0-9]+)?) ?m[,)]", t))[[1]]
    if (length(found) < 2) NA_real_ else as.numeric(found[2])
  }, 1, USE.NAMES = FALSE)
}

# The WCS scale factor of a request for the bounding box of `roi` from a raster with cells of `resolution` metres:
# `scalefactor` if it is given; otherwise 1, unless the request would have more than `max_cells` cells, and then the
# factor that makes it have `max_cells` (and a message says so). 1 when the resolution is not known.
stac_scalefactor <- function(roi, resolution, subset_crs, scalefactor = NULL, max_cells = 5e7) {
  if (!is.null(scalefactor)) {
    if (!is.numeric(scalefactor) || length(scalefactor) != 1 || is.na(scalefactor) || scalefactor <= 0 || scalefactor > 1) {
      stop("`scalefactor` must be a number above 0 and up to 1.", call. = FALSE)
    }
    return(scalefactor)
  }
  if (is.na(resolution) || is.null(max_cells) || is.na(max_cells)) return(1)
  bbox <- stac_bbox_in_crs(roi, target_crs = subset_crs)
  cells <- (bbox[["xmax"]] - bbox[["xmin"]]) * (bbox[["ymax"]] - bbox[["ymin"]]) / resolution^2
  if (cells <= max_cells) return(1)
  factor <- sqrt(max_cells / cells)
  message("The area has ", format(round(cells / 1e6), big.mark = ","), " million cells of ", resolution, " m: the raster is ",
          "requested with cells of about ", signif(resolution / factor, 2), " m (`scalefactor` ", signif(factor, 2),
          ") to keep it under ", format(round(max_cells / 1e6), big.mark = ","), " million cells (`max_cells`).")
  factor
}

# Download the `wcs` asset of every row of `meta` (a tibble from stac_collect_metadata(), with `href` and `title`) for the
# bounding box of `roi`, read it and clip it to `roi`. `name_of(row)` gives the name of a raster (and of its file, in
# `dir`). Returns list(rasters, files), both named. `transform(raster)` is applied to every clipped raster.
stac_download_items <- function(meta, roi, headers, dir, prefix, subset_crs, overwrite, scalefactor, max_cells, name_of,
                                transform = identity) {
  suffix <- stac_wcs_subset_suffix(roi, target_crs = subset_crs)
  rasters <- list()
  files <- list()
  for (i in seq_len(nrow(meta))) {
    name <- name_of(meta[i, , drop = FALSE])
    factor <- stac_scalefactor(roi, stac_title_resolution(meta$title[i]), subset_crs, scalefactor, max_cells)
    scaling <- if (factor < 1) paste0("&scalefactor=", format(factor, digits = 6, scientific = FALSE)) else ""
    file <- file.path(dir, paste0(prefix, name, ".tif"))
    ndc_progress(sprintf("Downloading %s (%d/%d)", name, i, nrow(meta)), i, nrow(meta))
    stac_download_one(paste0(meta$href[i], suffix, scaling), file, headers, overwrite = overwrite)
    r <- stac_clip_raster_to_aoi(terra::rast(file), roi)
    names(r) <- name
    rasters[[name]] <- transform(r)
    files[[name]] <- file
  }
  list(rasters = rasters, files = files)
}

# The rasters as one stack if they are on the same grid, otherwise NULL (rasters of different resolutions do not stack)
stac_stack_if_same_grid <- function(rasters) {
  if (length(rasters) == 0) return(NULL)
  same <- all(vapply(rasters[-1], function(r) terra::compareGeom(rasters[[1]], r, stopOnError = FALSE, messages = FALSE), NA))
  if (same) stac_build_stack(rasters) else NULL
}
