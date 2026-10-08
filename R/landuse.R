# Nature Data Cube Land Use raster retrieval helpers
# ---------------------------------------------------

landuse_collection <- "lgn"
landuse_asset_name <- "wcs"
landuse_default_year <- 2024L
landuse_subset_crs <- 32631L
landuse_min_file_size <- 100000L

landuse_normalize_year <- function(year) {
  if (is.null(year) || length(year) == 0) return(NA_integer_)
  year <- suppressWarnings(as.integer(as.character(year)[1]))
  if (is.na(year)) NA_integer_ else year
}

landuse_collect_metadata <- function(aoi, token = Sys.getenv("NDC_TOKEN"),
                                     endpoint = ndc_endpoint(),
                                     collection = landuse_collection,
                                     limit = 100,
                                     year = NULL) {
  meta <- stac_collect_metadata(
    aoi = aoi,
    token = token,
    endpoint = endpoint,
    collection = collection,
    asset_name = landuse_asset_name,
    limit = limit,
    trange = stac_year_trange(year)
  )

  if (nrow(meta) == 0) {
    return(meta)
  }

  meta$obs_date <- suppressWarnings(as.Date(meta$observation_date))
  meta <- meta[!is.na(meta$href) & nzchar(meta$href), , drop = FALSE]
  meta <- meta[!duplicated(meta$href), , drop = FALSE]
  meta[order(meta$obs_date, meta$title, meta$id), , drop = FALSE]
}

#' Download Land Use raster for an area of interest
#'
#' @param aoi sf, sfc, numeric or character. Area of interest, see [ndc_roi()]: an sf/sfc object, a
#'   bounding box (`xmin, ymin, xmax, ymax`, EPSG:4326) or a path to a file with the geometry.
#' @param year Year to retrieve (defaults to \code{landuse_default_year}).
#' @param token API token (defaults to the NDC_TOKEN env var).
#' @param endpoint,collection STAC endpoint and collection.
#' @param out_dir Output directory for the downloaded raster.
#' @param overwrite Overwrite existing files.
#' @param limit Max STAC items to fetch.
#' @param file_prefix Optional file prefix.
#' @param subset_crs,min_file_size Internal retrieval parameters.
#' @return A list with `rasters` (a named list of clipped `SpatRaster`s), `stack` (a `SpatRaster` with one layer
#'   per raster), `files` (the paths of the downloaded files) and `metadata` (a tibble with one row per STAC
#'   item). An error is raised if no item matches the area and year.
#' @export
get_landuse_raster <- function(aoi,
                               year = landuse_default_year,
                               token = Sys.getenv("NDC_TOKEN"),
                               endpoint = ndc_endpoint(),
                               collection = landuse_collection,
                               out_dir = tempdir(),
                               overwrite = TRUE,
                               limit = 100,
                               file_prefix = NULL,
                               subset_crs = landuse_subset_crs,
                               min_file_size = landuse_min_file_size) {
  year <- landuse_normalize_year(year)
  if (is.na(year)) {
    stop("Please provide a valid year for Land Use.", call. = FALSE)
  }

  meta <- landuse_collect_metadata(
    aoi = aoi,
    token = token,
    endpoint = endpoint,
    collection = collection,
    limit = limit,
    year = year
  )

  if (nrow(meta) == 0) {
    stop(paste0("No Land Use raster items were found for the selected AOI and year ", year, "."), call. = FALSE)
  }

  meta <- meta[!is.na(meta$obs_date) & lubridate::year(meta$obs_date) == year, , drop = FALSE]
  if (nrow(meta) == 0) {
    stop(
      paste0("No Land Use raster items matched year ", year, "."),
      call. = FALSE
    )
  }

  headers <- stac_make_headers(token)
  prefix <- stac_make_file_prefix(file_prefix)

  download_dir <- if (is.null(out_dir) || !nzchar(as.character(out_dir))) tempdir() else out_dir
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  subset_suffix <- stac_wcs_subset_suffix(aoi, target_crs = subset_crs, x_name = "E", y_name = "N")

  downloaded <- list()
  clipped <- list()

  for (i in seq_len(nrow(meta))) {
    obs <- meta$obs_date[i]
    base_nm <- if (!is.na(obs)) paste0("LGN_", format(obs, "%Y")) else paste0("LGN_", year)
    if (nrow(meta) > 1) base_nm <- paste0(base_nm, "_", i)

    fname <- paste0(prefix, base_nm, ".tif")
    fpath <- file.path(download_dir, fname)
    url_clip <- paste0(meta$href[i], subset_suffix)

    stac_download_one(
      url_clip,
      fpath,
      headers,
      overwrite = overwrite,
      retries = 3L,
      min_file_size = min_file_size
    )

    if (!file.exists(fpath)) {
      stop(
        paste0("Land Use download did not create a file for ", base_nm, "."),
        call. = FALSE
      )
    }

    file_size <- suppressWarnings(file.info(fpath)$size)
    if (is.na(file_size) || file_size < min_file_size) {
      if (file.exists(fpath)) unlink(fpath)
      stop(
        paste0(
          "Downloaded Land Use file looks invalid or incomplete for ", base_nm,
          " (size: ", file_size, " bytes)."
        ),
        call. = FALSE
      )
    }

    r <- terra::rast(fpath)
    r <- stac_clip_raster_to_aoi(r, aoi)
    names(r) <- base_nm

    downloaded[[base_nm]] <- fpath
    clipped[[base_nm]] <- r
  }

  stack <- stac_build_stack(clipped)

  list(
    rasters = clipped,
    stack = stack,
    files = downloaded,
    metadata = meta
  )
}
