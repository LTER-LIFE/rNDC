# Nitrogen / Nature Data Cube retrieval helpers
# --------------------------------------------

nitrogen_asset_name <- "wcs"
nitrogen_subset_crs <- 32631L

nitrogen_layer_choices <- c("ntot", "nox", "nh3")


nitrogen_clean_layer_name <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[nzchar(x)]
}

nitrogen_normalize_year <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  ifelse(nzchar(x), x, NA_character_)
}

nitrogen_controls_are_valid <- function(year, layers) {
  year_ok <- !is.null(year) && !is.na(year) && nzchar(as.character(year))
  layers_ok <- !is.null(layers) && length(layers) > 0 && any(nzchar(as.character(layers)))
  year_ok && layers_ok
}

nitrogen_collect_metadata <- function(aoi, token, endpoint = ndc_endpoint(),
                                      collection = NULL,
                                      layers = nitrogen_layer_choices,
                                      year = NULL,
                                      limit = 100) {
  # Each nitrogen layer lives in its own collection (named after the layer)
  collections <- if (is.null(collection)) layers else collection
  meta <- dplyr::bind_rows(lapply(collections, function(col) {
    stac_collect_metadata(
      aoi = aoi,
      token = token,
      endpoint = endpoint,
      collection = col,
      asset_name = nitrogen_asset_name,
      limit = limit
    )
  }))

  if (nrow(meta) == 0) {
    stop("No Nature Data Cube raster items were found for the selected AOI.", call. = FALSE)
  }

  keep <- !is.na(meta$layer) & !is.na(meta$year) & !is.na(meta$href) & meta$layer %in% layers
  if (!is.null(year)) {
    keep <- keep & meta$year %in% as.character(year)
  }
  meta <- meta[keep, , drop = FALSE]
  meta <- meta[!duplicated(meta[c("layer", "year")]), , drop = FALSE]

  if (nrow(meta) == 0) {
    stop(
      paste0(
        "No Nature Data Cube items matched the requested layer(s) (",
        paste(layers, collapse = ", "),
        ") and year (",
        paste(year, collapse = ", "),
        ")."
      ),
      call. = FALSE
    )
  }

  meta
}

#' Download Nitrogen raster layers for an area of interest
#'
#' @param aoi An sf object (area of interest).
#' @param year Year to retrieve (e.g. "2024", "2025", "2040").
#' @param layers Character vector of layer names. Defaults to all available
#'   nitrogen layers (\code{nitrogen_layer_choices}).
#' @param token API token (defaults to the NDC_TOKEN env var).
#' @param endpoint STAC endpoint.
#' @param collection STAC collection(s). By default, one collection per layer (`ntot`, `nox`, `nh3`).
#' @param out_dir Output directory for downloaded rasters.
#' @param overwrite Overwrite existing files.
#' @param limit Max STAC items to fetch.
#' @param file_prefix Optional file prefix.
#' @return A list with the rasters, the raster stack, the file paths and the metadata.
#' @export
get_nitrogen_raster <- function(aoi, year, layers = nitrogen_layer_choices, token = Sys.getenv("NDC_TOKEN"),
                                endpoint = ndc_endpoint(),
                                collection = NULL,
                                out_dir = tempdir(),
                                overwrite = TRUE,
                                limit = 100,
                                file_prefix = NULL) {
  year <- nitrogen_normalize_year(year)
  layers <- nitrogen_clean_layer_name(layers)

  if (!nitrogen_controls_are_valid(year, layers)) {
    stop("Please select a year and at least one nitrogen layer.", call. = FALSE)
  }

  meta <- nitrogen_collect_metadata(
    aoi = aoi,
    token = token,
    endpoint = endpoint,
    collection = collection,
    layers = layers,
    year = year,
    limit = limit
  )

  headers <- stac_make_headers(token)
  prefix <- stac_make_file_prefix(file_prefix)

  download_dir <- if (is.null(out_dir) || !nzchar(as.character(out_dir))) tempdir() else out_dir
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  # Only request the area of interest from the WCS server (not the full raster)
  subset_suffix <- stac_wcs_subset_suffix(aoi, target_crs = nitrogen_subset_crs)

  downloaded <- list()
  clipped <- list()

  for (i in seq_len(nrow(meta))) {
    lyr <- meta$layer[i]
    yr <- meta$year[i]
    nm <- paste0(lyr, "_", yr)
    fpath <- file.path(download_dir, paste0(prefix, nm, ".tif"))

    stac_download_one(paste0(meta$href[i], subset_suffix), fpath, headers,
                      overwrite = overwrite)

    r <- terra::rast(fpath)
    r <- stac_clip_raster_to_aoi(r, aoi)
    names(r) <- nm

    downloaded[[nm]] <- fpath
    clipped[[nm]] <- r
  }

  list(
    rasters = clipped,
    stack = stac_build_stack(clipped),
    files = downloaded,
    metadata = meta
  )
}

get_nitrogen_stats <- function(aoi, year, layers, token = Sys.getenv("NDC_TOKEN"),
                               endpoint = ndc_endpoint(),
                               collection = NULL,
                               out_dir = tempdir(),
                               overwrite = TRUE,
                               limit = 100,
                               file_prefix = NULL) {
  raster_res <- get_nitrogen_raster(
    aoi = aoi,
    year = year,
    layers = layers,
    token = token,
    endpoint = endpoint,
    collection = collection,
    out_dir = out_dir,
    overwrite = overwrite,
    limit = limit,
    file_prefix = file_prefix
  )

  meta <- raster_res$metadata
  out <- tibble::tibble(ID = 1L)

  for (i in seq_len(nrow(meta))) {
    nm <- paste0(meta$layer[i], "_", meta$year[i], ".tif")
    rnm <- paste0(meta$layer[i], "_", meta$year[i])
    r <- raster_res$rasters[[rnm]]
    if (is.null(r)) next

    val <- terra::global(r, fun = mean, na.rm = TRUE)[1, 1]
    out[[nm]] <- as.numeric(val)
  }

  out
}
