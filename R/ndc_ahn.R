# AHN (Actueel Hoogtebestand Nederland) rasters of the NatureDataCube: the terrain height (DTM) and the object height (OHN)
# ------------------------------------------------------------------------------------------------------------------

ahn_collection <- "ahnmetrics"
ahn_layers <- c("dtm", "ohn")
ahn_subset_crs <- 32631L
ahn_max_cells <- 2e7
# the rasters hold centimetres
ahn_cm_per_m <- 100

# The items of the AHN collection for `roi`, for the `layers` and the `year`s asked for (one per layer and year)
ahn_collect_metadata <- function(roi, token, collection = ahn_collection, layers = ahn_layers, year = NULL, limit = 100) {
  meta <- stac_collect_metadata(roi = roi, token = token, endpoint = ndc_endpoint(), collection = collection,
                                asset_name = "wcs", limit = limit)
  if (nrow(meta) == 0) stop("No AHN raster items were found for the selected area.", call. = FALSE)

  keep <- !is.na(meta$layer) & !is.na(meta$href) & tolower(meta$layer) %in% layers
  if (!is.null(year)) keep <- keep & !is.na(meta$year) & meta$year %in% as.character(year)
  meta <- meta[keep, , drop = FALSE]
  meta$layer <- tolower(meta$layer)
  meta <- meta[!duplicated(meta[c("layer", "year")]), , drop = FALSE]
  if (nrow(meta) == 0) {
    stop("No AHN items matched the requested layer(s) (", paste(layers, collapse = ", "), ")",
         if (!is.null(year)) paste0(" and year (", paste(year, collapse = ", "), ")") else "", ".", call. = FALSE)
  }
  meta[order(meta$layer, meta$year), , drop = FALSE]
}

# The layers to ask for, checked
ahn_check_layers <- function(layers) {
  layers <- unique(tolower(trimws(as.character(layers))))
  layers <- layers[nzchar(layers)]
  unknown <- setdiff(layers, ahn_layers)
  if (length(layers) == 0 || length(unknown) > 0) {
    stop("`layers` must be one or more of: ", paste(ahn_layers, collapse = ", "),
         if (length(unknown) > 0) paste0(" (not known: ", paste(unknown, collapse = ", "), ")") else "", ".", call. = FALSE)
  }
  layers
}

#' Download AHN rasters for an area of interest
#'
#' Download the height rasters of the _Actueel Hoogtebestand Nederland_ (AHN) that the NatureDataCube publishes in the
#' `ahnmetrics` collection: the terrain height (`"dtm"`, the digital terrain model of AHN3, 5 m cells) and the object
#' height (`"ohn"`, vegetation and buildings, from AHN4, 2.5 m cells). Only the bounding box of the area is requested
#' from the server, and the rasters are clipped to the area. The values are heights in metres (see `scale`).
#'
#' A raster of a large area has a lot of cells (the 2.5 m raster of 800 km\eqn{^2} has more than 100 million, and takes
#' a minute to download): when the area is larger than `max_cells` cells, the raster is requested at a lower resolution
#' (the server does the scaling; a message says so). Give `scalefactor` to ask for a lower resolution yourself.
#'
#' @param roi sf, sfc, numeric or character. Region of interest, see [ndc_roi()]: an sf/sfc object, a
#'   bounding box (`xmin, ymin, xmax, ymax`, EPSG:4326) or a path to a file with the geometry.
#' @param layers character. The layers: `"dtm"` (terrain height) and/or `"ohn"` (object height); see [ndc_ahn_layers()].
#' @param year integer or character. Year(s) of the items to retrieve (`NULL`, the default, for all the years); see
#'   [ndc_ahn_years()].
#' @param token character. API token (by default the environment variable `NDC_TOKEN`).
#' @param collection character. STAC collection with the AHN rasters.
#' @param output_dir character. Output directory for the downloaded rasters.
#' @param overwrite logical. Overwrite existing files.
#' @param limit integer. Number of STAC items per page of the search.
#' @param file_prefix character. Optional file prefix.
#' @param subset_crs integer. EPSG code of the CRS in which the bounding box is requested.
#' @param scale logical. The rasters hold centimetres; if `TRUE` (the default) the values are in metres, otherwise they
#'   are as published.
#' @param scalefactor numeric. A number above 0 and up to 1: the fraction of the cells per side to request (`0.5` gives
#'   cells twice as large). By default (`NULL`) it is 1, or less for an area with more than `max_cells` cells.
#' @param max_cells numeric. The most cells that a raster may have before the resolution is lowered.
#' @inheritParams ndc_get
#' @returns A list with `rasters` (a named list of clipped `SpatRaster`s, named `AHN_<LAYER>_<year>`), `stack` (a
#'   `SpatRaster` with one layer per raster, or `NULL` if the rasters are not on the same grid, as the 5 m and 2.5 m
#'   ones are not), `files` (the paths of the downloaded files, in centimetres) and `metadata` (a tibble with one
#'   row per item). An error is raised if no item matches.
#' @seealso [ndc_ahn_stats()] for the statistics of the heights in the area, [ndc_with_progress()] to follow the
#'   downloads, or to stop them.
#' @export
ndc_ahn_raster <- function(roi,
                           layers = ahn_layers,
                           year = NULL,
                           token = Sys.getenv("NDC_TOKEN"),
                           collection = ahn_collection,
                           output_dir = tempdir(),
                           overwrite = TRUE,
                           limit = 100,
                           file_prefix = NULL,
                           subset_crs = ahn_subset_crs,
                           scale = TRUE,
                           scalefactor = NULL,
                           max_cells = ahn_max_cells,
                           params_file = NULL,
                           save_params = NULL) {
  if (!is.null(params_file)) params_apply("ndc_ahn_raster", params_file)
  if (!is.null(save_params)) params_save_when_done("ndc_ahn_raster", save_params)
  layers <- ahn_check_layers(layers)

  meta <- ahn_collect_metadata(roi, token, collection, layers, year, limit)

  download_dir <- if (is.null(output_dir) || !nzchar(as.character(output_dir))) tempdir() else output_dir
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  downloaded <- stac_download_items(
    meta, roi, stac_make_headers(token), download_dir, stac_make_file_prefix(file_prefix), subset_crs, overwrite,
    scalefactor, max_cells,
    name_of = function(row) paste0("AHN_", toupper(row$layer), "_", row$year),
    transform = function(r) if (scale) r / ahn_cm_per_m else r
  )

  list(
    rasters = downloaded$rasters,
    stack = stac_stack_if_same_grid(downloaded$rasters),
    files = downloaded$files,
    metadata = meta
  )
}

#' Statistics of the AHN heights in an area of interest
#'
#' Summarise the AHN height rasters (see [ndc_ahn_raster()]) over an area: for each layer and year, the area covered
#' and the minimum, maximum, mean and standard deviation of the heights of the cells inside the area. The rasters are
#' downloaded to compute them (and removed afterwards), so a large area takes time and is requested at a lower
#' resolution (see [ndc_ahn_raster()]).
#'
#' @inheritParams ndc_ahn_raster
#' @returns A tibble with one row per layer and year: `layer`, `year`, `area` (in m\eqn{^2}, of the cells inside the
#'   area), `min`, `max`, `mean`, `sd` (heights, in metres unless `scale = FALSE`) and `n_cells`.
#' @seealso [ndc_ahn_raster()]
#' @export
ndc_ahn_stats <- function(roi,
                          layers = ahn_layers,
                          year = NULL,
                          token = Sys.getenv("NDC_TOKEN"),
                          collection = ahn_collection,
                          limit = 100,
                          subset_crs = ahn_subset_crs,
                          scale = TRUE,
                          scalefactor = NULL,
                          max_cells = ahn_max_cells,
                          params_file = NULL,
                          save_params = NULL) {
  if (!is.null(params_file)) params_apply("ndc_ahn_stats", params_file)
  if (!is.null(save_params)) params_save_when_done("ndc_ahn_stats", save_params)

  dir <- tempfile("ahn_stats_")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  # one layer at a time: the rasters of a large area are big, and only their statistics are kept
  layers <- ahn_check_layers(layers)
  layers <- intersect(layers, ahn_collect_metadata(roi, token, collection, layers, year, limit)$layer)   # the ones with items
  rows <- lapply(layers, function(layer) {
    res <- ndc_ahn_raster(roi, layers = layer, year = year, token = token, collection = collection, output_dir = dir,
                          limit = limit, subset_crs = subset_crs, scale = scale, scalefactor = scalefactor,
                          max_cells = max_cells)
    dplyr::bind_rows(lapply(seq_along(res$rasters), function(i) {
      r <- res$rasters[[i]]
      stats <- terra::global(r, c("min", "max", "mean", "sd", "notNA"), na.rm = TRUE)
      tibble::tibble(layer = res$metadata$layer[i], year = res$metadata$year[i], area = stats$notNA * prod(terra::res(r)),
                     min = stats$min, max = stats$max, mean = stats$mean, sd = stats$sd, n_cells = stats$notNA)
    }))
  })
  dplyr::bind_rows(rows)
}
