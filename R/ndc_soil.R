# Soil map (Bodemkaart) raster of the NatureDataCube
# --------------------------------------------------

soil_collection <- "soiltypes"
soil_subset_crs <- 32631L
soil_max_cells <- 5e7

# The items of the soil collection for `roi` and the `year`s asked for
soil_collect_metadata <- function(roi, token, collection = soil_collection, year = NULL, limit = 100) {
  meta <- stac_collect_metadata(roi = roi, token = token, endpoint = ndc_endpoint(), collection = collection,
                                asset_name = "wcs", limit = limit, trange = stac_year_trange(year))
  if (nrow(meta) == 0) stop("No soil map items were found for the selected area.", call. = FALSE)

  keep <- !is.na(meta$href) & nzchar(meta$href)
  if (!is.null(year)) keep <- keep & !is.na(meta$year) & meta$year %in% as.character(year)
  meta <- meta[keep, , drop = FALSE]
  meta <- meta[!duplicated(meta$href), , drop = FALSE]
  if (nrow(meta) == 0) {
    stop("No soil map items matched the year (", paste(year, collapse = ", "), ").", call. = FALSE)
  }
  meta[order(meta$year, meta$id), , drop = FALSE]
}

#' Download the soil map raster for an area of interest
#'
#' Download the Dutch soil map (_Bodemkaart_, 1:50,000) that the NatureDataCube publishes in the `soiltypes` collection: a
#' raster with 5 m cells whose values are the codes of the soil types (see [ndc_soil_legend()] for their names). Only
#' the bounding box of the area is requested from the server, and the raster is clipped to the area. As for
#' [ndc_ahn_raster()], an area with more than `max_cells` cells is requested at a lower resolution.
#'
#' @inheritParams ndc_ahn_raster
#' @param year integer or character. Year(s) of the map (`NULL`, the default, for all the years); see [ndc_soil_years()].
#' @param collection character. STAC collection with the soil map rasters.
#' @returns A list with `rasters` (a named list of clipped `SpatRaster`s, named `SOIL_<year>`), `stack` (a `SpatRaster`
#'   with one layer per raster), `files` (the paths of the downloaded files), `metadata` (a tibble with one row per
#'   item) and `legend` (the tibble of [ndc_soil_legend()], or `NULL` if it could not be read). An error is raised if no
#'   item matches.
#' @details The raster has codes that the legend does not list (16 and above: the legend names the soil types 1 to 15,
#'   and the NatureDataCube does not say what the others are, so they have no name in `legend`).
#' @seealso [ndc_with_progress()] to follow the downloads, or to stop them.
#' @export
ndc_soil_raster <- function(roi,
                            year = NULL,
                            token = Sys.getenv("NDC_TOKEN"),
                            collection = soil_collection,
                            output_dir = tempdir(),
                            overwrite = TRUE,
                            limit = 100,
                            file_prefix = NULL,
                            subset_crs = soil_subset_crs,
                            scalefactor = NULL,
                            max_cells = soil_max_cells,
                            params_file = NULL,
                            save_params = NULL) {
  if (!is.null(params_file)) params_apply("ndc_soil_raster", params_file)
  if (!is.null(save_params)) params_save_when_done("ndc_soil_raster", save_params)

  meta <- soil_collect_metadata(roi, token, collection, year, limit)

  download_dir <- if (is.null(output_dir) || !nzchar(as.character(output_dir))) tempdir() else output_dir
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  downloaded <- stac_download_items(
    meta, roi, stac_make_headers(token), download_dir, stac_make_file_prefix(file_prefix), subset_crs, overwrite,
    scalefactor, max_cells, name_of = function(row) paste0("SOIL_", row$year)
  )

  list(
    rasters = downloaded$rasters,
    stack = stac_stack_if_same_grid(downloaded$rasters),
    files = downloaded$files,
    metadata = meta,
    legend = tryCatch(ndc_soil_legend(token, collection), error = function(e) NULL)
  )
}

#' The soil types of the soil map raster
#'
#' The legend of the soil map (see [ndc_soil_raster()]), as the NatureDataCube publishes it with the item: the code in
#' the raster of each soil type, its name and the colour of the map.
#'
#' @param token character. API token (by default the environment variable `NDC_TOKEN`).
#' @param collection character. STAC collection with the soil map rasters.
#' @returns A tibble with `code` (integer), `label` (the name of the soil type, without the code) and `color` (a hexadecimal
#'   colour).
#' @export
ndc_soil_legend <- function(token = Sys.getenv("NDC_TOKEN"), collection = soil_collection) {
  items <- ndc_get(collections = collection, token = token, limit = 1, mode = "fetch")
  href <- items$features[[1]]$assets$legend$href
  if (is.null(href)) stop("The soil map item has no legend.", call. = FALSE)

  # the legend asset is a picture: the same request in JSON gives the classes
  url <- sub("([?&]format=)[^&]*", "\\1application%2Fjson", href)
  response <- httr::GET(url)
  stop_for_http_error(response, "Legend")
  entries <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"),
                                simplifyVector = FALSE)$Legend[[1]]$rules[[1]]$symbolizers[[1]]$Raster$colormap$entries
  codes <- vapply(entries, function(e) as.integer(e$quantity), 1L)
  labels <- vapply(entries, function(e) trimws(as.character(e$label)), "")
  tibble::tibble(code = codes, label = trimws(sub("^[0-9]+ ?-", "", labels)),
                 color = vapply(entries, function(e) as.character(e$color), ""))
}
