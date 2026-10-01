#' Search and download data from NatureDataCube
#'
#' Search (and optionally download) data from NatureDataCube, through a custom STAC query.
#'
#' @param collection character. Collection ID.
#' @param roi character, numeric or sf. Region of interest. Can be either: (i) a character value for one of the projects from the Data Registry, (ii) a numeric vector with coordinates representing a bounding box, or (iii) an sf object with a (multi)polygon representing a custom region of interest.
#' @param trange character. Temporal range.
#' @param limit integer. Maximum number of STAC Items to return.
#' @param token character. API token.
#' @param mode character. Output mode. Can be either: (i) `items` (default), (ii) `tibble`, (iii) `sf`, (iv) `sfc`, (v) `download`.
#' @param output_dir character. Output directory path.
#' @param overwrite boolean. If `TRUE`, overwrite file.
#' @param progress boolean. If `TRUE`, show progress bar.
#' @returns A request response list.
#' @export

ndc_get <- function(collection, roi = NULL, trange = NULL, asset_names = NULL,
                    limit = 100, token = Sys.getenv("NDC_TOKEN"), mode = "items",
                    output_dir = tempdir(), overwrite = TRUE, progress = FALSE) {
  
  headers <- add_headers("Authorization" = paste0("Bearer ", token))
  endpoint <- stac("https://ndc-test.containers.wur.nl/api/")
  
  ## Search items
  query <- stac_search(endpoint, collections = collection, limit = limit)
  if (!is.null(roi)) {
    query <- stac_search(query, intersects = ndc_roi(roi), limit = limit)
  }
  if (!is.null(trange)) {
    query <- stac_search(query, datetime = ndc_trange(trange), limit = limit)
  }
  items <- post_request(query, headers)
  
  ## Return (and optionally download) searched items
  mode <- match.arg(mode, c("items", "fetch", "tibble", "sf", "sfc", "download", "download_wcs"))
  switch(mode,
         items = items,
         fetch = items_fetch(items, progress = progress, headers),
         tibble = items_as_tibble(items),
         sf = items_as_sf(items),
         sfc = items_as_sfc(items),
         download = assets_download(items, asset_names = asset_names,
                                    output_dir = output_dir, overwrite = overwrite),
         download_wcs = assets_download_wcs(items, asset_names = asset_names,
                                            output_dir = output_dir, overwrite = overwrite))
}
