#' Search and download data from NatureDataCube
#'
#' Search (and optionally download) data from NatureDataCube, through a custom STAC query.
#'
#' @param collection character. Collection ID.
#' @param roi character, numeric or sf. Region of interest, see [ndc_roi()]. Can be either: (i) a path to a file with a custom geometry, (ii) a numeric vector with coordinates representing a bounding box, or (iii) an sf object with a (multi)polygon representing a custom region of interest.
#' @param trange Date, POSIXct or character. One or two dates; `NA` leaves a side of the range open. Converted with `ndc_trange()`.
#' @param limit integer. Maximum number of STAC Items to return. Note that for `mode = "items"` only the first page of at most `limit` items is returned; compare with the number matched.
#' @param token character. API token.
#' @param asset_names character. Names of the STAC assets to download (for the `download` and `download_wcs` modes).
#' @param mode character. Output mode, one of `items` (default; first page of results), `fetch` (all pages), `tibble`, `sf`, `sfc`, `download` or `download_wcs`. Without `all_pages`, the last five only use the first page of results.
#' @param output_dir character. Output directory path.
#' @param overwrite boolean. If `TRUE`, overwrite file.
#' @param progress boolean. If `TRUE`, show progress bar.
#' @param all_pages boolean. If `TRUE`, the modes that convert or download only the first page (`tibble`, `sf`, `sfc`, `download` and `download_wcs`) first retrieve all the pages of the search, as `mode = "fetch"` does (`limit` is then the size of each page). Has no effect on the modes `items` (first page) and `fetch` (all pages already).
#' @returns Depends on `mode`: a STAC item collection (`items`, `fetch`), a tibble, an sf or sfc object (empty if nothing matches), or the result of downloading the assets.
#' @export

ndc_get <- function(collection, roi = NULL, trange = NULL, asset_names = NULL,
                    limit = 100, token = Sys.getenv("NDC_TOKEN"), mode = "items",
                    output_dir = tempdir(), overwrite = TRUE, progress = FALSE, all_pages = FALSE) {
  
  mode <- match.arg(mode, c("items", "fetch", "tibble", "sf", "sfc", "download", "download_wcs"))

  headers <- add_headers("Authorization" = paste0("Bearer ", token))
  endpoint <- stac(ndc_endpoint())
  
  ## Search items
  query <- stac_search(endpoint, collections = collection, limit = limit)
  if (!is.null(roi)) {
    query <- stac_search(query, intersects = ndc_roi(roi), limit = limit)
  }
  if (!is.null(trange)) {
    datetime <- ndc_trange(trange)
    if (is.na(datetime)) {
      stop("Invalid `trange`: provide one or two dates.", call. = FALSE)
    }
    query <- stac_search(query, datetime = datetime, limit = limit)
  }
  items <- post_request(query, headers)
  if (all_pages && mode %in% c("tibble", "sf", "sfc", "download", "download_wcs")) {
    items <- items_fetch(items, progress = progress, headers)
  }

  # Only the first page is converted/downloaded in these modes: warn if it is incomplete
  n_matched <- items$numberMatched
  n_returned <- length(items$features)
  if (mode %in% c("tibble", "sf", "sfc", "download", "download_wcs") &&
      !is.null(n_matched) && n_returned < n_matched) {
    warning(sprintf(paste0("Only %d of %d matched items were returned. ",
                           "Increase `limit`, or use `mode = \"fetch\"` to retrieve all pages."),
                    n_returned, n_matched), call. = FALSE)
  }

  ## Return (and optionally download) searched items
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
