#' Search and download data from NatureDataCube
#'
#' Search (and optionally download) data from NatureDataCube, through a custom STAC query.
#'
#' @import rstac
#' @import httr
#' @param collection character. Collection ID.
#' @param roi character, numeric or sf. Region of interest. Can be either: (i) a character value for one of the projects from the Data Registry, (ii) a numeric vector with coordinates representing a bounding box, or (iii) an sf object with a (multi)polygon representing a custom region of interest.
#' @param trange character. Temporal range.
#' @param limit integer. Maximum number of STAC Items to return.
#' @param token character. API token.
#' @param mode
#' @param progress boolean. If `TRUE`, show progress bar.
#' @returns A request response list.
#' @export

library(rstac)
library(httr)
library(tibble)
library(sf)

ndc_get <- function(collection, roi = NULL, trange = NULL, asset_names = NULL,
                    limit = 100, token = Sys.getenv("NDC_TOKEN"), mode = "items",
                    output_dir = tempdir(), overwrite = TRUE, progress = FALSE) {
  
  headers <- add_headers("Authorization" = paste0("Bearer ", token))
  endpoint <- stac("https://ndc-test.containers.wur.nl/api/")
  
  ## Search items
  query <- stac_search(endpoint, collections = collection, limit = limit)
  if (!missing(roi) || !is.null(roi)) {
    query <- stac_search(query, intersects = ndc_roi(roi), limit = limit)
  }
  if (!missing(trange)) {
    query <- stac_search(query, datetime = trange, limit = limit)
  }
  items <- post_request(query, headers)
  
  ## Return (and optionally download) searched items
  if (mode == "fetch") {
    items_fetch(items, progress = progress, headers)
  } else if (mode == "tibble") {
    items_as_tibble(items)
  } else if (mode == "sf") {
    items_as_sf(items)
  } else if (mode == "sfc") {
    items_as_sfc(items)
  } else if (mode == "download") {
    assets_download(items, asset_names = asset_names,
                    output_dir = output_dir, overwrite = overwrite)
  } else if (mode == "download_wcs") {
    assets_download_wcs(items, asset_names = asset_names,
                        output_dir = output_dir , overwrite = overwrite)
  } else {
    items
  }
}
