#' Download STAC assets originating in WCS servers
#'
#' Wrapper to download data from STAC endpoints that originated in WCS servers.
#'
#' @param items doc_items. Items list resulting from `ndc_get`.
#' @param asset_names character. Names of the STAC assets to download.
#' @param output_dir character. Output directory path.
#' @param output_ext character. Output file extension.
#' @param overwrite boolean. If `TRUE`, overwrite file.
#' @returns The `httr` response (or list of responses), or `NULL` if there is nothing to download.
#' @export

assets_download_wcs <- function(items, asset_names = "wcs",
                                output_dir = tempdir(), output_ext = ".tif",
                                overwrite = TRUE) {
  
  # Get asset URLs, item IDs, and output paths
  wcs_urls <- assets_url(items, asset_names = asset_names)
  item_ids <- vapply(items$features, function(f) f$id, character(1))
  dest <- file.path(output_dir, paste0(item_ids, output_ext))

  # Download files, checking each response (an error body is not kept as a file)
  if (length(wcs_urls) == 0 || all(is.na(wcs_urls))) return(NULL)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  responses <- vector("list", length(wcs_urls))
  for (i in seq_along(wcs_urls)) {
    response <- GET(wcs_urls[i], write_disk(dest[i], overwrite = overwrite))
    if (httr::http_error(response)) {
      unlink(dest[i])
      stop_for_http_error(response, "NatureDataCube")
    }
    responses[[i]] <- response
  }
  if (length(responses) == 1) responses[[1]] else responses
}
