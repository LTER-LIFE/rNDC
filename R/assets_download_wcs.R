#' Download STAC assets originating in WCS servers
#'
#' Wrapper to download data from STAC endpoints that originated in WCS servers.
#'
#' @param items doc_item. Items list resulting from `ndc_get`.
#' @param output_dir character. Output directory path.
#' @param output_ext character. Output file extension.
#' @param overwrite boolean. If `TRUE`, overwrite file.
#' @returns A request response list.
#' @export

assets_download_wcs <- function(items, asset_names = "wcs",
                                output_dir = tempdir(), output_ext = ".tif",
                                overwrite = TRUE) {
  
  # Get asset URLs, item IDs, and output paths
  wcs_urls <- assets_url(items, asset_names = asset_names)
  wcs_url <- wcs_urls[1]
  item_ids <- vapply(items$features, function(f) f$id, character(1))
  dest <- file.path(output_dir, paste0(item_ids, output_ext))

  # Download files
  if (!is.na(wcs_url) && length(wcs_url) > 0) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    response <- GET(wcs_url, write_disk(dest[1], overwrite = overwrite))
    if (length(wcs_urls) > 1) {
      for (i in 2:length(wcs_urls)) {
        new_response <- GET(wcs_urls[i], write_disk(dest[i], overwrite = overwrite))
        response <- list(response, new_response)
      }
    }
    stop_for_status(response)
    return(response)
  }
}
