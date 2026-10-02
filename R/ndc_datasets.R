#' List available datasets
#'
#' List all datasets (optionally constrained by query parameters) in NatureDataCube.
#'
#' @param roi character, numeric or sf. Region of interest, see [ndc_roi()]. Can be either: (i) a path to a file with a custom geometry, (ii) a numeric vector with coordinates representing a bounding box, or (iii) an sf object with a (multi)polygon representing a custom region of interest.
#' @param trange character. Temporal range.
#' @param token character. API token.
#' @param total boolean. If `TRUE`, return total number of STAC Items for each STAC Collection.
#' @param matched boolean. If `TRUE`, return matched number of STAC Items for each STAC Collection.
#' @returns character or data frame. List of STAC Collections and (optionally) the total and/or matched STAC Items count.
#' @export

ndc_datasets <- function(roi = NULL, trange = NULL,
                         token = Sys.getenv("NDC_TOKEN"),
                         total = FALSE, matched = FALSE) {
  
  headers <- add_headers("Authorization" = paste0("Bearer ", token))
  endpoint <- stac(ndc_endpoint())
  
  collections_list <- collections(endpoint) |>
    get_request(headers)

  collections_ids <- unlist(lapply(collections_list$collections, FUN = \(x) x$id))

  if (any(!missing(roi), !missing(trange))) matched <- TRUE

  if (any(total, matched)) {
    colls <- data.frame(dataset_id = collections_ids)
    if (total) {
      f <- function(x) ndc_count(collection = x, roi = NULL, trange = NULL, token = token)
      colls$n_total <- unlist(lapply(collections_ids, FUN = f))
    }
    if (matched) {
      f <- function(x) ndc_count(collection = x, roi = ndc_roi(roi), trange = trange, token = token)
      colls$n_matched <- unlist(lapply(collections_ids, FUN = f))
    }
  } else {
    colls <- collections_ids
  }

  colls
}
