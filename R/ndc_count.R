#' Count datasets within spatiotemporal contraints
#'
#' Obtain a number of items available in NatureDataCube datasets (optionally constrained by query parameters).
#'
#' @param s doc_item. Search object resulting from `ndc_search` or a standard STAC query.
#' @param collection character. Collection ID.
#' @param roi character, numeric or sf. Region of interest. Can be either: (i) a character value for one of the projects from the Data Registry, (ii) a numeric vector with coordinates representing a bounding box, or (iii) an sf object with a (multi)polygon representing a custom region of interest.
#' @param trange character. Temporal range.
#' @param limit integer. Maximum number of STAC Items to return.
#' @param token character. API token.
#' @returns integer. Number of STAC Items matched by the search query.
#' @export

# Return the number of STAC Items in a STAC Collection
ndc_count <- function(s = NULL, collection = NULL, roi = NULL, trange = NULL,
                      limit = 100, token = Sys.getenv("NDC_TOKEN")) {
  
  headers <- add_headers("Authorization" = paste0("Bearer ", token))
  endpoint <- stac("https://ndc-test.containers.wur.nl/api/")

  if (missing(s)) {
    s <- ndc_get(collection = collection, roi = ndc_roi(roi), trange = trange,
                 limit = limit, token = token, mode = "items", progress = FALSE)
  }

  items_matched(s)
}
