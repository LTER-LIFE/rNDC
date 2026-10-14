#' Which collections have items for an area
#'
#' Find out which of some STAC collections have items that intersect a region of interest, and for which years, with
#' one search over all of them (instead of counting the items of each collection with [ndc_count()]). It is meant
#' for questions such as "is there land use or nitrogen data for this area?".
#'
#' The search returns at most `limit` items. If there are more, a collection can be missing from the first page
#' without having no items: the collections that did not show are then asked one by one (with [ndc_count()]), and get
#' a row without a year.
#'
#' @param collections character. IDs of the collections.
#' @param roi character, numeric or sf. Region of interest, see [ndc_roi()]. `NULL` for no restriction of the area.
#' @param token character. API token.
#' @param limit integer. Maximum number of items that the search returns.
#' @param trange Date, POSIXct or character. Optional period, see [ndc_trange()]: only the items in it count.
#' @returns A data frame with the columns `collection`, `year` (of the datetime of the items; `NA` for a collection that
#'   was asked separately) and `n_items` (the number of items; for a collection that was asked separately the number
#'   matched, or `NA` if that request failed). Collections without items have no row. An error if the search fails.
#'   The attribute `complete` tells whether the search returned all the items that match: if not (`FALSE`), the
#'   `n_items` of the rows with a `year` are the items on the first page only, and so a lower bound.
#' @examples
#' \dontrun{
#' stac_availability(c("lgn", "ntot", "nox", "nh3"), roi = c(5.74, 52.05, 5.76, 52.07))
#' }
#' @export
stac_availability <- function(collections, roi = NULL, token = Sys.getenv("NDC_TOKEN"), limit = 500L, trange = NULL) {
  collections <- unique(as.character(collections))
  items <- ndc_get(collections = collections, roi = roi, trange = trange, limit = limit, token = token, mode = "items")

  text <- function(x) if (is.null(x)) NA_character_ else as.character(x)[1]
  features <- items$features
  found <- data.frame(collection = character(0), year = character(0), n_items = numeric(0), stringsAsFactors = FALSE)
  if (length(features) > 0) {
    collection <- vapply(features, function(f) text(f$collection), "")
    year <- substr(vapply(features, function(f) text(f$properties$datetime), ""), 1, 4)
    counts <- as.data.frame(table(collection = collection, year = year), stringsAsFactors = FALSE)
    counts <- counts[counts$Freq > 0, , drop = FALSE]
    found <- data.frame(collection = counts$collection, year = counts$year, n_items = as.numeric(counts$Freq),
                        stringsAsFactors = FALSE)
  }

  matched <- items$numberMatched
  if (!is.null(matched) && matched > length(features)) {
    for (missing in setdiff(collections, found$collection)) {
      n <- tryCatch(as.numeric(ndc_count(collection = missing, roi = roi, trange = trange, limit = 1, token = token)), error = function(e) NA_real_)
      if (is.na(n) || n > 0) {
        found <- rbind(found, data.frame(collection = missing, year = NA_character_, n_items = n,
                                         stringsAsFactors = FALSE))
      }
    }
  }
  found <- found[order(found$collection, found$year), , drop = FALSE]
  rownames(found) <- NULL
  attr(found, "complete") <- is.null(matched) || matched <= length(features)
  found
}
