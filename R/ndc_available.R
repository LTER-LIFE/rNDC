#' Which datasets are available for an area and period
#'
#' List the datasets that have data for a region of interest and/or a period: the collections of the
#' NatureDataCube, checked against their items, and the datasets that rNDC retrieves from other services
#' (AgroDataCube, GroenMonitor), for which only the coverage that is known for them can be given (see
#' [ndc_external_datasets()]).
#'
#' The collections of the NatureDataCube are checked with one search over all of them (see [stac_availability()]),
#' and the collections that a full page of results may have hidden are counted one by one. The extents that the
#' collections publish in their metadata are not used to leave collections out: they can be out of date (for
#' example `ndvi-snl` has items that are later than the end of its extent).
#'
#' A collection has data for an area when an item's footprint intersects it. Many rasters are one item for the
#' whole country, so every place within its bounding box, also in the sea, counts. `n_items` is the exact number of
#' items that match; when one search cannot return them all, the collections that have items are counted once more
#' (one request each).
#'
#' @param roi character, numeric or sf. Region of interest, see [ndc_roi()]. `NULL` for no restriction of the area.
#' @param trange Date, POSIXct or character. Period, see [ndc_trange()]: one or two dates, `NA` leaves a side open.
#'   `NULL` for no restriction of the period.
#' @param collections character. IDs of the STAC collections to check. By default all of them.
#' @param external boolean. If `TRUE`, add the datasets from [ndc_external_datasets()] (not the ones that a STAC
#'   collection in the result describes already).
#' @param token character. API token.
#' @returns A tibble with one row per dataset: `id`, `title`, `source` (`"NatureDataCube"`, `"AgroDataCube"` or
#'   `"GroenMonitor"`), `available` (`TRUE`, `FALSE`, or `NA` if the items of a collection could not be counted),
#'   `verified` (`TRUE` when the answer comes from the items of the NatureDataCube, `FALSE` when it comes from the
#'   known coverage of an external dataset), `n_items` (the number of items that match; `NA` for the external
#'   datasets, and for a collection whose items could not be counted), `start` and `end` (the extent of the dataset: for the STAC collections as published in their
#'   metadata, which can be out of date; `NA` means open or unknown), `license` and `doi` (from the metadata of the
#'   collection) and `note`.
#' @seealso [ndc_metadata()], [stac_availability()], [ndc_external_datasets()]
#' @examples
#' \dontrun{
#' ndc_available(roi = c(5.74, 52.05, 5.76, 52.07), trange = c("2024-01-01", "2024-12-31"))
#' ndc_available(trange = "2025-06-01")   # for the whole country
#' }
#' @export
ndc_available <- function(roi = NULL, trange = NULL, collections = NULL, external = TRUE,
                          token = Sys.getenv("NDC_TOKEN")) {
  meta <- ndc_metadata(collections, token = token)
  roi_4326 <- ndc_roi(roi)
  if (!is.null(trange) && is.na(ndc_trange(trange))) stop("Invalid `trange`: provide one or two dates.", call. = FALSE)

  # the NatureDataCube: the items that match, per collection
  found <- if (nrow(meta) > 0) stac_availability(meta$id, roi, token = token, trange = trange) else NULL
  seen <- if (is.null(found)) character(0) else unique(found$collection[!is.na(found$year)])  # items were returned
  # a search that did not return all the items counts only those of its first page: count them again
  if (!is.null(found) && !isTRUE(attr(found, "complete"))) {
    again <- unique(found$collection[!is.na(found$year)])
    exact <- lapply(again, function(id) {
      n <- tryCatch(as.numeric(ndc_count(collection = id, roi = roi, trange = trange, limit = 1, token = token)),
                    error = function(e) NA_real_)
      data.frame(collection = id, year = NA_character_, n_items = n, stringsAsFactors = FALSE)
    })
    found <- rbind(found[!found$collection %in% again, c("collection", "year", "n_items")], do.call(rbind, exact))
  }
  n_items <- vapply(meta$id, function(id) {
    n <- found$n_items[found$collection == id]
    if (length(n) == 0) 0 else if (anyNA(n)) NA_real_ else sum(n)
  }, numeric(1))
  stac <- tibble::tibble(
    id = meta$id, title = meta$title, source = "NatureDataCube",
    available = unname(meta$id %in% seen | n_items > 0), verified = TRUE, n_items = unname(n_items),
    start = meta$start, end = meta$end, license = meta$license, doi = meta$doi, note = NA_character_
  )
  if (!external) return(stac)

  # the other services: only the coverage that is known
  ext <- ndc_external_datasets()
  ext <- ext[is.na(ext$stac_collection) | !ext$stac_collection %in% stac$id, , drop = FALSE]
  in_area <- is.null(roi_4326) || external_in_area(roi_4326)
  ext_rows <- tibble::tibble(
    id = ext$id, title = ext$title, source = ext$source,
    available = in_area & external_in_period(ext$start, ext$end, trange),
    verified = FALSE, n_items = NA_real_,
    start = as.POSIXct(ext$start, tz = "UTC"), end = as.POSIXct(ext$end, tz = "UTC"),
    license = NA_character_, doi = NA_character_,
    note = paste0("Coverage as known, not checked against the service. ", ext$note)
  )
  dplyr::bind_rows(stac, ext_rows)
}

# Does the region (an sfc in EPSG:4326) intersect the Netherlands?
external_in_area <- function(roi) {
  nl <- ndc_netherlands()
  hit <- sf::st_intersects(st_transform(st_geometry(roi), st_crs(nl)), st_geometry(nl), sparse = FALSE)
  any(hit)
}

# For datasets that start at `start` (a Date; `NA`: no time dimension, always) and run to `end` (`NA`: to the
# present): does the period overlap them?
external_in_period <- function(start, end, trange) {
  bounds <- trange_bounds(trange)
  from <- bounds[1]
  to <- bounds[2]
  start <- as.POSIXct(start, tz = "UTC")
  end <- as.POSIXct(end, tz = "UTC")
  ends_after_from <- is.na(from) | is.na(end) | end >= from
  starts_before_to <- is.na(to) | is.na(start) | start <= to
  # the data are updated up to the present: a period that starts in the future has none yet
  not_future <- is.na(from) | !is.na(end) | from <= Sys.time()
  is.na(start) & is.na(end) | (ends_after_from & starts_before_to & not_future)
}
