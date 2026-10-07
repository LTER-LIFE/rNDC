#' Monthly NDVI statistics
#'
#' Summarise the NDVI statistics of the NatureDataCube as monthly values. The NDVI collections (`"ndvi-lter"`
#' and `"ndvi-snl"`) hold, for each polygon of the project area, statistics of the satellite observations
#' (`ndvi_mean`, `ndvi_std`), made on irregular dates. This function retrieves all of them for an area and
#' period (all pages of the search, see [ndc_get()]) and averages them per polygon and month.
#'
#' A search for an area returns every polygon that intersects it, neighbours included. When the area is an sf
#' object with the column `ndc_id` (as the polygons of the project collections have), only the polygons with
#' those ids are kept.
#'
#' @param aoi character, numeric or sf. Area of interest, see [ndc_roi()].
#' @param collection character. STAC collection with the NDVI statistics, e.g. `"ndvi-lter"` or `"ndvi-snl"`.
#' @param from,to Date or character. First and last day of the period (the whole of `to` counts). `NA` leaves
#'   a side of the period open.
#' @param ndc_id character. Ids of the polygons to keep. By default the `ndc_id` column of `aoi`, if it has
#'   one (otherwise all the polygons that intersect `aoi` are kept).
#' @param token character. API token.
#' @returns A tibble with one row per polygon and month: `ndc_id`, `month` (`"YYYY-MM"`), `ndvi_mean`,
#'   `ndvi_std` (if the collection has it) and `n_obs` (the number of observations). It has no rows if nothing
#'   matches.
#' @export

get_ndvi_stats <- function(aoi, collection, from = NA, to = NA, ndc_id = NULL,
                           token = Sys.getenv("NDC_TOKEN")) {

  # the whole of the last day counts: a date alone would be midnight at the start of it
  trange <- ndc_trange(c(as_datetime(from), as_datetime(to) + 86399))
  if (is.na(trange)) trange <- NULL

  items <- ndc_get(collection = collection, roi = aoi, trange = trange, token = token, limit = 1000,
                   mode = "sf", all_pages = TRUE)

  if (is.null(ndc_id) && inherits(aoi, "sf") && "ndc_id" %in% names(aoi)) ndc_id <- aoi$ndc_id
  ndc_id <- unique(as.character(ndc_id[!is.na(ndc_id)]))
  if (length(ndc_id) > 0 && "ndc_id" %in% names(items)) {
    items <- items[as.character(items$ndc_id) %in% ndc_id, , drop = FALSE]
  }

  empty <- tibble::tibble(ndc_id = character(), month = character(), ndvi_mean = numeric(),
                          ndvi_std = numeric(), n_obs = integer())
  if (nrow(items) == 0) return(empty)

  for (field in c("observation_date", "ndvi_mean")) {
    if (!field %in% names(items)) {
      stop("Expected field '", field, "' not found in NDVI collection.", call. = FALSE)
    }
  }

  df <- sf::st_drop_geometry(items)
  df$month <- substr(as.character(df$observation_date), 1, 7)
  keys <- intersect(c("ndc_id", "month"), names(df))
  has_std <- "ndvi_std" %in% names(df)
  num <- function(x) suppressWarnings(as.numeric(x))

  groups <- split(seq_len(nrow(df)), interaction(df[keys], drop = TRUE, lex.order = TRUE))
  rows <- lapply(groups, function(i) {
    out <- tibble::as_tibble(df[i[1], keys, drop = FALSE])
    out$ndvi_mean <- mean(num(df$ndvi_mean[i]), na.rm = TRUE)
    if (has_std) out$ndvi_std <- mean(num(df$ndvi_std[i]), na.rm = TRUE)
    out$n_obs <- length(i)
    out
  })
  out <- dplyr::bind_rows(rows)
  out[order(out$month, if ("ndc_id" %in% names(out)) out$ndc_id), , drop = FALSE]
}
