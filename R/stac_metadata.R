# STAC helpers: the headers of a request, and the metadata of the items of a search. All the `stac_*` helpers are documented
# on one page (`?stac_helpers`), here with `stac_make_headers()`.

#' STAC raster helpers
#'
#' Building blocks shared by the thematic raster functions ([ndc_landuse_raster()],
#' [ndc_nitrogen_raster()]): querying STAC items, downloading WCS subsets and
#' clipping rasters to a region of interest.
#'
#' @param token character. API token.
#' @param roi sf, sfc or numeric. Region of interest, normalised with [ndc_roi()].
#' @param aoi Deprecated: use `roi`.
#' @param endpoint,collection character. STAC endpoint and collection ID.
#' @param asset_name character. Name of the STAC asset holding the raster URL.
#' @param limit integer. Maximum number of STAC items per page.
#' @param trange Date, POSIXct or character. Optional time range of the search, see [ndc_trange()].
#' @param year integer or character. Year(s) to build a time range from; a range from the first day of the
#'   earliest to the last day of the latest.
#' @param feat list. A STAC feature (item).
#' @param x character. Keywords, or a text to build a file prefix from.
#' @param target_crs integer. EPSG code of the CRS used for WCS subsets.
#' @param x_name,y_name character. Axis labels used in the WCS `subset` parameters.
#' @param href character. URL to download.
#' @param outfile character. Path of the downloaded file.
#' @param headers Request headers, as returned by `stac_make_headers()`.
#' @param overwrite logical. If `TRUE`, overwrite an existing file.
#' @param retries integer. Number of download attempts.
#' @param min_file_size numeric. Minimum acceptable file size in bytes (`NULL` to skip the check).
#' @param r SpatRaster. Raster to clip.
#' @param clipped list of SpatRaster. Rasters to combine.
#' @returns Depends on the function: `stac_make_headers()` returns request headers;
#'   `stac_collect_metadata()` a tibble with one row per STAC item;
#'   `stac_year_trange()` a time range (see [ndc_trange()]), or `NULL` if there is no valid year;
#'   `stac_download_one()` the path of the downloaded file;
#'   `stac_clip_raster_to_aoi()` a clipped `SpatRaster`;
#'   `stac_build_stack()` a `SpatRaster` (or `NULL`).
#' @name stac_helpers
#' @export
stac_make_headers <- function(token) {
  check_token(token, "NatureDataCube", "NDC_TOKEN")
  token <- trimws(as.character(token))
  httr::add_headers("Authorization" = paste0("Bearer ", token),
                    "token" = token, "Accept" = "application/json")
}

#' @rdname stac_helpers
stac_keywords_to_vec <- function(x) {
  if (is.null(x)) return(character(0))
  x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- trimws(x)
  x <- x[nzchar(x)]
  if (length(x) == 1L && grepl(",", x, fixed = TRUE)) {
    x <- trimws(unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE))
    x <- x[nzchar(x)]
  }
  unique(tolower(x))
}

#' @rdname stac_helpers
stac_feature_meta <- function(feat, asset_name = "wcs") {
  props <- feat$properties %||% list()
  assets <- feat$assets %||% list()
  href <- NA_character_
  if (!is.null(assets[[asset_name]]) && !is.null(assets[[asset_name]]$href)) {
    href <- as.character(assets[[asset_name]]$href)
  }

  obs <- props$`ndc:observation_date` %||% props$datetime %||% NA_character_
  layer <- props$`ndc:layer_type` %||% props$`geoserver:layer_name` %||% NA_character_
  title <- props$title %||% NA_character_
  keywords <- stac_keywords_to_vec(props$keywords)

  tibble::tibble(
    id = as.character(feat$id %||% NA_character_),
    title = as.character(title),
    layer = as.character(layer),
    observation_date = as.character(obs),
    year = ifelse(!is.na(obs) & nzchar(as.character(obs)), substr(as.character(obs), 1, 4), NA_character_),
    keywords = list(keywords),
    href = href
  )
}

#' @rdname stac_helpers
#' @export
stac_collect_metadata <- function(roi, token, endpoint, collection, asset_name = "wcs", limit = 100,
                                  trange = NULL, aoi = NULL) {
  if (!is.null(aoi)) roi <- deprecated_argument(aoi, "aoi", "roi", "stac_collect_metadata")
  roi_4326 <- ndc_roi(roi)
  headers <- stac_make_headers(token)

  query <- rstac::stac(endpoint) |>
    rstac::stac_search(
      collections = collection,
      intersects = roi_4326,
      limit = limit
    )
  if (!is.null(trange)) {
    datetime <- ndc_trange(trange)
    if (is.na(datetime)) stop("Invalid `trange`: provide one or two dates.", call. = FALSE)
    query <- rstac::stac_search(query, datetime = datetime, limit = limit)
  }

  items <- query |>
    rstac::post_request(headers) |>
    rstac::items_fetch(progress = FALSE)

  feats <- items$features
  if (is.null(feats) || length(feats) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(lapply(feats, stac_feature_meta, asset_name = asset_name))
}

#' @rdname stac_helpers
#' @export
stac_year_trange <- function(year) {
  year <- suppressWarnings(as.integer(as.character(unlist(year))))
  year <- year[!is.na(year)]
  if (length(year) == 0) return(NULL)
  c(sprintf("%04d-01-01T00:00:00Z", min(year)), sprintf("%04d-12-31T23:59:59Z", max(year)))
}
