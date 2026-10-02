# Shared helpers for STAC-based raster retrieval
# ---------------------------------------------

#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

#' STAC raster helpers
#'
#' Building blocks shared by the thematic raster functions ([get_landuse_raster()],
#' [get_nitrogen_raster()]): querying STAC items, downloading WCS subsets and
#' clipping rasters to an area of interest.
#'
#' @param token character. API token.
#' @param aoi sf, sfc or numeric. Area of interest, normalised with [ndc_roi()].
#' @param endpoint,collection character. STAC endpoint and collection ID.
#' @param asset_name character. Name of the STAC asset holding the raster URL.
#' @param limit integer. Maximum number of STAC items per page.
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
#'   `stac_download_one()` the path of the downloaded file;
#'   `stac_clip_raster_to_aoi()` a clipped `SpatRaster`;
#'   `stac_build_stack()` a `SpatRaster` (or `NULL`).
#' @name stac_helpers
#' @export
stac_make_headers <- function(token) {
  token <- as.character(token)
  token <- trimws(token)
  if (!nzchar(token)) {
    stop("Nature Data Cube token is missing. Set the token explicitly before retrieval.", call. = FALSE)
  }
  httr::add_headers("Authorization" = paste0("Bearer ", token),
                    "token" = token, "Accept" = "application/json")
}

#' @rdname stac_helpers
#' @export
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
#' @export
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
stac_collect_metadata <- function(aoi, token, endpoint, collection, asset_name = "wcs", limit = 100) {
  aoi_4326 <- ndc_roi(aoi)
  headers <- stac_make_headers(token)

  items <- rstac::stac(endpoint) |>
    rstac::stac_search(
      collections = collection,
      intersects = aoi_4326,
      limit = limit
    ) |>
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
stac_bbox_in_crs <- function(aoi, target_crs = 32631L) {
  aoi_4326 <- ndc_roi(aoi)
  aoi_sf <- sf::st_as_sf(aoi_4326)
  aoi_proj <- sf::st_transform(aoi_sf, target_crs)
  sf::st_bbox(aoi_proj)
}

#' @rdname stac_helpers
#' @export
stac_wcs_subset_suffix <- function(aoi, target_crs = 32631L, x_name = "E", y_name = "N") {
  bbox <- stac_bbox_in_crs(aoi, target_crs = target_crs)
  fmt <- function(x) format(as.numeric(x), scientific = FALSE, trim = TRUE, digits = 12)

  paste0(
    "&subset=", x_name, "(", fmt(bbox[["xmin"]]), ",", fmt(bbox[["xmax"]]), ")",
    "&subset=", y_name, "(", fmt(bbox[["ymin"]]), ",", fmt(bbox[["ymax"]]), ")"
  )
}

#' @rdname stac_helpers
#' @export
stac_download_one <- function(href, outfile, headers, overwrite = TRUE, retries = 3L, min_file_size = NULL) {
  if (file.exists(outfile) && !overwrite) return(outfile)

  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)

  last_status <- NA_integer_
  last_error <- NULL

  for (attempt in seq_len(max(1L, as.integer(retries)))) {
    res <- tryCatch(
      httr::RETRY(
        "GET",
        href,
        headers,
        httr::write_disk(outfile, overwrite = overwrite),
        times = 1,
        pause_base = 0.5,
        pause_cap = 2,
        quiet = TRUE
      ),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (inherits(res, "response")) {
      last_status <- httr::status_code(res)

      if (!httr::http_error(res)) {
        if (!is.null(min_file_size) && file.exists(outfile)) {
          file_size <- suppressWarnings(file.info(outfile)$size)
          if (is.na(file_size) || file_size < as.numeric(min_file_size)) {
            if (file.exists(outfile)) unlink(outfile)
            last_error <- simpleError(
              paste0("Downloaded file is too small (", file_size, " bytes) for ", href)
            )
          } else {
            return(outfile)
          }
        } else {
          return(outfile)
        }
      } else if (file.exists(outfile)) {
        unlink(outfile)
      }
    }

    if (attempt < max(1L, as.integer(retries))) {
      Sys.sleep(min(2 ^ (attempt - 1L), 4))
      next
    }
  }

  if (!is.null(last_error)) {
    stop(last_error)
  }

  if (!is.na(last_status)) {
    stop(
      paste0("Failed to download raster: HTTP ", last_status, " for ", href),
      call. = FALSE
    )
  }

  stop(
    paste0("Failed to download raster for ", href),
    call. = FALSE
  )
}

#' @rdname stac_helpers
#' @export
stac_clip_raster_to_aoi <- function(r, aoi) {
  if (is.null(r)) return(NULL)

  aoi_vect <- terra::vect(ndc_roi(aoi))
  r_crs <- terra::crs(r)
  if (is.na(r_crs) || !nzchar(r_crs)) {
    stop("Downloaded raster has no CRS, so it cannot be clipped safely.", call. = FALSE)
  }

  aoi_proj <- terra::project(aoi_vect, r_crs)
  r_clip <- terra::crop(r, aoi_proj)
  r_clip <- terra::mask(r_clip, aoi_proj)
  r_clip
}

#' @rdname stac_helpers
#' @export
stac_make_file_prefix <- function(x) {
  if (is.null(x) || !nzchar(as.character(x))) return("")
  paste0(gsub("[^A-Za-z0-9_\\-]+", "_", as.character(x)), "_")
}

#' @rdname stac_helpers
#' @export
stac_build_stack <- function(clipped) {
  if (length(clipped) == 0) return(NULL)
  if (length(clipped) == 1) clipped[[1]] else terra::rast(clipped)
}
