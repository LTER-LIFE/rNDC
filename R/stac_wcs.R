# STAC helpers for the WCS assets of the items: the area of a request as a WCS subset, and the download of a file (with
# retries). See `stac_make_headers()` for the documentation of the helpers.

#' @rdname stac_helpers
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
