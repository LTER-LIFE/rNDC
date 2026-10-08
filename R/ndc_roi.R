#' Spatial region of interest
#'
#' Import and transform spatial region of interest.
#'
#' @param roi character, numeric or sf. Region of interest. Can be either: (i) a character pointing to a path of a file containing a custom geometry, (ii) a numeric vector with four coordinates representing a bounding box, or (iii) an sf object with a (multi)polygon representing a custom region of interest.
#' @param layer character. Layer to read when `roi` is a path to a file with several layers (e.g. a GeoPackage such as the
#'   one of [ndc_sites()]). By default the first layer is used, with a warning if there are more. Ignored otherwise.
#' @returns An sfc object in EPSG:4326 (or `NULL` if `roi` is `NULL`). If the RoI has no CRS, EPSG:4326 is assumed.
#' @export

ndc_roi <- function(roi = NULL, layer = NULL) {

  if (missing(roi) || is.null(roi)) {
    r <- NULL
  } else {
    
    # Check object type
    if (is.numeric(roi)) {
      if (length(roi) == 4) {
        r <- st_as_sfc(st_bbox(c(xmin = roi[[1]], ymin = roi[[2]], xmax = roi[[3]], ymax = roi[[4]]),
                               crs = st_crs(4326)))
      } else {
        stop("Invalid bounding box coordinates.", call. = FALSE)
      }
    } else if (inherits(roi, "character")) {
      if (file.exists(roi)) {
        r <- roi_read_file(roi, layer)
      } else {
        stop("Path to RoI file not found or the file does not exist.", call. = FALSE)
      }
    } else {
      r <- roi
    }
  
    # Extract geometry
    if (inherits(r, c("sf", "sfc"))) {
      r <- st_geometry(r)
    } else if (inherits(r, "sfg")) {
      r <- st_sfc(r)
    } else if (!inherits(r, "bbox")) {
      stop("RoI must be a bounding box, a file path, or an sf/sfc/sfg object.", call. = FALSE)
    }
    if (inherits(r, "bbox")) {
      r <- st_as_sfc(r)
    }

    # Combine multiple geometries into one (the STAC `intersects` filter takes a single geometry)
    if (length(r) > 1) {
      r <- st_union(r)
    }

    # Reprojection (assume EPSG:4326 if no CRS is set)
    if (is.na(st_crs(r))) {
      st_crs(r) <- 4326
    } else if (st_crs(r) != st_crs(4326)) {
      r <- st_transform(r, 4326)
    }
  }

  r
}

# Read a file with the geometry of a RoI. A file can have several layers: read `layer`, or the first one (with a
# warning if there are more).
roi_read_file <- function(path, layer = NULL) {
  layers <- tryCatch(st_layers(path)$name, error = function(e) NULL)  # not every format has layers
  if (is.null(layers)) return(st_read(path, quiet = TRUE))

  if (is.null(layer)) {
    if (length(layers) > 1) {
      warning("The file has ", length(layers), " layers (", paste(layers, collapse = ", "), "): using '", layers[1],
              "'. Use `layer` to choose one.", call. = FALSE)
    }
    layer <- layers[1]
  } else if (length(layer) != 1 || !layer %in% layers) {
    stop("Layer '", paste(layer, collapse = ", "), "' not found in the file. Available layers: ",
         paste(layers, collapse = ", "), ".", call. = FALSE)
  }
  st_read(path, layer = layer, quiet = TRUE)
}

# TODO: Add the possibility to input one of several special character values for pre-defined RoIs
