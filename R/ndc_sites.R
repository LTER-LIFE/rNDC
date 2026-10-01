#' Load study sites
#'
#' Load polygons for all study sites available through LTER-LIFE.
#'
#' @param layer character. Pre-defined study site. If empty (or `layer = NULL`), return all available study sites. Otherwise, return the boundaries for the pre-defined study site with that name.
#' @returns Either a character vector with layer names, or an sf object with a (multi)polygon representing the boundaries for a pre-defined study site.
#' @export

ndc_sites <- function(layer = NULL) {

  gpkg <- system.file("extdata/study_sites.gpkg", package = "rNDC")

  layers <- tryCatch(st_layers(gpkg)$name, error = function(e) NULL)

  all_layers <- NULL

  if (is.null(layer) | is.null(layers)) {
    return(layers)
  } else if (layer %in% layers) {
    return(st_read(gpkg, layer = layer, quiet = TRUE))
  } else {
    stop(paste0("Layer '", layer, "' not found. Available layers: ",
         paste(layers, collapse = ", ")), call. = FALSE)
  }
}
