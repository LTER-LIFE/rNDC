#' Land use and nitrogen rasters: defaults and available years
#'
#' The collection and default year of the land use rasters (see [get_landuse_raster()]), the layers of the
#' nitrogen rasters (see [get_nitrogen_raster()]), and the years for which they exist in the NatureDataCube.
#'
#' @param token character. API token.
#' @returns `ndc_landuse_collection()`: the STAC collection of the land use rasters; `ndc_landuse_default_year()`:
#'   the default year (integer); `ndc_nitrogen_layers()`: the nitrogen layers (also their STAC collections);
#'   `ndc_landuse_years()` and `ndc_nitrogen_years()`: character vector with the years that have items,
#'   read from the STAC API (an error if the API cannot be reached).
#' @name ndc_defaults
NULL

#' @rdname ndc_defaults
#' @export
ndc_landuse_collection <- function() landuse_collection

#' @rdname ndc_defaults
#' @export
ndc_landuse_default_year <- function() landuse_default_year

#' @rdname ndc_defaults
#' @export
ndc_nitrogen_layers <- function() nitrogen_layer_choices

#' @rdname ndc_defaults
#' @export
ndc_landuse_years <- function(token = Sys.getenv("NDC_TOKEN")) {
  raster_years(landuse_collection, token)
}

#' @rdname ndc_defaults
#' @export
ndc_nitrogen_years <- function(token = Sys.getenv("NDC_TOKEN")) {
  raster_years(nitrogen_layer_choices, token)
}

# The years of the items of the STAC collection(s), sorted
raster_years <- function(collections, token) {
  years <- unlist(lapply(collections, function(collection) {
    items <- ndc_get(collection = collection, token = token, limit = 100, mode = "fetch")
    if (length(items$features) == 0) return(character(0))
    dplyr::bind_rows(lapply(items$features, stac_feature_meta))$year
  }))
  sort(unique(as.character(years[!is.na(years)])))
}
