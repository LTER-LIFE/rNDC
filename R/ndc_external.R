#' Datasets that are not in the NatureDataCube
#'
#' The datasets that rNDC retrieves from other services than the NatureDataCube STAC API (the AgroDataCube REST API
#' and the GroenMonitor WCS server), with the coverage that is known for them: the period they start in, and that
#' they cover the Netherlands. These services publish no catalogue that can be asked, so this is the information
#' of the documentation of the providers and of what was found when the package was written (it is checked
#' against the live services by the tests of the package that run on request): it is not read from the
#' services when it is used. [ndc_available()] uses it for the datasets that are not in the STAC catalogue.
#'
#' @returns A tibble with one row per dataset:
#'   \itemize{
#'     \item `id`, `title`, `source` (the service) and `url` (its web site);
#'     \item `service`: how the package asks for it (the `option` of [adc_url()], or `"WCS"`);
#'     \item `start`: the first date with data (`NA`: no time dimension or no known start). There is no end: the
#'       datasets are updated, `end` is `NA` for "to the present";
#'     \item `end`: `NA` (see above);
#'     \item `region`: `"Netherlands"`;
#'     \item `stac_collection`: the STAC collection that describes the same data (or `NA`), so that
#'       [ndc_available()] does not list it twice;
#'     \item `note`: what to know about the coverage.
#'   }
#' @seealso [ndc_available()], [ndc_netherlands()]
#' @examples
#' ndc_external_datasets()[, c("id", "source", "start")]
#' @export
ndc_external_datasets <- function() {
  adc <- "https://agrodatacube.wur.nl/"
  tibble::tibble(
    id = c("weather", "soil-map", "ahn", "agricultural-fields", "groenmonitor-ndvi"),
    title = c("Weather (KNMI daily observations)", "Soil map (soil types)", "Elevation (AHN)",
              "Agricultural fields (crop parcels)", "NDVI rasters (GroenMonitor)"),
    source = c("AgroDataCube", "AgroDataCube", "AgroDataCube", "AgroDataCube", "GroenMonitor"),
    url = c(adc, adc, adc, adc, "https://www.groenmonitor.nl/"),
    service = c("Meteo_data", "Soiltypes", "AHN", "Fields", "WCS"),
    start = as.Date(c("1970-01-01", NA, NA, "2009-01-01", "2017-05-26")),
    end = as.Date(rep(NA, 5)),
    region = "Netherlands",
    stac_collection = c(NA, NA, NA, NA, "ndvi"),
    note = c("Observations of the KNMI stations. The station closest to an area may start later than 1970.",
             "Soil types of the Netherlands. No time dimension.",
             "Elevation of the Netherlands. No time dimension.",
             "Field geometries and crop types per year; 2008 and earlier have none.",
             "Daily NDVI of the Netherlands; the same data as the STAC collection 'ndvi'.")
  )
}

#' Outline of the Netherlands
#'
#' The outline of the Netherlands that is bundled with the package (`inst/extdata/nl.gpkg`). It is used to tell
#' whether an area is in the country that the AgroDataCube and GroenMonitor datasets cover.
#'
#' @returns An sf object with the outline, in the Dutch grid (EPSG:28992).
#' @seealso [ndc_external_datasets()], [ndc_available()]
#' @examples
#' sf::st_bbox(ndc_netherlands())
#' @export
ndc_netherlands <- function() {
  file <- system.file("extdata", "nl.gpkg", package = "rNDC")
  if (!nzchar(file)) stop("The outline of the Netherlands (nl.gpkg) is not part of the installation.", call. = FALSE)
  st_read(file, quiet = TRUE)
}
