#' NatureDataCube STAC API endpoint
#'
#' URL of the NatureDataCube STAC API used by all `ndc_*` functions and by the
#' thematic raster functions. It defaults to the NatureDataCube test server and
#' can be overridden with the `rNDC.endpoint` option, e.g.
#' `options(rNDC.endpoint = "https://example.org/api/")`.
#'
#' @returns character. The endpoint URL.
#' @export

ndc_endpoint <- function() {
  getOption("rNDC.endpoint", "https://ndc-test.containers.wur.nl/api/")
}
