#' rNDC: R interface to LTER-LIFE's NatureDataCube
#'
#' Functions and wrappers to find and get data from the NatureDataCube STAC API,
#' and from the related AgroDataCube and GroenMonitor services.
#'
#' @importFrom geojsonsf geojson_sf
#' @importFrom httr GET VERB add_headers content stop_for_status write_disk
#' @importFrom jsonlite toJSON
#' @importFrom lubridate as_datetime ceiling_date year
#' @importFrom rstac assets_download assets_url collections get_request items_as_sf items_as_sfc items_as_tibble items_fetch items_matched post_request stac stac_search
#' @importFrom sf `st_crs<-` st_as_sfc st_bbox st_centroid st_crs st_distance st_geometry st_layers st_read st_sfc st_transform st_union
#' @importFrom stats na.omit
#' @importFrom terra app compareGeom extend rast resample values
#' @importFrom utils URLencode download.file
#' @keywords internal
"_PACKAGE"
