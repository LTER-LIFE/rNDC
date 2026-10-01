#' Get data from _GroenMonitor_
#'
#' Wrapper to get data from the _GroenMonitor_ WCS server.
#'
#' @param url character. Request URL.
#' @param option character. Determines the type of request.
#' @param params vector or list. List of named parameters.
#' @param out_path character. Output path.
#' @param overwrite boolean. If `TRUE`, overwrite file.
#' @returns A request response list.
#' @export

gm_get <- function(url, option = "NDVI", params, out_path = tempfile(), overwrite = TRUE) {

  # Compose request URL
  if (missing(url) && !missing(option) && !missing(params)) {
    request_url <- gm_url(option = option, params = params)
  } else {
    request_url <- url
  }

  # Download file
  response <- GET(request_url, write_disk(out_path, overwrite = overwrite))

  return(response)
}
