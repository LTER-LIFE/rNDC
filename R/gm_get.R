#' Get data from _GroenMonitor_
#'
#' Wrapper to get data from the _GroenMonitor_ WCS server.
#'
#' @param url character. Request URL.
#' @param option character. Determines the type of request.
#' @param params vector or list. List of named parameters.
#' @param out_path character. Output path.
#' @param overwrite boolean. If `TRUE`, overwrite file.
#' @returns The `httr` response. The file is written to `out_path`; an error is raised (and the file removed) if the request fails, e.g. when no coverage exists for the requested date.
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
  if (httr::http_error(response)) {
    on.exit(unlink(out_path), add = TRUE)  # remove the saved error body
    stop_for_http_error(response, "GroenMonitor")
  }

  return(response)
}
