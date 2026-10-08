#' Get data from _AgroDataCube_
#'
#' Wrapper to get data from _AgroDataCube_ through their REST API.
#'
#' @param url character. Request URL.
#' @param option character. Determines the type of request.
#' @param params vector or list. List of named parameters.
#' @param server character. Server to connect to, either the main or test _AgroDataCube_ server.
#' @param token character. API token.
#' @param download boolean. If `TRUE`, download file.
#' @param out_path character. Output path.
#' @param overwrite boolean. If `TRUE`, overwrite file.
#' @returns The parsed content of the response (a list for JSON responses). An error is raised if the request fails.
#' @export

adc_get <- function(url = NULL, option, params, server = "adc", download = FALSE,
                    token = Sys.getenv("ADC_TOKEN"), out_path = tempfile(), overwrite = TRUE) {

  # Servers
  server_code <- tolower(server)
  if (server_code %in% c("adc", "agrodatacube", "agro data cube")) {
    base_url <- "https://agrodatacube.wur.nl/api/v2/rest/"
  } else if (server_code %in% c("test", "adct", "agrodatacube-test", "agro data cube test")) {
    base_url <- "https://agrodatacube-test.containers.wur.nl/api/v2/rest/"
  } else {
    stop("Unknown `server`: use \"adc\" or \"test\".", call. = FALSE)
  }

  # Compose request URL
  if ((is.null(url) || missing(url)) && !missing(option) && !missing(params)) {
    request_url <- adc_url(option = option, params = params, base_url = base_url)
  } else {
    request_url <- url
  }
    
  if (is.null(request_url)) {
    stop("Provide either `url`, or `option` and `params`.", call. = FALSE)
  }

  # The health check is the only request that needs no token
  if (!grepl("/lifeprobe$", request_url)) {
    check_token(token, "AgroDataCube", "ADC_TOKEN")
  }

  # Compose request headers
  request_headers <- c("Accept" = "application/json;charset=utf-8",
                       "token"  = token)

  # Submit request
  if (download) {
    response <- VERB("GET", url = request_url, add_headers(request_headers),
                     write_disk(out_path, overwrite = overwrite))
  } else {
    response <- VERB("GET", url = request_url, add_headers(request_headers))
  }
  if (httr::http_error(response)) {
    if (download) on.exit(unlink(out_path), add = TRUE)  # remove the saved error body
    stop_for_http_error(response, "AgroDataCube")
  }
  return(content(response))
}
