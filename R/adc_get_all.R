#' Get all pages of data from _AgroDataCube_
#'
#' Wrapper around [adc_get()] for requests whose results are paged (e.g. `"Fields"` and `"Soiltypes"`: the API
#' returns 50 features by default). It requests page after page, until a page comes back with fewer than
#' `page_size` features, and combines the features.
#'
#' @param option character. Determines the type of request, see [adc_url()].
#' @param params vector or list. List of named parameters, without `page_size` and `page_offset`.
#' @param token character. API token.
#' @param server character. Server to connect to, see [adc_get()].
#' @param page_size integer. Number of features requested per page (the maximum of the API is 1000 or more).
#' @param max_pages integer. Stop after this many pages (with a warning), as a safeguard.
#' @returns The parsed GeoJSON of the first page, with the features of all the pages.
#' @export

adc_get_all <- function(option, params, token = Sys.getenv("ADC_TOKEN"), server = "adc",
                        page_size = 1000L, max_pages = 100L) {
  params <- c(params, page_size = format(page_size, scientific = FALSE, trim = TRUE))
  res <- NULL

  for (page in seq_len(max_pages) - 1L) {
    page_params <- c(params, page_offset = format(page, scientific = FALSE, trim = TRUE))
    cur <- adc_get(option = option, params = page_params, server = server, token = token)
    if (is.null(res)) res <- cur else res$features <- c(res$features, cur$features)
    if (length(cur$features) < page_size) return(res)
  }

  warning("The request stopped after ", max_pages, " pages: the result may be incomplete.", call. = FALSE)
  res
}
