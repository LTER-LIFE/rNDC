# Internal helpers for HTTP requests

# Stop with an informative message when an API token is missing (NULL, NA or empty), instead of sending a request
# that the server will answer with an opaque 401.
check_token <- function(token, service, envvar) {
  if (length(token) != 1 || is.na(token) || !nzchar(trimws(as.character(token)))) {
    stop(sprintf("%s token is missing. Set the `%s` environment variable or pass `token`.", service, envvar),
         call. = FALSE)
  }
  invisible(token)
}

# Raise an informative error for a failed httr response, including the message sent by the server
# (JSON or OWS/XML exception reports), instead of just the status text.
stop_for_http_error <- function(response, service) {
  if (!httr::http_error(response)) return(invisible(response))

  msg <- tryCatch(httr::content(response, as = "text", encoding = "UTF-8"),
                  error = function(e) "")
  msg <- gsub("<[^>]+>", " ", msg)
  msg <- trimws(gsub("[[:space:]]+", " ", gsub("[\"{}]", "", msg)))
  if (nchar(msg) > 300) msg <- paste0(substr(msg, 1, 300), "...")

  # a classed error, so that callers can tell e.g. a missing resource (404) from a server failure
  stop(structure(class = c("rNDC_http_error", "error", "condition"),
                 list(message = sprintf("%s request failed (HTTP %s)%s", service, httr::status_code(response),
                                        if (nzchar(msg)) paste0(": ", msg) else "."),
                      call = NULL, status = httr::status_code(response))))
}
