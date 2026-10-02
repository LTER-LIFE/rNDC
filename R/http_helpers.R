# Internal helpers for HTTP requests

# Raise an informative error for a failed httr response, including the message sent by the server
# (JSON or OWS/XML exception reports), instead of just the status text.
stop_for_http_error <- function(response, service) {
  if (!httr::http_error(response)) return(invisible(response))

  msg <- tryCatch(httr::content(response, as = "text", encoding = "UTF-8"),
                  error = function(e) "")
  msg <- gsub("<[^>]+>", " ", msg)
  msg <- trimws(gsub("[[:space:]]+", " ", gsub("[\"{}]", "", msg)))
  if (nchar(msg) > 300) msg <- paste0(substr(msg, 1, 300), "...")

  stop(sprintf("%s request failed (HTTP %s)%s", service, httr::status_code(response),
               if (nzchar(msg)) paste0(": ", msg) else "."),
       call. = FALSE)
}
