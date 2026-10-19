# Helpers that the whole package uses: small operators, the checks of tokens and HTTP errors, and the machinery of the
# parameters files (`save_params` / `params_file`, see `ndc_repeat()`).

# ---- Small helpers ----

#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# The value of an argument that was renamed: `old_value` was given with the deprecated name `old`, which is now `new`.
# Warns that the old name is deprecated.
deprecated_argument <- function(old_value, old, new, fn) {
  .Deprecated(msg = sprintf("The argument `%s` of %s() is deprecated: use `%s`.", old, fn, new))
  old_value
}

# ---- Tokens and HTTP errors ----

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

# ---- Parameters of a retrieval in a JSON file ----

# Saving the parameters of a retrieval to a JSON file, and running the retrieval from such a file. The functions that
# can do it have the arguments `params_file` (read a file) and `save_params` (write one); see ndc_repeat() for the format.

# The version of the format of the files (the last number of `schema`); the files of a later version are refused
params_version <- 1L

# What is in the file of each function: the arguments that are written and read, with their type, and whether the
# function uses the NatureDataCube endpoint (then it is in the file, and a file for another one is warned about).
# What is not here is never written nor read: the token, where files go (`output_dir`, `out_dir`, `out_path`,
# `file_prefix`, `overwrite`), the endpoint argument of the functions that have one, and the objects that cannot be
# put in a file (the items given to assets_download_wcs()).
#
# Types: "text" and "texts" (one, or one or more strings), "trange" (a period, written as the STAC `datetime` text), "integer", "count" (a whole number of at least 1), "number", "flag" (TRUE/FALSE), "any" (a string
# or a number), "date" (a string "YYYY-MM-DD", or a date-time), "roi" (a geometry, as GeoJSON in EPSG:4326) and "map"
# (a named list of strings: the `params` of the AgroDataCube and GroenMonitor functions).
params_specs <- list(
  ndc_get = list(endpoint = TRUE, args = c(collections = "texts", roi = "roi", trange = "trange", asset_names = "texts",
                                           limit = "count", mode = "text", all_pages = "flag")),
  ndc_landuse_raster = list(endpoint = TRUE, args = c(roi = "roi", year = "any", collection = "text", limit = "count",
                                                      subset_crs = "integer", min_file_size = "integer")),
  ndc_nitrogen_raster = list(endpoint = TRUE, args = c(roi = "roi", year = "any", layers = "texts", collections = "texts",
                                                       limit = "count")),
  ndc_ndvi_stats = list(endpoint = TRUE, args = c(roi = "roi", collection = "text", trange = "trange", ndc_id = "texts")),
  ndc_ndvi_month = list(endpoint = TRUE, args = c(roi = "roi", year = "integer", month = "integer", collection = "text",
                                                  subset_crs = "integer", limit = "count", clip = "flag")),
  ndc_ndvi_stack = list(endpoint = TRUE, args = c(roi = "roi", trange = "trange", collection = "text",
                                                  subset_crs = "integer", limit = "count", clip = "flag")),
  gm_ndvi_month = list(endpoint = FALSE, args = c(roi = "roi", year = "integer", month = "integer", epsg = "integer")),
  gm_ndvi_stack = list(endpoint = FALSE, args = c(roi = "roi", trange = "trange", epsg = "integer")),
  adc_ndvi = list(endpoint = FALSE, args = c(roi = "roi", fieldid = "texts", trange = "trange", page_size = "count")),
  adc_weather_station = list(endpoint = FALSE, args = c(roi = "roi", page_size = "count", page_offset = "integer",
                                                        output_epsg = "any")),
  adc_weather_date = list(endpoint = FALSE, args = c(station = "any", date = "date", page_size = "count",
                                                     page_offset = "integer", output_epsg = "any")),
  adc_weather_period = list(endpoint = FALSE, args = c(station = "any", trange = "trange", page_size = "count",
                                                       page_offset = "integer", output_epsg = "any")),
  adc_weather_long_period = list(endpoint = FALSE, args = c(station = "any", trange = "trange", by_days = "count",
                                                            page_size = "count", output_epsg = "any", sleep_sec = "number")),
  adc_get = list(endpoint = FALSE, args = c(url = "text", option = "text", params = "map", server = "text")),
  adc_get_all = list(endpoint = FALSE, args = c(option = "text", params = "map", server = "text", page_size = "count",
                                                max_pages = "count")),
  gm_get = list(endpoint = FALSE, args = c(url = "text", option = "text", params = "map"))
)

# Fields of the file that describe the run, not the retrieval
params_description <- c("schema", "created", "rNDC", "endpoint", "matched", "items")

# Stands for "no return value" in the function that saves the parameters when a retrieval is done
params_no_value <- new.env()

params_schema <- function(name) paste0("rNDC-", name, "/", params_version)

# ---- writing ----

# One value as it goes in the file, by its type (NULL: not written)
params_encode <- function(value, type, what) {
  if (is.null(value) || length(value) == 0) return(NULL)
  switch(type,
    text = , any = if (is.na(value[1])) NULL else if (type == "any" && is.numeric(value)) unname(value[[1]]) else as.character(value[[1]]),
    texts = as.list(as.character(value)),
    trange = { text <- ndc_trange(value); if (is.na(text)) NULL else text },
    integer = , count = if (is.na(value[1])) NULL else as.integer(value[[1]]),
    number = if (is.na(value[1])) NULL else as.numeric(value[[1]]),
    flag = isTRUE(value[[1]]),
    date = {
      v <- value[[1]]
      if (is.na(v)) NULL else if (inherits(v, "POSIXt")) format(v, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      else if (inherits(v, "Date")) format(v, "%Y-%m-%d") else as.character(v)
    },
    roi = {
      # a text that is no file is a geometry in WKT (EPSG:4326)
      sfc <- if (is.character(value) && length(value) == 1 && !file.exists(value)) sf::st_as_sfc(value, crs = 4326) else ndc_roi(value)
      jsonlite::fromJSON(as.character(geojsonsf::sfc_geojson(sfc))[1], simplifyVector = FALSE)
    },
    map = {
      if (is.null(names(value)) || any(!nzchar(names(value)))) stop("The parameters `", what, "` must all have a name.", call. = FALSE)
      as.list(vapply(value, function(v) as.character(v[[1]]), ""))
    }
  )
}

# Write the parameters of the call of `name` that is running in `env` (a function frame) to `path`. `values` replaces
# the value of an argument (the normalised form of it); `extra` is added to the file as it is. An argument that was not
# given and has no default is left out; the others are written with the value they have (the defaults too).
params_save <- function(name, path, env, values = list(), extra = list()) {
  spec <- params_specs[[name]]
  fields <- list(schema = params_schema(name),
                 created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 rNDC = as.character(utils::packageVersion("rNDC")))
  if (spec$endpoint) fields$endpoint <- ndc_endpoint()

  for (arg in names(spec$args)) {
    value <- if (arg %in% names(values)) {
      values[[arg]]
    } else if (eval(call("missing", as.name(arg)), env) && !arg_has_default(name, arg)) {
      NULL
    } else {
      get(arg, envir = env)
    }
    encoded <- params_encode(value, spec$args[[arg]], arg)
    if (!is.null(encoded)) fields[[arg]] <- encoded
  }
  fields <- c(fields, extra[!vapply(extra, is.null, NA)])

  json <- jsonlite::toJSON(fields, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  tryCatch(writeLines(json, path, useBytes = TRUE),
           error = function(e) stop("Could not write the parameters to '", path, "': ", conditionMessage(e), call. = FALSE),
           warning = function(w) stop("Could not write the parameters to '", path, "': ", conditionMessage(w), call. = FALSE))
  invisible(path)
}

# Does the function `name` give its argument `arg` a default?
arg_has_default <- function(name, arg) {
  no_default <- vapply(formals(get(name, envir = asNamespace("rNDC"))), function(x) identical(x, quote(expr = )), NA)
  !no_default[[arg]]
}

# Write the parameters of the call that runs in the calling function when it returns without an error (not when it stops
# with one). Call it at the start of the function; what is written is what the arguments are when the function ends,
# so a function may normalise an argument it records (a year, the ids that an area stands for) but must not change it
# into something else (a parameter added to a list that is given back to the user, say): a repeat would add it again.
params_save_when_done <- function(name, path, env = parent.frame()) {
  thunk <- bquote({
    if (!identical(returnValue(params_no_value), params_no_value)) params_save(.(name), .(path), environment())
  })
  do.call(base::on.exit, list(thunk, TRUE), envir = env)
  invisible(NULL)
}

# ---- reading ----

# Read the parameters of `name` from `path`: a list with the arguments that the file sets, by type (`roi` as an sfc in
# EPSG:4326). Warns about fields it does not know (they are not used), and when the file is for another endpoint.
params_read <- function(name, path) {
  spec <- params_specs[[name]]
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("The file with the parameters was not found: ", paste(path, collapse = ", "), call. = FALSE)
  }
  content <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                      error = function(e) stop("'", path, "' is not valid JSON: ", conditionMessage(e), call. = FALSE))
  if (!is.list(content) || is.null(names(content))) stop("'", path, "' does not hold the parameters of a search.", call. = FALSE)

  schema <- content$schema
  found <- if (is.character(schema) && length(schema) == 1) regmatches(schema, regexec("^rNDC-([A-Za-z_]+)/([0-9]+)$", schema))[[1]] else character(0)
  if (length(found) != 3 || !found[2] %in% names(params_specs)) {
    stop("'", path, "' is not a file with the parameters of ", name, "() (its `schema` is missing or unknown; it should be ",
         params_schema(name), ").", call. = FALSE)
  }
  if (found[2] != name) {
    stop("'", path, "' holds the parameters of ", found[2], "(), not of ", name, "().", call. = FALSE)
  }
  if (as.integer(found[3]) > params_version) {
    stop("'", path, "' was written by a newer version of rNDC (", schema, "): update the package to read it.", call. = FALSE)
  }

  unknown <- setdiff(names(content), c(names(spec$args), params_description))
  if (length(unknown) > 0) {
    warning("Ignored the fields of '", basename(path), "' that ", name, "() does not take from a file: ",
            paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  endpoint <- content$endpoint
  if (spec$endpoint && is.character(endpoint) && length(endpoint) == 1 &&
      !identical(sub("/+$", "", endpoint), sub("/+$", "", ndc_endpoint()))) {
    warning("'", basename(path), "' was written for another endpoint (", endpoint, "); the search is made at ",
            ndc_endpoint(), ".", call. = FALSE)
  }

  out <- list()
  for (arg in intersect(names(spec$args), names(content))) {
    value <- content[[arg]]
    if (is.null(value)) next
    out[[arg]] <- params_decode(value, spec$args[[arg]], arg, path)
  }
  out
}

# One value of the file, checked and turned into what the function takes
params_decode <- function(value, type, arg, path) {
  fail <- function(what) stop("The field `", arg, "` of '", path, "' must be ", what, ".", call. = FALSE)
  scalar <- function(v) !is.list(v) && length(v) == 1 && !is.na(v)
  switch(type,
    text = , date = if (is.character(value) && scalar(value) && nzchar(value)) value else fail("a text"),
    trange = if (is.character(value) && scalar(value) && !is.na(ndc_trange(value))) value else fail("a period (a STAC datetime text)"),
    any = if (scalar(value) && (is.character(value) || is.numeric(value))) value else fail("a text or a number"),
    texts = {
      v <- unlist(value)
      if (is.character(v) && length(v) >= 1 && !anyNA(v)) v else fail("a list of texts")
    },
    integer = {
      v <- suppressWarnings(as.numeric(value))
      if (is.numeric(value) && scalar(value) && v == round(v)) as.integer(v) else fail("a whole number")
    },
    count = {
      v <- suppressWarnings(as.numeric(value))
      if (is.numeric(value) && scalar(value) && v == round(v) && v >= 1) as.integer(v) else fail("a positive whole number")
    },
    number = if (is.numeric(value) && scalar(value)) as.numeric(value) else fail("a number"),
    flag = if (is.logical(value) && scalar(value)) value else fail("true or false"),
    roi = {
      geometry <- jsonlite::toJSON(value, auto_unbox = TRUE, digits = NA)
      roi <- tryCatch(geojsonsf::geojson_sfc(as.character(geometry)),
                      error = function(e) stop("The field `", arg, "` of '", path, "' is not a GeoJSON geometry: ",
                                               conditionMessage(e), call. = FALSE))
      if (length(roi) != 1) fail("one geometry")
      sf::st_crs(roi) <- 4326
      roi
    },
    map = {
      if (!is.list(value) || is.null(names(value)) || !all(vapply(value, function(v) scalar(v) && !is.list(v), NA))) {
        fail("an object with texts or numbers")
      }
      vapply(value, as.character, "")
    }
  )
}

# Take the arguments of the call of `name` that is running in `env` from the file `path`: those that the call did
# not give itself, so that what is given overrides the file (also NULL).
params_apply <- function(name, path, env = parent.frame()) {
  file <- params_read(name, path)
  for (arg in names(file)) {
    if (eval(call("missing", as.name(arg)), env)) assign(arg, file[[arg]], envir = env)
  }
  invisible(file)
}

# ---- NDVI of several months ----

# The monthly mean NDVI of the months `month_seq` (the first day of each) as one stack: `files_of_month(year, month)`
# gives the daily files of a month (the days with data), they are averaged (gm_month_mean(), which removes the files)
# and the months are put on a common grid. NULL if no month has data. Shared by gm_ndvi_stack() and ndc_ndvi_stack().
ndvi_stack_months <- function(month_seq, files_of_month) {
  raster_list <- list()
  for (m in as.list(month_seq)) {
    message("Processing ", format(m, "%B %Y"))
    year_m <- lubridate::year(m)
    month_m <- as.integer(format(m, "%m"))

    files <- files_of_month(year_m, month_m)  # (before the call: gm_month_mean() removes the files when it ends)
    r_mean <- gm_month_mean(files, year_m, month_m)
    if (is.null(r_mean)) {
      message("No NDVI data available for ", format(m, "%B %Y"))
    } else {
      raster_list[[length(raster_list) + 1]] <- r_mean
    }
  }

  if (length(raster_list) == 0) {
    message("No NDVI data available for the selected range")
    return(NULL)
  }

  # The WCS snaps each month's coverage to its own acquisition grid, so the monthly means can come back on
  # slightly different extents, which rast() cannot combine. Align them to the largest one (nothing is cropped).
  # The layer names (ndvi_mean_YYYYMM) are kept, also when some months had no data.
  if (length(raster_list) > 1) {
    ref <- raster_list[[which.max(vapply(raster_list, terra::ncell, numeric(1)))]]
    raster_list <- lapply(raster_list, function(r) {
      if (terra::compareGeom(ref, r, stopOnError = FALSE, messages = FALSE)) r
      else terra::resample(r, ref, method = "bilinear")
    })
  }

  terra::rast(raster_list)
}
