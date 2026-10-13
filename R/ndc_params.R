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
# Types: "text" and "texts" (one, or one or more strings), "integer", "count" (a whole number of at least 1), "number", "flag" (TRUE/FALSE), "any" (a string
# or a number), "date" (a string "YYYY-MM-DD", or a date-time), "roi" (a geometry, as GeoJSON in EPSG:4326) and "map"
# (a named list of strings: the `params` of the AgroDataCube and GroenMonitor functions).
params_specs <- list(
  ndc_get = list(endpoint = TRUE, args = c(collection = "texts", roi = "roi", trange = "text", asset_names = "texts",
                                           limit = "count", mode = "text", all_pages = "flag")),
  get_landuse_raster = list(endpoint = TRUE, args = c(aoi = "roi", year = "any", collection = "text", limit = "count",
                                                      subset_crs = "integer", min_file_size = "integer")),
  get_nitrogen_raster = list(endpoint = TRUE, args = c(aoi = "roi", year = "any", layers = "texts", collection = "texts",
                                                       limit = "count")),
  get_ndvi_stats = list(endpoint = TRUE, args = c(aoi = "roi", collection = "text", from = "date", to = "date",
                                                  ndc_id = "texts")),
  download_avg_ndvi_month = list(endpoint = FALSE, args = c(poly = "roi", year = "integer", month = "integer",
                                                            epsg = "integer")),
  download_avg_ndvi_stack = list(endpoint = FALSE, args = c(poly = "roi", start_year = "integer", start_month = "integer",
                                                            end_year = "integer", end_month = "integer", epsg = "integer")),
  get_closest_meteostation = list(endpoint = FALSE, args = c(polygon_wkt = "roi", page_size = "count",
                                                             page_offset = "integer", output_epsg = "any")),
  get_meteo_for_date = list(endpoint = FALSE, args = c(stationid = "any", date = "date", page_size = "count",
                                                       page_offset = "integer", output_epsg = "any")),
  get_meteo_for_period = list(endpoint = FALSE, args = c(meteostation = "any", fromdate = "date", todate = "date",
                                                         page_size = "count", page_offset = "integer", output_epsg = "any")),
  get_meteo_for_long_period = list(endpoint = FALSE, args = c(meteostation = "any", fromdate = "date", todate = "date",
                                                              by_days = "count", page_size = "count",
                                                              output_epsg = "any", sleep_sec = "number")),
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

#' Repeat a retrieval from a file with its parameters
#'
#' The retrieval functions can record their parameters in a JSON file (`save_params = "file.json"`) and run again
#' from such a file (`params_file = "file.json"`): the file is a record of what was asked, and `ndc_repeat()` runs a file
#' without having to know which function it is for.
#'
#' The functions with `save_params` and `params_file` are [ndc_get()], [get_landuse_raster()], [get_nitrogen_raster()],
#' [get_ndvi_stats()], [download_avg_ndvi_month()], [download_avg_ndvi_stack()], [get_closest_meteostation()],
#' [get_meteo_for_date()], [get_meteo_for_period()], [get_meteo_for_long_period()], [adc_get()], [adc_get_all()] and
#' [gm_get()]. A function with `save_params` writes the file when it has finished without an error (`ndc_get()`: when
#' the search has succeeded). `params_file` supplies the arguments that you do not give yourself; what you give overrides the
#' file, also `NULL` (for example `roi = NULL` leaves the area out).
#'
#' The file has `schema` (the format and the function: `"rNDC-get_landuse_raster/1"`), the arguments of the function as
#' they were when it finished (the defaults too, so that a later change of a default does not change the file's
#' meaning), and what describes the run: the time (`created`), the rNDC version (`rNDC`) and, for the functions that use
#' the NatureDataCube, its `endpoint`. An area is written as a GeoJSON geometry in EPSG:4326 whatever form it was given
#' in, so the file does not depend on a path that may change, and a period as text. The `ndc_get()` file also has the
#' number of items matched (`matched`) and, with `save_ids = TRUE`, their ids (`items`).
#'
#' **What is not in a file.** The token is never written, and neither are the places where files go (`output_dir`,
#' `out_dir`, `out_path`, `file_prefix`, `overwrite`): they are for the person who runs the retrieval. A file is not
#' read for them either. A file is a record of the question, not of the answer, which changes as the data does.
#' Fields that a function does not take from a file are ignored with a warning, a file for another function is refused,
#' and a file written for another endpoint is run at the current one, with a warning. Coordinates are written with about
#' 15 significant digits.
#'
#' **What cannot be recorded.** [assets_download_wcs()] takes the items of a search, and the functions that list
#' what the catalogue holds ([ndc_datasets()], [ndc_count()], [ndc_available()], [ndc_metadata()]) are questions about
#' the catalogue, not retrievals.
#'
#' @param path character. Path of the file.
#' @param ... Arguments of the function of the file that override those of the file, such as `token` (which is never in
#'   a file), `mode` or `out_dir`.
#' @returns What the function of the file returns.
#' @examples
#' \dontrun{
#' get_landuse_raster(c(5.74, 52.05, 5.76, 52.07), year = 2024, save_params = "landuse.json")
#' ndc_repeat("landuse.json")                      # again, here or on another machine
#' ndc_repeat("landuse.json", year = 2023)         # the same area, another year
#' }
#' @export
ndc_repeat <- function(path, ...) {
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("The file with the parameters was not found: ", paste(path, collapse = ", "), call. = FALSE)
  }
  content <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                      error = function(e) stop("'", path, "' is not valid JSON: ", conditionMessage(e), call. = FALSE))
  schema <- if (is.list(content)) content$schema else NULL
  name <- if (is.character(schema) && length(schema) == 1) sub("^rNDC-([A-Za-z_]+)/[0-9]+$", "\\1", schema) else ""
  if (!name %in% names(params_specs)) {
    stop("'", path, "' is not a file with the parameters of a retrieval of rNDC (its `schema` is missing or unknown).", call. = FALSE)
  }
  do.call(get(name, envir = asNamespace("rNDC")), c(list(params_file = path), list(...)))
}
