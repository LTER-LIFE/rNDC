#' Repeat a retrieval from a file with its parameters
#'
#' The retrieval functions can record their parameters in a JSON file (`save_params = "file.json"`) and run again
#' from such a file (`params_file = "file.json"`): the file is a record of what was asked, and `ndc_repeat()` runs a file
#' without having to know which function it is for.
#'
#' The functions with `save_params` and `params_file` are [ndc_get()], [ndc_landuse_raster()], [ndc_nitrogen_raster()],
#' [ndc_ahn_raster()], [ndc_ahn_stats()], [ndc_soil_raster()], [ndc_ndvi_stats()], [ndc_ndvi_month()], [ndc_ndvi_stack()], [gm_ndvi_month()], [gm_ndvi_stack()], [adc_ndvi()],
#' [adc_weather_station()],
#' [adc_weather_date()], [adc_weather_period()], [adc_weather_long_period()], [adc_get()], [adc_get_all()] and
#' [gm_get()]. A function with `save_params` writes the file when it has finished without an error (`ndc_get()`: when
#' the search has succeeded). `params_file` supplies the arguments that you do not give yourself; what you give overrides the
#' file, also `NULL` (for example `roi = NULL` leaves the area out).
#'
#' The file has `schema` (the format and the function: `"rNDC-ndc_landuse_raster/1"`), the arguments of the function as
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
#' @seealso [ndc_params_write()] to write the file of a call that has not been made.
#' @examples
#' \dontrun{
#' ndc_landuse_raster(c(5.74, 52.05, 5.76, 52.07), year = 2024, save_params = "landuse.json")
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

#' Write the parameters of a retrieval to a file, without running it
#'
#' The retrieval functions record their parameters in a JSON file when they have finished (`save_params`, see
#' [ndc_repeat()]). `ndc_params_write()` writes the same file for a call that has not been made: give the name of the function
#' and its arguments, and [ndc_repeat()] runs the file later, here or somewhere else. An argument that is not given has
#' its default, which is written too (as the functions do), so that a later change of a default does not change what the file
#' says. The token and the places where files go are not part of a file, and cannot be given.
#'
#' @param fun character. The name of the function: one of those that [ndc_repeat()] lists, such as `"ndc_landuse_raster"` or
#'   `"adc_weather_period"`.
#' @param args named list. The arguments of the call (an area as anything [ndc_roi()] accepts, or a WKT text in EPSG:4326; a period
#'   as two dates; and so on). Arguments that the function does not record, such as `token`, are refused.
#' @param path character. Where to write the file.
#' @returns `path`, invisibly.
#' @seealso [ndc_repeat()]
#' @examples
#' \dontrun{
#' f <- tempfile(fileext = ".json")
#' ndc_params_write("ndc_landuse_raster", list(roi = c(5.74, 52.05, 5.76, 52.07), year = 2024), f)
#' ndc_repeat(f)                 # runs the call (add `token =` if NDC_TOKEN is not set)
#' }
#' @export
ndc_params_write <- function(fun, args, path) {
  spec <- params_specs[[fun]]
  if (!is.character(fun) || length(fun) != 1 || is.null(spec)) {
    stop("`fun` must be one of: ", paste(names(params_specs), collapse = ", "), ".", call. = FALSE)
  }
  if (length(args) > 0 && (is.null(names(args)) || any(!nzchar(names(args))))) {
    stop("`args` must be a list with a name for every argument.", call. = FALSE)
  }
  unknown <- setdiff(names(args), names(spec$args))
  if (length(unknown) > 0) {
    stop(fun, "() does not record: ", paste(unknown, collapse = ", "), " (it records: ", paste(names(spec$args), collapse = ", "), ").",
         call. = FALSE)
  }

  defaults <- formals(get(fun, envir = asNamespace("rNDC")))
  fields <- list(schema = params_schema(fun),
                 created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 rNDC = as.character(utils::packageVersion("rNDC")))
  if (spec$endpoint) fields$endpoint <- ndc_endpoint()
  for (arg in names(spec$args)) {
    value <- if (arg %in% names(args)) {
      args[[arg]]
    } else if (!identical(defaults[[arg]], quote(expr = ))) {
      eval(defaults[[arg]], envir = asNamespace("rNDC"))
    }
    encoded <- params_encode(value, spec$args[[arg]], arg)
    if (!is.null(encoded)) fields[[arg]] <- encoded
  }

  json <- jsonlite::toJSON(fields, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  tryCatch(writeLines(json, path, useBytes = TRUE),
           error = function(e) stop("Could not write the parameters to '", path, "': ", conditionMessage(e), call. = FALSE),
           warning = function(w) stop("Could not write the parameters to '", path, "': ", conditionMessage(w), call. = FALSE))
  invisible(path)
}
