fake_stac_meta <- function() {
  tibble::tibble(
    id = c("a", "b", "c"),
    title = "t",
    layer = c("ntot", "nox", "ntot"),
    observation_date = c("2024-01-01", "2024-01-01", "2025-01-01"),
    year = c("2024", "2024", "2025"),
    keywords = list(character(0)),
    href = c("http://h/a", "http://h/b", "http://h/c")
  )
}

fake_response <- function(body, status = 200L) {
  structure(
    list(url = "http://example.org", status_code = status,
         headers = structure(list(`content-type` = "application/json"), class = c("insensitive", "list")),
         content = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))),
    class = "response"
  )
}

square_4326 <- function(xmin = 0.1, ymin = 0.1, xmax = 0.9, ymax = 0.9) {
  sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                                         crs = sf::st_crs(4326))))
}

# ---- transport-level HTTP mocking with webmockr (rstac and httr both go through httr) ----

json_header <- list("Content-Type" = "application/json")

stac_item <- function(id, layer = "ntot", date = "2024-01-01", keywords = list("lgn")) {
  list(type = "Feature", stac_version = "1.0.0", id = id, collection = "coll",
       geometry = list(type = "Polygon",
                       coordinates = list(list(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
       bbox = c(0, 0, 1, 1),
       properties = list(datetime = paste0(date, "T00:00:00Z"),
                         `ndc:layer_type` = layer, `ndc:observation_date` = date,
                         title = paste("Item", id), keywords = keywords),
       links = list(),
       assets = list(wcs = list(href = paste0("https://example.org/data/", id))))
}

stac_collection <- function(id) {
  list(type = "Collection", stac_version = "1.0.0", id = id, description = "d",
       license = "proprietary",
       extent = list(spatial = list(bbox = list(c(0, 0, 1, 1))),
                     temporal = list(interval = list(list(NULL, NULL)))),
       links = list())
}

json_body <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))

# A STAC item with the given properties (e.g. of the NDVI collections)
stac_feature <- function(id, properties = list()) {
  list(type = "Feature", stac_version = "1.0.0", id = as.character(id), collection = "coll",
       geometry = list(type = "Polygon",
                       coordinates = list(list(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
       bbox = c(0, 0, 1, 1),
       properties = c(list(datetime = "2024-01-01T00:00:00Z"), properties), links = list())
}

# An item of an NDVI collection
ndvi_item <- function(id, ndc_id, date, mean, std = NULL) {
  props <- list(ndc_id = ndc_id, observation_date = paste0(date, "T00:00:00Z"), ndvi_mean = mean)
  if (!is.null(std)) props$ndvi_std <- std
  stac_feature(id, props)
}

# Enable webmockr for the duration of a test, with a stub STAC API at example.org.
# `items` is the list of STAC items returned by every search; `matched` the reported total.
# `collections` is the list of collections that the API lists.
# With `pages` (a list with the items of each page) the searches return the pages one after the other, linked
# as in the STAC specification; `status` is the HTTP status of the searches.
local_stac_api <- function(items = list(stac_item("a")), matched = length(items), pages = NULL, status = 200L,
                           env = parent.frame(), collections = list(stac_collection("a"), stac_collection("b"))) {
  webmockr::enable(adapter = "httr", quiet = TRUE)
  withr::defer({
    webmockr::stub_registry_clear()
    webmockr::request_registry_clear()
    webmockr::disable(adapter = "httr", quiet = TRUE)
  }, envir = env)
  withr::local_options(rNDC.endpoint = "https://example.org/api/", .local_envir = env)
  # a second call in the same test replaces the stubs (and the sequence of pages) of the first
  webmockr::stub_registry_clear()
  webmockr::request_registry_clear()

  auth <- list(Authorization = "Bearer t")
  webmockr::stub_request("get", "https://example.org/api/") |>
    webmockr::to_return(
      body = json_body(list(
        type = "Catalog", id = "x", description = "x", stac_version = "1.0.0",
        conformsTo = list("https://api.stacspec.org/v1.0.0/core",
                          "https://api.stacspec.org/v1.0.0/item-search",
                          "https://api.stacspec.org/v1.0.0/collections"),
        links = list())),
      headers = json_header)
  search <- webmockr::stub_request("post", "https://example.org/api/search") |>
    webmockr::wi_th(headers = auth)
  if (is.null(pages)) pages <- list(items)
  for (i in seq_along(pages)) {
    links <- if (i < length(pages)) {
      list(list(rel = "next", href = "https://example.org/api/search", method = "POST",
                body = list(page = i + 1L), merge = FALSE))
    } else list()
    total <- if (length(pages) > 1) sum(lengths(pages)) else matched
    search <- webmockr::to_return(
      search, status = status,
      body = json_body(list(type = "FeatureCollection", features = pages[[i]],
                            numberMatched = total, links = links)),
      headers = json_header)
  }
  webmockr::stub_request("get", "https://example.org/api/collections") |>
    webmockr::wi_th(headers = auth) |>
    webmockr::to_return(
      body = json_body(list(collections = collections, links = list())),
      headers = json_header)
  invisible(NULL)
}

# Body of the last POST request sent to the stubbed API
last_request_body <- function() {
  reqs <- webmockr::request_registry()$request_signatures$hash
  bodies <- Filter(nzchar, vapply(reqs, function(r) r$sig$body %||% "", character(1)))
  jsonlite::fromJSON(bodies[[length(bodies)]], simplifyVector = FALSE)
}

# Enable webmockr for the duration of a test (the AgroDataCube requests are stubbed in the test itself).
with_adc_stubs <- function(env = parent.frame()) {
  webmockr::enable(adapter = "httr", quiet = TRUE)
  webmockr::request_registry_clear()  # the requests of earlier tests must not be counted
  withr::defer({ webmockr::stub_registry_clear(); webmockr::disable(adapter = "httr", quiet = TRUE) },
               envir = env)
}

# a STAC item of the collection `collection`, with the datetime `date`
in_collection <- function(collection, date = "2024-01-01", id = paste0(collection, date)) {
  item <- stac_item(id)
  item$collection <- collection
  item$properties$datetime <- paste0(date, "T00:00:00Z")
  item
}

# ---- shared by several test files ----

mock_download <- function(href, outfile, headers, overwrite = TRUE, ...) {
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                   crs = "EPSG:4326", vals = 1:16)
  terra::writeRaster(r, outfile, overwrite = TRUE)
  outfile
}

nitrogen_items <- function() {
  list(stac_item("a", layer = "ntot", date = "2024-01-01"),
       stac_item("b", layer = "nox", date = "2024-01-01"),
       stac_item("c", layer = "ntot", date = "2025-01-01"))
}

# an error as raised by gm_get() for a failed request
http_error <- function(status) {
  structure(class = c("rNDC_http_error", "error", "condition"),
            list(message = paste("GroenMonitor request failed (HTTP", status, ")"), call = NULL, status = status))
}

# a fake gm_get that writes a small GeoTIFF, as the WCS of GroenMonitor would; the days in `missing` have no coverage
fake_ndvi_download <- function(missing = character(0)) {
  function(url, option, params, out_path, overwrite = TRUE) {
    if (any(vapply(missing, grepl, NA, x = url, fixed = TRUE))) stop(http_error(404))
    terra::writeRaster(terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = c(0.2, 0.4, 0.6, 0.8)),
                       out_path, overwrite = TRUE)
    invisible(NULL)
  }
}

station_feature <- function(id, x, y) {
  list(type = "Feature", geometry = list(type = "Point", coordinates = c(x, y)),
       properties = list(meteostationid = id))
}

ndvi_items <- function() {
  list(ndvi_item("a", "1", "2024-05-01", 0.5, 0.1), ndvi_item("b", "1", "2024-05-14", 0.7, 0.3),
       ndvi_item("c", "1", "2024-06-02", 0.8, 0.2),
       ndvi_item("d", "2", "2024-05-02", 0.9, 0.5))  # a neighbouring polygon
}

square <- function(...) {
  sf::st_sf(..., geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = 0.1, ymin = 0.1, xmax = 0.9, ymax = 0.9),
                                                       crs = sf::st_crs(4326))))
}

# ---- files with the parameters of a retrieval (R/utils.R) ----

# the search body that was sent last, without the fields that do not describe the search
sent <- function() {
  b <- last_request_body()
  b[c("collections", "intersects", "datetime", "limit")]
}

read_file <- function(f) jsonlite::fromJSON(f, simplifyVector = FALSE)

# A file with the parameters must not hold the token nor where files go
expect_no_secrets <- function(f) {
  text <- paste(readLines(f), collapse = "\n")
  testthat::expect_false(grepl("token|Bearer|out_dir|out_path|file_prefix|overwrite|SECRET", text, ignore.case = TRUE))
}

meteo_url <- function(from, to) {
  paste0("https://agrodatacube.wur.nl/api/v2/rest/meteodata?output_epsg=4326&meteostation=260",
         "&fromdate=", from, "&todate=", to, "&page_size=500&page_offset=0")
}

# a collection as the API publishes it: with the Scientific extension, providers and a field of another extension
# a collection as the API publishes it: with the Scientific extension, providers and a field of another extension
rich_collection <- function(id = "lgn") {
  list(type = "Collection", stac_version = "1.0.0", id = id, title = "Land Use (LGN)",
       description = "Land use of the Netherlands.", license = "CC-BY-4.0",
       keywords = list("lgn", "land-use"),
       stac_extensions = list("https://stac-extensions.github.io/scientific/v1.0.0/schema.json"),
       extent = list(spatial = list(bbox = list(c(3.31, 50.75, 7.23, 53.55))),
                     temporal = list(interval = list(list("2024-01-01T00:00:00Z", "2024-12-31T00:00:00Z")))),
       providers = list(list(name = "NatureDataCube", url = "https://example.org/geoserver",
                             roles = list("host", "processor"), description = "Publication endpoint.")),
       `sci:citation` = "Hazeu, G. (2026). LGN2024 [Data set]. 4TU.ResearchData.",
       `sci:doi` = "10.4121/abc",
       `sci:publications` = list(list(doi = "10.1/xyz", citation = "Hazeu et al. (2026). A paper."),
                                 list(citation = "A report without DOI.")),
       `proj:epsg` = 28992, links = list())
}
