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

# Enable webmockr for the duration of a test, with a stub STAC API at example.org.
# `items` is the list of STAC items returned by every search; `matched` the reported total.
local_stac_api <- function(items = list(stac_item("a")), matched = length(items), env = parent.frame()) {
  webmockr::enable(adapter = "httr", quiet = TRUE)
  withr::defer({
    webmockr::stub_registry_clear()
    webmockr::request_registry_clear()
    webmockr::disable(adapter = "httr", quiet = TRUE)
  }, envir = env)
  withr::local_options(rNDC.endpoint = "https://example.org/api/", .local_envir = env)

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
  webmockr::stub_request("post", "https://example.org/api/search") |>
    webmockr::wi_th(headers = auth) |>
    webmockr::to_return(
      body = json_body(list(type = "FeatureCollection", features = items,
                            numberMatched = matched, links = list())),
      headers = json_header)
  webmockr::stub_request("get", "https://example.org/api/collections") |>
    webmockr::wi_th(headers = auth) |>
    webmockr::to_return(
      body = json_body(list(collections = list(stac_collection("a"), stac_collection("b")),
                            links = list())),
      headers = json_header)
  invisible(NULL)
}

# Body of the last POST request sent to the stubbed API
last_request_body <- function() {
  reqs <- webmockr::request_registry()$request_signatures$hash
  bodies <- Filter(nzchar, vapply(reqs, function(r) r$sig$body %||% "", character(1)))
  jsonlite::fromJSON(bodies[[length(bodies)]], simplifyVector = FALSE)
}
