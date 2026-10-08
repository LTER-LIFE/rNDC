test_that("ndc_get sends token, roi and datetime in the STAC search", {
  local_stac_api()
  res <- ndc_get("coll", roi = c(5, 52, 6, 53), trange = c("2024-01-01", "2024-02-01"), token = "t")
  expect_s3_class(res, "doc_items")
  expect_equal(res$numberMatched, 1)

  body <- last_request_body()
  expect_equal(body$collections, list("coll"))
  expect_equal(body$datetime, "2024-01-01T00:00:00Z/2024-02-01T00:00:00Z")
  expect_equal(body$intersects$type, "Polygon")
})

test_that("ndc_get does not warn when the first page is complete", {
  local_stac_api(list(stac_item("a"), stac_item("b")))
  expect_no_warning(ndc_get("coll", token = "t", mode = "tibble"))
})

test_that("ndc_get warns when a conversion mode returns an incomplete first page", {
  local_stac_api(list(stac_item("a"), stac_item("b")), matched = 5)
  expect_warning(ndc_get("coll", token = "t", mode = "sf"), "Only 2 of 5 matched items")
  expect_no_warning(ndc_get("coll", token = "t", mode = "items"))
})

test_that("ndc_get does not add filters when roi and trange are NULL", {
  local_stac_api()
  ndc_get("coll", token = "t")
  body <- last_request_body()
  expect_null(body$datetime)
  expect_null(body$intersects)
})

test_that("ndc_get fails when the token is rejected", {
  local_stac_api()
  expect_error(ndc_get("coll", token = "wrong"))
})

test_that("ndc_get output modes", {
  local_stac_api(list(stac_item("a"), stac_item("b")))
  tb <- ndc_get("coll", token = "t", mode = "tibble")
  expect_s3_class(tb, "tbl_df")
  expect_equal(nrow(tb), 2)
  sfo <- ndc_get("coll", token = "t", mode = "sf")
  expect_s3_class(sfo, "sf")
  expect_equal(nrow(sfo), 2)
})

test_that("ndc_get rejects invalid trange and mode before sending anything", {
  local_stac_api()
  expect_error(ndc_get("coll", trange = c(NA, NA), token = "t"), "Invalid `trange`")
  expect_error(ndc_get("coll", mode = "nope", token = "t"), "should be one of")
})

test_that("ndc_count and ndc_datasets use the matched counts", {
  local_stac_api(list(stac_item("a"), stac_item("b"), stac_item("c")))
  expect_equal(ndc_count(collection = "coll", token = "t"), 3)

  expect_equal(ndc_datasets(token = "t"), c("a", "b"))
  d <- ndc_datasets(token = "t", total = TRUE)
  expect_equal(d$dataset_id, c("a", "b"))
  expect_equal(d$n_total, c(3, 3))
  d <- ndc_datasets(roi = c(5, 52, 6, 53), token = "t")
  expect_equal(d$n_matched, c(3, 3))
})

test_that("assets_download_wcs requests the asset URLs", {
  local_stac_api()
  webmockr::stub_request("get", "https://example.org/data/a") |>
    webmockr::to_return(body = "x", status = 200)
  items <- ndc_get("coll", token = "t")
  res <- assets_download_wcs(items, asset_names = "wcs", output_dir = withr::local_tempdir())
  expect_equal(httr::status_code(res), 200L)
  expect_equal(res$url, "https://example.org/data/a")
})

test_that("adc_get sends the token header, returns parsed content and fails on HTTP errors", {
  withr::defer({ webmockr::stub_registry_clear(); webmockr::disable(adapter = "httr", quiet = TRUE) })
  webmockr::enable(adapter = "httr", quiet = TRUE)
  webmockr::stub_request("get", "https://agrodatacube.wur.nl/api/v2/rest/fields?output_epsg=4326") |>
    webmockr::wi_th(headers = list(token = "secret")) |>
    webmockr::to_return(body = json_body(list(features = list(list(id = 1)))), headers = json_header)
  webmockr::stub_request("get", "https://agrodatacube.wur.nl/api/v2/rest/fields?output_epsg=4326") |>
    webmockr::to_return(body = json_body(list(message = "denied")), status = 401, headers = json_header)

  res <- adc_get(option = "Fields", params = c(output_epsg = "4326"), token = "secret")
  expect_equal(res$features[[1]]$id, 1)
  expect_error(adc_get(option = "Fields", params = c(output_epsg = "4326"), token = "bad"), "401")
})

test_that("adc_get rejects unknown servers", {
  expect_error(adc_get(option = "Fields", params = c(a = "1"), server = "zzz"), "Unknown `server`")
})

station_feature <- function(id, x, y) {
  list(type = "Feature", geometry = list(type = "Point", coordinates = c(x, y)),
       properties = list(meteostationid = id))
}

test_that("get_closest_meteostation picks the nearest station", {
  with_adc_stubs()
  webmockr::stub_request("get", uri_regex = "agrodatacube.wur.nl/api/v2/rest/meteostations") |>
    webmockr::to_return(
      body = json_body(list(type = "FeatureCollection",
                            features = list(station_feature(1, 4.9, 52.4), station_feature(2, 6.5, 53.2)))),
      headers = json_header)
  poly <- "POLYGON((6.4 53.1, 6.6 53.1, 6.6 53.3, 6.4 53.3, 6.4 53.1))"
  res <- get_closest_meteostation(poly, token = "t")
  expect_equal(res$closest_id, "2")
  expect_length(res$distances, 2)
})

meteo_url <- function(from, to) {
  paste0("https://agrodatacube.wur.nl/api/v2/rest/meteodata?output_epsg=4326&meteostation=260",
         "&fromdate=", from, "&todate=", to, "&page_size=500&page_offset=0")
}

test_that("meteo functions fetch data, warn on empty results and surface HTTP errors", {
  with_adc_stubs()
  meteo <- function(date) list(type = "Feature", geometry = list(type = "Point", coordinates = c(5, 52)),
                               properties = list(date = date, tg = 10))
  webmockr::stub_request("get", meteo_url("20240101", "20240102")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection",
                                              features = list(meteo("20240101"), meteo("20240102")))),
                        headers = json_header)
  webmockr::stub_request("get", meteo_url("20240201", "20240202")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list())),
                        headers = json_header)
  webmockr::stub_request("get", meteo_url("20240301", "20240302")) |>
    webmockr::to_return(body = "{}", status = 500)

  res <- get_meteo_for_period(260, "2024-01-01", "2024-01-02", token = "t")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 2)
  expect_warning(expect_null(get_meteo_for_period(260, "2024-02-01", "2024-02-02", token = "t")),
                 "No meteo data")
  expect_error(get_meteo_for_period(260, "2024-03-01", "2024-03-02", token = "t"), "500")
})

test_that("get_meteo_for_long_period combines the chunks", {
  calls <- 0
  local_mocked_bindings(get_meteo_for_period = function(...) {
    calls <<- calls + 1
    sf::st_sf(day = calls, geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))
  })
  res <- suppressMessages(get_meteo_for_long_period(1, "2024-01-01", "2024-01-20", token = "t", by_days = 7))
  expect_equal(calls, 3)
  expect_equal(nrow(res), 3)
})

test_that("ndc_sites lists and loads study sites", {
  layers <- ndc_sites()
  expect_type(layers, "character")
  expect_s3_class(ndc_sites(layers[1]), "sf")
  expect_error(ndc_sites("nope"), "not found")
})

test_that("adc_get includes the API message in HTTP errors", {
  with_adc_stubs()
  webmockr::stub_request("get", "https://agrodatacube.wur.nl/api/v2/rest/fields?output_epsg=4326") |>
    webmockr::to_return(body = json_body(list(status = "Geometry area too large")), status = 403,
                        headers = json_header)
  expect_error(adc_get(option = "Fields", params = c(output_epsg = "4326"), token = "t"),
               "HTTP 403\\).*Geometry area too large")
})

test_that("get_meteo_for_date uses the stationid/date parameters", {
  with_adc_stubs()
  url <- paste0("https://agrodatacube.wur.nl/api/v2/rest/meteodata?output_epsg=4326&stationid=210",
                "&date=20160101&page_size=500&page_offset=0")
  webmockr::stub_request("get", url) |>
    webmockr::to_return(
      body = json_body(list(type = "FeatureCollection",
                            features = list(list(type = "Feature",
                                                 geometry = list(type = "Point", coordinates = c(5, 52)),
                                                 properties = list(datum = "2016-01-01", mean_temperature = 5.6))))),
      headers = json_header)
  res <- get_meteo_for_date(210, "2016-01-01", token = "t")
  expect_s3_class(res, "sf")
  expect_equal(res$mean_temperature, 5.6)
  expect_error(get_meteo_for_date(210, "not a date", token = "t"), "date must be")
})

test_that("gm_get writes the file, and removes it and reports the message on errors", {
  with_adc_stubs()
  p <- c(date = "20251225", xmin = "1", xmax = "2", ymin = "3", ymax = "4", format = "tiff")
  ok <- gm_url("NDVI", p)
  bad <- gm_url("WDVI", p)
  webmockr::stub_request("get", ok) |> webmockr::to_return(body = "x", status = 200)
  webmockr::stub_request("get", bad) |>
    webmockr::to_return(
      body = "<ows:ExceptionReport><ows:Exception><ows:ExceptionText>Could not locate coverage</ows:ExceptionText></ows:Exception></ows:ExceptionReport>",
      status = 404, headers = list("Content-Type" = "application/xml"))

  expect_equal(httr::status_code(gm_get(url = ok, out_path = withr::local_tempfile())), 200L)

  out <- withr::local_tempfile()
  expect_error(gm_get(url = bad, out_path = out), "HTTP 404\\): Could not locate coverage")
  expect_false(file.exists(out))
})

test_that("assets_download_wcs downloads several assets and checks every response", {
  local_stac_api(list(stac_item("a"), stac_item("b")))
  items <- ndc_get("coll", token = "t")
  webmockr::stub_request("get", "https://example.org/data/a") |> webmockr::to_return(status = 200, body = "x")
  webmockr::stub_request("get", "https://example.org/data/b") |> webmockr::to_return(status = 200, body = "y")
  res <- assets_download_wcs(items, output_dir = file.path(withr::local_tempdir(), "new", "dir"))
  expect_length(res, 2)
  expect_true(all(vapply(res, inherits, NA, "response")))

  local_stac_api(list(stac_item("c")))
  items <- ndc_get("coll", token = "t")
  webmockr::stub_request("get", "https://example.org/data/c") |> webmockr::to_return(status = 404)
  out <- file.path(withr::local_tempdir(), "o")
  expect_error(assets_download_wcs(items, output_dir = out), "404")
  expect_false(file.exists(file.path(out, "c.tif")))
})

test_that("an empty token is refused before any request is sent", {
  local_stac_api()
  withr::local_envvar(NDC_TOKEN = "", ADC_TOKEN = "")
  expect_error(ndc_get("coll"), "NatureDataCube token is missing")
  expect_error(ndc_get("coll", token = NA_character_), "token is missing")
  expect_error(ndc_get("coll", token = "  "), "token is missing")
  expect_error(ndc_datasets(), "NDC_TOKEN")
  expect_error(ndc_count(collection = "coll"), "token is missing")
  expect_error(adc_get(option = "Fields", params = c(page_size = "1")), "AgroDataCube token is missing")
  expect_error(adc_get(url = "https://agrodatacube.wur.nl/api/v2/rest/fields"), "ADC_TOKEN")
  expect_error(adc_get(), "Provide either")

  # the health check needs no token
  webmockr::stub_request("get", "https://agrodatacube.wur.nl/api/v2/rest/lifeprobe") |>
    webmockr::to_return(status = 200, body = "{}", headers = json_header)
  expect_no_error(adc_get(option = "Health_check", params = NULL))
})
