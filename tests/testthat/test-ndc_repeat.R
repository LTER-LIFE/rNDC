# The retrieval functions other than ndc_get() record their parameters in a JSON file and run again from it.

tmp_json <- function(env = parent.frame()) withr::local_tempfile(fileext = ".json", .local_envir = env)

test_that("every function with parameters in a file has the arguments that the file is made of", {
  expect_setequal(names(params_specs),
                  c("ndc_get", "ndc_landuse_raster", "ndc_nitrogen_raster", "ndc_ahn_raster", "ndc_ahn_stats", "ndc_soil_raster", "ndc_ndvi_stats", "ndc_ndvi_month",
                    "ndc_ndvi_stack", "gm_ndvi_month", "gm_ndvi_stack", "adc_ndvi", "adc_weather_station", "adc_weather_date", "adc_weather_period",
                    "adc_weather_long_period", "adc_get", "adc_get_all", "gm_get"))
  for (name in names(params_specs)) {
    spec <- params_specs[[name]]
    formal_names <- names(formals(get(name, envir = asNamespace("rNDC"))))
    expect_true(all(names(spec$args) %in% formal_names), info = paste(name, ": arguments of the file are not arguments"))
    expect_true(all(c("params_file", "save_params") %in% formal_names), info = name)
    # what must not be in a file is not an argument of the file
    expect_false(any(c("token", "out_dir", "out_path", "output_dir", "output_path", "overwrite", "file_prefix", "endpoint") %in%
                       names(spec$args)), info = name)
  }
})

test_that("ndc_landuse_raster records its parameters, and repeats them", {
  local_stac_api(list(stac_item("a", date = "2024-01-01")))
  urls <- character()
  local_mocked_bindings(stac_download_one = function(href, ...) { urls <<- c(urls, href); mock_download(href, ...) })
  f <- tmp_json()
  first <- ndc_landuse_raster(square_4326(), year = 2024, token = "t", output_dir = withr::local_tempdir(),
                              min_file_size = 0, limit = 50, save_params = f)
  p <- read_file(f)
  expect_equal(p$schema, "rNDC-ndc_landuse_raster/1")
  expect_equal(p$roi$type, "Polygon")
  expect_equal(list(p$year, p$collection, p$limit, p$subset_crs, p$min_file_size), list(2024, "lgn", 50, 32631, 0))
  expect_equal(p$endpoint, "https://example.org/api/")
  expect_no_secrets(f)

  first_urls <- urls
  urls <- character()
  again <- ndc_landuse_raster(params_file = f, token = "t", output_dir = withr::local_tempdir())
  expect_equal(urls, first_urls)                       # the same area, subset and file
  expect_equal(names(again$stack), names(first$stack))

  expect_error(ndc_landuse_raster(params_file = f, year = 1999, token = "t"), "year 1999")   # the call overrides the file
})

test_that("ndc_nitrogen_raster records its parameters, and repeats them with other ones", {
  local_stac_api(nitrogen_items())
  urls <- character()
  local_mocked_bindings(stac_download_one = function(href, ...) { urls <<- c(urls, href); mock_download(href, ...) })
  f <- tmp_json()
  first <- ndc_nitrogen_raster(square_4326(), "2024", token = "t", output_dir = withr::local_tempdir(), save_params = f)
  p <- read_file(f)
  expect_equal(p$schema, "rNDC-ndc_nitrogen_raster/1")
  expect_equal(c(p$year, unlist(p$layers)), c("2024", "ntot", "nox", "nh3"))        # the defaults are written too
  expect_equal(p$roi$type, "Polygon")
  expect_no_secrets(f)

  again <- ndc_nitrogen_raster(params_file = f, token = "t", output_dir = withr::local_tempdir())
  expect_equal(names(again$stack), names(first$stack))

  other <- ndc_nitrogen_raster(params_file = f, year = "2025", layers = "ntot", token = "t", output_dir = withr::local_tempdir())
  expect_equal(names(other$stack), "ntot_2025")
})

test_that("ndc_ndvi_stats records the polygons it kept, so that a file without the area's columns repeats it", {
  local_stac_api(items = ndvi_items())
  f <- tmp_json()
  first <- ndc_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", trange = c("2024-05-01", "2024-06-30"), token = "t",
                          save_params = f)
  p <- read_file(f)
  expect_equal(p$schema, "rNDC-ndc_ndvi_stats/1")
  expect_equal(c(p$collection, p$trange, unlist(p$ndc_id)), c("ndvi-lter", "2024-05-01T00:00:00Z/2024-06-30T00:00:00Z", "1"))
  expect_no_secrets(f)

  again <- ndc_ndvi_stats(params_file = f, token = "t")      # the area is a bare geometry now: ndc_id comes from the file
  expect_identical(again, first)
  expect_equal(unique(ndc_ndvi_stats(params_file = f, ndc_id = c("1", "2"), token = "t")$ndc_id), c("1", "2"))
})

test_that("the NDVI downloads record their parameters, and repeat them", {
  urls <- character()
  fake <- fake_ndvi_download()
  local_mocked_bindings(gm_get = function(url, ...) { urls <<- c(urls, url); fake(url, ...) })
  poly <- square_4326(0.1, 0.1, 0.9, 0.9)
  f <- tmp_json()

  first <- suppressMessages(gm_ndvi_month(poly, 2024, 2, save_params = f))
  p <- read_file(f)
  expect_equal(p$schema, "rNDC-gm_ndvi_month/1")
  expect_equal(list(p$year, p$month, p$epsg, p$scale), list(2024, 2, 32631, TRUE))
  expect_equal(p$roi$type, "Polygon")
  expect_null(p$endpoint)                               # not a NatureDataCube function
  expect_no_secrets(f)
  first_urls <- urls; urls <- character()
  again <- suppressMessages(gm_ndvi_month(params_file = f))
  expect_equal(urls, first_urls)
  expect_equal(names(again), names(first))

  g <- tmp_json(); urls <- character()
  stack <- suppressMessages(gm_ndvi_stack(poly, c("2024-01-01", "2024-02-29"), save_params = g))
  q <- read_file(g)
  expect_equal(q$trange, "2024-01-01T00:00:00Z/2024-02-29T00:00:00Z")
  first_urls <- urls; urls <- character()
  expect_equal(names(suppressMessages(gm_ndvi_stack(params_file = g))), names(stack))
  expect_equal(urls, first_urls)
})

test_that("the NDVI functions of the NatureDataCube record their parameters, and repeat them", {
  local_stac_api(ndvi_stac_items())
  urls <- character()
  local_mocked_bindings(stac_download_one = function(href, ...) { urls <<- c(urls, href); mock_download(href, ...) })
  f <- tmp_json()
  first <- suppressMessages(ndc_ndvi_stack(square_4326(), c("2024-02-01", "2024-03-31"), token = "t", clip = TRUE, save_params = f))
  p <- read_file(f)
  expect_equal(p$schema, "rNDC-ndc_ndvi_stack/1")
  expect_equal(list(p$trange, p$collection, p$subset_crs, p$limit, p$clip, p$scale),
               list("2024-02-01T00:00:00Z/2024-03-31T00:00:00Z", "ndvi", 32631, 100, TRUE, TRUE))
  expect_equal(p$endpoint, "https://example.org/api/")
  expect_no_secrets(f)
  first_urls <- urls; urls <- character()
  again <- suppressMessages(ndc_repeat(f, token = "t"))
  expect_equal(names(again), names(first))
  expect_equal(urls, first_urls)

  g <- tmp_json()
  month <- suppressMessages(ndc_ndvi_month(square_4326(), 2024, 2, token = "t", save_params = g))
  expect_equal(list(read_file(g)$year, read_file(g)$month), list(2024, 2))
  expect_equal(names(suppressMessages(ndc_ndvi_month(params_file = g, token = "t"))), names(month))
})

test_that("the AHN and soil map functions record their parameters, and repeat them", {
  local_stac_api(ahn_items())
  urls <- character()
  base <- mock_height_download()
  local_mocked_bindings(stac_download_one = function(href, ...) { urls <<- c(urls, href); base(href, ...) })
  f <- tmp_json()
  first <- ndc_ahn_raster(square_4326(), layers = "dtm", year = 2019, token = "t", output_dir = withr::local_tempdir(),
                          scale = FALSE, scalefactor = 0.5, save_params = f)
  p <- read_file(f)
  expect_equal(p$schema, "rNDC-ndc_ahn_raster/1")
  expect_equal(list(unlist(p$layers), unlist(p$year), p$collection, p$scale, p$scalefactor, p$max_cells),
               list("dtm", "2019", "ahnmetrics", FALSE, 0.5, 2e7))
  expect_equal(p$endpoint, "https://example.org/api/")
  expect_no_secrets(f)
  first_urls <- urls; urls <- character()
  again <- ndc_repeat(f, token = "t", output_dir = withr::local_tempdir())
  expect_equal(names(again$rasters), names(first$rasters))
  expect_equal(urls, first_urls)

  g <- tmp_json()
  stats <- ndc_ahn_stats(square_4326(), layers = "dtm", token = "t", save_params = g)
  expect_equal(read_file(g)$schema, "rNDC-ndc_ahn_stats/1")
  expect_identical(ndc_ahn_stats(params_file = g, token = "t"), stats)

  local_stac_api(list(soil_item()))
  local_legend()
  h <- tmp_json()
  soil <- ndc_soil_raster(square_4326(), token = "t", output_dir = withr::local_tempdir(), save_params = h)
  expect_equal(read_file(h)$schema, "rNDC-ndc_soil_raster/1")
  expect_equal(names(ndc_soil_raster(params_file = h, token = "t", output_dir = withr::local_tempdir())$rasters),
               names(soil$rasters))
})

test_that("adc_ndvi records its parameters, and repeats them", {
  local_mocked_bindings(
    adc_get = function(option, params, ...) list(features = list(ndvi_feature(as.integer(params[["fieldid"]]), "2024-01-01"))),
    adc_get_all = function(...) list(features = list(ndvi_feature(5L, "2024-01-01")))
  )
  f <- tmp_json()
  first <- adc_ndvi(fieldid = c(1, 2), trange = c("2024-01-01", "2024-01-01"), token = "SECRET-1", save_params = f)
  p <- read_file(f)
  expect_equal(p$schema, "rNDC-adc_ndvi/1")
  expect_equal(unlist(p$fieldid), c("1", "2"))
  expect_null(p$roi)
  expect_null(p$endpoint)
  expect_no_secrets(f)
  expect_identical(adc_ndvi(params_file = f, token = "SECRET-1"), first)

  g <- tmp_json()
  by_area <- adc_ndvi(roi = square_4326(), trange = c("2024-01-01", "2024-01-01"), token = "SECRET-1", save_params = g)
  expect_equal(read_file(g)$roi$type, "Polygon")
  expect_null(read_file(g)$fieldid)
  expect_identical(adc_ndvi(params_file = g, token = "SECRET-1"), by_area)
})

test_that("the weather functions record their parameters, and repeat them", {
  with_adc_stubs()
  feature <- function(date) list(type = "Feature", geometry = list(type = "Point", coordinates = c(5, 52)),
                                 properties = list(datum = date, mean_temperature = 10))
  webmockr::stub_request("get", meteo_url("20240101", "20240102")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list(feature("2024-01-01"), feature("2024-01-02")))),
                        headers = json_header)
  webmockr::stub_request("get", paste0("https://agrodatacube.wur.nl/api/v2/rest/meteodata?output_epsg=4326&stationid=210",
                                       "&date=20160101&page_size=500&page_offset=0")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list(feature("2016-01-01")))),
                        headers = json_header)

  f <- tmp_json()
  first <- adc_weather_period(260, c(as.Date("2024-01-01"), as.Date("2024-01-02")), token = "SECRET-1", save_params = f)
  p <- read_file(f)
  expect_equal(p$schema, "rNDC-adc_weather_period/1")
  expect_equal(list(p$station, p$trange, p$page_size), list(260, "2024-01-01T00:00:00Z/2024-01-02T00:00:00Z", 500))
  expect_no_secrets(f)
  expect_identical(adc_weather_period(params_file = f, token = "SECRET-1"), first)

  g <- tmp_json()
  day <- adc_weather_date("210", "2016-01-01", token = "SECRET-1", save_params = g)
  expect_equal(read_file(g)$station, "210")
  expect_no_secrets(g)
  expect_identical(adc_weather_date(params_file = g, token = "SECRET-1"), day)

  # the token comes from the environment when it is not given (the file never has it)
  withr::local_envvar(ADC_TOKEN = "from-the-environment")
  expect_identical(adc_weather_date(params_file = g), day)
})

test_that("adc_weather_long_period records the chunks it was asked for", {
  seen <- list()
  local_mocked_bindings(adc_weather_period = function(station, trange, ...) {
    seen[[length(seen) + 1]] <<- c(station, as.character(trange))
    sf::st_sf(a = 1, geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))
  })
  f <- tmp_json()
  suppressMessages(adc_weather_long_period(260, c("2024-01-01", "2024-01-20"), token = "SECRET", by_days = 7,
                                             sleep_sec = 0, save_params = f))
  p <- read_file(f)
  expect_equal(list(p$station, p$trange, p$by_days, p$sleep_sec), list(260, "2024-01-01T00:00:00Z/2024-01-20T00:00:00Z", 7, 0))
  expect_no_secrets(f)
  first <- seen; seen <- list()
  suppressMessages(adc_weather_long_period(params_file = f, token = "SECRET"))
  expect_equal(seen, first)
  seen <- list()
  suppressMessages(adc_weather_long_period(params_file = f, by_days = 30, token = "SECRET"))
  expect_length(seen, 1)
})

test_that("adc_weather_station records the area in any form, as a geometry", {
  with_adc_stubs()
  webmockr::stub_request("get", uri_regex = "agrodatacube.wur.nl/api/v2/rest/meteostations") |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection",
                                              features = list(station_feature(1, 4.9, 52.4), station_feature(2, 6.5, 53.2)))),
                        headers = json_header)
  wkt <- "POLYGON((6.4 53.1, 6.6 53.1, 6.6 53.3, 6.4 53.3, 6.4 53.1))"
  f <- tmp_json(); g <- tmp_json()
  first <- adc_weather_station(wkt, token = "t", save_params = f)
  adc_weather_station(sf::st_sf(geometry = sf::st_as_sfc(wkt, crs = 4326)), token = "t", save_params = g)
  expect_equal(read_file(f)$roi, read_file(g)$roi)       # a WKT text and an sf object: the same file
  expect_equal(read_file(f)$roi$type, "Polygon")
  expect_no_secrets(f)
  again <- adc_weather_station(params_file = f, token = "t")
  expect_equal(again$closest_id, first$closest_id)
  expect_equal(again$distances, first$distances)
})

test_that("adc_get, adc_get_all and gm_get record their parameters, whose `params` are the request parameters", {
  with_adc_stubs()
  url <- "https://agrodatacube.wur.nl/api/v2/rest/fields?output_epsg=4326&year=2024"
  webmockr::stub_request("get", url) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list())), headers = json_header)

  f <- tmp_json()
  first <- adc_get(option = "Fields", params = c(output_epsg = "4326", year = "2024"), token = "SECRET", save_params = f)
  p <- read_file(f)
  expect_equal(p$schema, "rNDC-adc_get/1")
  expect_equal(p$option, "Fields")
  expect_equal(p$params, list(output_epsg = "4326", year = "2024"))
  expect_equal(p$server, "adc")
  expect_null(p$url); expect_null(p$endpoint)
  expect_no_secrets(f)
  expect_identical(adc_get(params_file = f, token = "SECRET"), first)

  # a call with a URL records the URL, and what was not given is not in the file
  g <- tmp_json()
  adc_get(url = url, token = "SECRET", save_params = g)
  q <- read_file(g)
  expect_equal(q$url, url); expect_null(q$option); expect_null(q$params)
  expect_identical(adc_get(params_file = g, token = "SECRET"), first)

  # all pages: what the function asks of adc_get() is the same
  calls <- list()
  local_mocked_bindings(adc_get = function(option, params, token, server, ...) {
    calls[[length(calls) + 1]] <<- list(option, params, server)
    list(type = "FeatureCollection", features = list())
  })
  h <- tmp_json()
  adc_get_all("Fields", c(geometry = "POINT(5 52)", year = "2024"), token = "SECRET", page_size = 200, save_params = h)
  r <- read_file(h)
  expect_equal(list(r$option, r$page_size, r$max_pages), list("Fields", 200, 100))
  expect_equal(r$params, list(geometry = "POINT(5 52)", year = "2024"))
  expect_no_secrets(h)
  first_calls <- calls; calls <- list()
  adc_get_all(params_file = h, token = "SECRET")
  expect_equal(calls, first_calls)
})

test_that("gm_get records its parameters, and repeats them", {
  with_adc_stubs()
  p <- c(date = "20251225", xmin = "1", xmax = "2", ymin = "3", ymax = "4", format = "tiff")
  ok <- gm_url("NDVI", p)
  webmockr::stub_request("get", ok) |> webmockr::to_return(body = "x", status = 200)
  f <- tmp_json()
  gm_get(option = "NDVI", params = p, output_path = withr::local_tempfile(), save_params = f)
  q <- read_file(f)
  expect_equal(q$schema, "rNDC-gm_get/1")
  expect_equal(q$params$date, "20251225")
  expect_no_secrets(f)
  res <- gm_get(params_file = f, output_path = withr::local_tempfile())
  expect_equal(httr::status_code(res), 200L)
  expect_equal(res$url, ok)
})

test_that("ndc_repeat runs a file without knowing its function", {
  local_stac_api(nitrogen_items())
  local_mocked_bindings(stac_download_one = function(href, ...) mock_download(href, ...))
  f <- tmp_json()
  first <- ndc_nitrogen_raster(square_4326(), "2024", token = "t", output_dir = withr::local_tempdir(), save_params = f)
  again <- ndc_repeat(f, token = "t", output_dir = withr::local_tempdir())
  expect_equal(names(again$stack), names(first$stack))
  expect_equal(names(ndc_repeat(f, year = "2025", token = "t", output_dir = withr::local_tempdir())$stack), "ntot_2025")

  # a function whose `params` are request parameters
  with_adc_stubs()
  url <- "https://agrodatacube.wur.nl/api/v2/rest/fields?output_epsg=4326"
  webmockr::stub_request("get", url) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list())), headers = json_header)
  g <- tmp_json()
  adc_get(url = url, token = "t", save_params = g)
  expect_equal(ndc_repeat(g, token = "t")$type, "FeatureCollection")

  h <- tmp_json()
  writeLines('{"schema": "rNDC-nope/1"}', h)
  expect_error(ndc_repeat(h), "not a file with the parameters of a retrieval")
  writeLines("{not json", h)
  expect_error(ndc_repeat(h), "not valid JSON")
  expect_error(ndc_repeat(file.path(tempdir(), "none.json")), "not found")
})

test_that("a file is for one function, and nothing is written when the function stops with an error", {
  local_stac_api(list(stac_item("a")))
  f <- tmp_json()
  ndc_get("coll", token = "t", save_params = f)
  expect_error(ndc_landuse_raster(params_file = f, token = "t"), "holds the parameters of ndc_get\\(\\), not of ndc_landuse_raster\\(\\)")

  # a failing call leaves no file, and an existing one is not touched
  g <- tmp_json()
  local_stac_api(list())
  expect_error(ndc_landuse_raster(square_4326(), year = 2024, token = "t", output_dir = withr::local_tempdir(), save_params = g),
               "No Land Use raster items")
  expect_false(file.exists(g))
  expect_error(ndc_nitrogen_raster(square_4326(), year = "", token = "t", save_params = g))
  expect_false(file.exists(g))
  local_stac_api(items = ndvi_items(), status = 500L)
  expect_error(ndc_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", token = "t", save_params = g))
  expect_false(file.exists(g))
})

test_that("fields that a function does not take from a file are ignored with a warning, in every function", {
  f <- tmp_json()
  writeLines(paste0('{"schema": "rNDC-adc_weather_period/1", "station": 260, "trange": "2024-01-01T00:00:00Z/2024-01-02T00:00:00Z", ',
                    '"token": "SECRET", "out_dir": "/etc"}'), f)
  with_adc_stubs()
  webmockr::stub_request("get", meteo_url("20240101", "20240102")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list(
      list(type = "Feature", geometry = list(type = "Point", coordinates = c(5, 52)), properties = list(datum = "2024-01-01"))))),
      headers = json_header)
  expect_warning(adc_weather_period(params_file = f, token = "t"), "token, out_dir")
  # values of the wrong type are refused with the name of the field
  writeLines('{"schema": "rNDC-adc_weather_period/1", "station": [1], "page_size": 0}', f)
  expect_error(adc_weather_period(params_file = f, token = "t"), "`station`")
  writeLines('{"schema": "rNDC-adc_weather_period/1", "station": 1, "page_size": 0}', f)
  expect_error(adc_weather_period(params_file = f, token = "t"), "`page_size`")
})

# ---- ndc_params_write(): the file of a call that has not been made ----

# the fields of a file, without the time it was made
fields_of <- function(f) {
  x <- read_file(f)
  x$created <- NULL
  x
}

test_that("ndc_params_write() writes the file that the function records, and the defaults too", {
  local_stac_api(list(stac_item("a", date = "2024-01-01")))
  local_mocked_bindings(stac_download_one = function(href, ...) mock_download(href, ...))
  ran <- tmp_json()
  ndc_landuse_raster(square_4326(), year = 2024, token = "t", output_dir = withr::local_tempdir(), min_file_size = 0, limit = 50,
                     save_params = ran)
  written <- tmp_json()
  expect_equal(ndc_params_write("ndc_landuse_raster", list(roi = square_4326(), year = 2024, limit = 50, min_file_size = 0), written),
               written)
  expect_equal(fields_of(written), fields_of(ran))
  expect_equal(read_file(written)$schema, "rNDC-ndc_landuse_raster/1")
  expect_no_secrets(written)

  # ndc_nitrogen_raster: the year as text and the layers, as the function normalises them
  ran_n <- tmp_json()
  ndc_nitrogen_raster(square_4326(), year = "2024", layers = c("ntot", "nox"), token = "t", output_dir = withr::local_tempdir(),
                      save_params = ran_n)
  written_n <- tmp_json()
  ndc_params_write("ndc_nitrogen_raster", list(roi = square_4326(), year = "2024", layers = c("ntot", "nox")), written_n)
  expect_equal(fields_of(written_n), fields_of(ran_n))
})

test_that("ndc_params_write() matches the AHN, soil map and NDVI functions", {
  local_stac_api(c(ahn_items(), list(soil_item(), stac_item("n1", date = "2024-02-03"))))
  local_legend()
  local_mocked_bindings(stac_download_one = mock_height_download())
  same <- function(fun, run, args) {
    ran <- tmp_json()
    suppressMessages(run(ran))
    written <- tmp_json()
    ndc_params_write(fun, args, written)
    expect_equal(fields_of(written), fields_of(ran), info = fun)
  }
  roi <- square_4326()
  same("ndc_ahn_raster", function(f) ndc_ahn_raster(roi, layers = "dtm", year = 2019, token = "t", max_cells = Inf,
                                                     output_dir = withr::local_tempdir(), scale = FALSE, save_params = f),
       list(roi = roi, layers = "dtm", year = 2019, max_cells = Inf, scale = FALSE))
  same("ndc_ahn_stats", function(f) ndc_ahn_stats(roi, layers = "dtm", token = "t", max_cells = Inf, save_params = f),
       list(roi = roi, layers = "dtm", max_cells = Inf))
  same("ndc_soil_raster", function(f) ndc_soil_raster(roi, token = "t", output_dir = withr::local_tempdir(), save_params = f),
       list(roi = roi))
  same("ndc_ndvi_stack", function(f) ndc_ndvi_stack(roi, c("2024-02-01", "2024-03-31"), token = "t", save_params = f),
       list(roi = roi, trange = as.Date(c("2024-02-01", "2024-03-31"))))
  same("ndc_ndvi_month", function(f) ndc_ndvi_month(roi, 2024, 2, token = "t", save_params = f),
       list(roi = roi, year = 2024, month = 2))
})

test_that("ndc_params_write() matches the AgroDataCube functions, with a WKT text as the area", {
  with_adc_stubs()
  feature <- function(date) list(type = "Feature", geometry = list(type = "Point", coordinates = c(5, 52)),
                                 properties = list(datum = date, mean_temperature = 10))
  webmockr::stub_request("get", meteo_url("20240101", "20240102")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list(feature("2024-01-01")))), headers = json_header)
  ran <- tmp_json()
  adc_weather_period(260, c(as.Date("2024-01-01"), as.Date("2024-01-02")), token = "t", save_params = ran)
  written <- tmp_json()
  ndc_params_write("adc_weather_period", list(station = "260", trange = as.Date(c("2024-01-01", "2024-01-02"))), written)
  expect_equal(fields_of(written)[c("schema", "trange", "page_size")], fields_of(ran)[c("schema", "trange", "page_size")])
  expect_equal(fields_of(written)$station, "260")

  wkt <- "POLYGON((6.4 53.1, 6.6 53.1, 6.6 53.3, 6.4 53.3, 6.4 53.1))"
  station <- tmp_json()
  ndc_params_write("adc_weather_station", list(roi = wkt), station)
  expect_equal(read_file(station)$roi$type, "Polygon")
  expect_equal(read_file(station)$page_size, 10000)                     # (the default)
  expect_null(read_file(station)$endpoint)                              # not a NatureDataCube function

  fields <- tmp_json()
  ndc_params_write("adc_get_all", list(option = "Fields", params = c(geometry = wkt, epsg = "4326", year = "2023", output_epsg = "4326")), fields)
  expect_equal(read_file(fields)$params$year, "2023")
  expect_equal(c(read_file(fields)$page_size, read_file(fields)$max_pages), c(1000, 100))
})

test_that("a file from ndc_params_write() is run by ndc_repeat(), and a bad call is refused", {
  local_stac_api(list(stac_item("a", date = "2024-01-01")))
  local_mocked_bindings(stac_download_one = function(href, ...) mock_download(href, ...))
  f <- tmp_json()
  ndc_params_write("ndc_landuse_raster", list(roi = square_4326(), year = 2024, min_file_size = 0), f)
  res <- ndc_repeat(f, token = "t", output_dir = withr::local_tempdir())
  expect_equal(names(res$stack), "LGN_2024")

  expect_error(ndc_params_write("not_a_function", list(), f), "must be one of")
  expect_error(ndc_params_write(c("ndc_get", "gm_get"), list(), f), "must be one of")
  expect_error(ndc_params_write("ndc_landuse_raster", list(roi = square_4326(), token = "secret"), f), "does not record: token")
  expect_error(ndc_params_write("ndc_landuse_raster", list(square_4326()), f), "a name for every argument")
  expect_error(ndc_params_write("ndc_landuse_raster", list(roi = square_4326()), file.path(tempdir(), "no", "such", "dir", "x.json")),
               "Could not write the parameters")
})
