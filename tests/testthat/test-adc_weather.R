# The weather functions of the AgroDataCube: the closest station, a day, a period, a long period, and the split of a range.

test_that("adc_weather_station picks the nearest station", {
  with_adc_stubs()
  webmockr::stub_request("get", uri_regex = "agrodatacube.wur.nl/api/v2/rest/meteostations") |>
    webmockr::to_return(
      body = json_body(list(type = "FeatureCollection",
                            features = list(station_feature(1, 4.9, 52.4), station_feature(2, 6.5, 53.2)))),
      headers = json_header)
  poly <- "POLYGON((6.4 53.1, 6.6 53.1, 6.6 53.3, 6.4 53.3, 6.4 53.1))"
  res <- adc_weather_station(poly, token = "t")
  expect_equal(res$closest_id, "2")
  expect_length(res$distances, 2)

  # the study area can be anything that ndc_roi() accepts, in any CRS
  area <- sf::st_transform(sf::st_as_sf(sf::st_as_sfc(poly, crs = 4326)), 28992)
  expect_equal(adc_weather_station(area, token = "t")$closest_id, "2")
  expect_equal(adc_weather_station(c(6.4, 53.1, 6.6, 53.3), token = "t")$closest_id, "2")
  expect_error(adc_weather_station("not a polygon", token = "t"), "Invalid WKT")
})

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

  res <- adc_weather_period(260, c("2024-01-01", "2024-01-02"), token = "t")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 2)
  expect_warning(expect_null(adc_weather_period(260, c("2024-02-01", "2024-02-02"), token = "t")),
                 "No meteo data")
  expect_error(adc_weather_period(260, c("2024-03-01", "2024-03-02"), token = "t"), "500")
})

test_that("adc_weather_long_period combines the chunks", {
  calls <- 0
  local_mocked_bindings(adc_weather_period = function(...) {
    calls <<- calls + 1
    sf::st_sf(day = calls, geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))
  })
  res <- suppressMessages(adc_weather_long_period(1, c("2024-01-01", "2024-01-20"), token = "t", by_days = 7))
  expect_equal(calls, 3)
  expect_equal(nrow(res), 3)
})

test_that("adc_weather_date uses the station and date parameters", {
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
  res <- adc_weather_date(210, "2016-01-01", token = "t")
  expect_s3_class(res, "sf")
  expect_equal(res$mean_temperature, 5.6)
  expect_error(adc_weather_date(210, "not a date", token = "t"), "date must be")
})

test_that("split_date_range covers the range without overlap", {
  r <- split_date_range("2024-01-01", "2024-01-20", by_days = 7)
  expect_equal(nrow(r), 3)
  expect_equal(r$from, as.Date(c("2024-01-01", "2024-01-08", "2024-01-15")))
  expect_equal(r$to, as.Date(c("2024-01-07", "2024-01-14", "2024-01-20")))
})
