# The functions and arguments that were renamed still work, with a warning (R/deprecated.R).

test_that("every old function name is still there, and says which one to use", {
  old <- c(get_landuse_raster = "ndc_landuse_raster", get_nitrogen_raster = "ndc_nitrogen_raster",
           get_ndvi_stats = "ndc_ndvi_stats", get_closest_meteostation = "adc_weather_station",
           get_meteo_for_date = "adc_weather_date", get_meteo_for_period = "adc_weather_period",
           get_meteo_for_long_period = "adc_weather_long_period", download_avg_ndvi_month = "gm_ndvi_month",
           download_avg_ndvi_stack = "gm_ndvi_stack")
  for (name in names(old)) {
    expect_true(exists(name, asNamespace("rNDC")), info = name)
    expect_true(old[[name]] %in% getNamespaceExports("rNDC"), info = old[[name]])
    expect_true(name %in% getNamespaceExports("rNDC"), info = name)
    # the new function takes what the old one took, under the new names
    expect_true(grepl(old[[name]], paste(deparse(get(name, asNamespace("rNDC"))), collapse = " "), fixed = TRUE), info = name)
  }
})

test_that("the raster functions with their old names and arguments give the same result", {
  local_stac_api(nitrogen_items())
  urls <- character()
  local_mocked_bindings(stac_download_one = function(href, ...) { urls <<- c(urls, href); mock_download(href, ...) })

  expect_warning(old <- get_nitrogen_raster(square_4326(), "2024", token = "t", out_dir = withr::local_tempdir()),
                 "get_nitrogen_raster.*deprecated")
  old_urls <- urls; urls <- character()
  new <- ndc_nitrogen_raster(square_4326(), "2024", token = "t", output_dir = withr::local_tempdir())
  expect_equal(names(old$stack), names(new$stack))
  expect_equal(old_urls, urls)

  local_stac_api(list(stac_item("a", date = "2024-01-01")))
  expect_warning(lu <- get_landuse_raster(aoi = square_4326(), year = 2024, token = "t", out_dir = withr::local_tempdir(),
                                          min_file_size = 0), "get_landuse_raster.*deprecated")
  expect_equal(names(lu$stack), "LGN_2024")
})

test_that("the NDVI statistics keep their old arguments `from` and `to`", {
  local_stac_api(items = ndvi_items())
  expect_warning(old <- get_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", from = "2024-05-01", to = "2024-06-30", token = "t"),
                 "get_ndvi_stats.*deprecated")
  new <- ndc_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", trange = c("2024-05-01", "2024-06-30"), token = "t")
  expect_identical(old, new)

  expect_warning(get_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", from = as.Date("2024-06-01"), token = "t"), "deprecated")
  expect_equal(last_request_body()$datetime, "2024-06-01T00:00:00Z/..")   # an open end, as before
})

test_that("the weather functions with their old names and arguments give the same result", {
  with_adc_stubs()
  feature <- function(date) list(type = "Feature", geometry = list(type = "Point", coordinates = c(5, 52)),
                                 properties = list(datum = date, mean_temperature = 10))
  webmockr::stub_request("get", meteo_url("20240101", "20240102")) |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection", features = list(feature("2024-01-01")))),
                        headers = json_header)
  webmockr::stub_request("get", uri_regex = "agrodatacube.wur.nl/api/v2/rest/meteostations") |>
    webmockr::to_return(body = json_body(list(type = "FeatureCollection",
                                              features = list(station_feature(1, 4.9, 52.4), station_feature(260, 6.5, 53.2)))),
                        headers = json_header)

  # dates as Dates, as texts, or as YYYYMMDD
  expect_warning(a <- get_meteo_for_period(260, as.Date("2024-01-01"), as.Date("2024-01-02"), token = "t"), "deprecated")
  expect_warning(b <- get_meteo_for_period(meteostation = 260, fromdate = "2024-01-01", todate = "20240102", token = "t"), "deprecated")
  expect_identical(a, b)
  expect_identical(a, adc_weather_period(260, c("2024-01-01", "2024-01-02"), token = "t"))

  poly <- "POLYGON((6.4 53.1, 6.6 53.1, 6.6 53.3, 6.4 53.3, 6.4 53.1))"
  expect_warning(st <- get_closest_meteostation(polygon_wkt = poly, token = "t"), "deprecated")
  expect_equal(st$closest_id, "260")

  calls <- list()
  local_mocked_bindings(adc_weather_period = function(station, trange, ...) { calls[[length(calls) + 1]] <<- list(station, trange); NULL })
  warned <- testthat::capture_warnings(suppressMessages(get_meteo_for_long_period(5, "2024-01-01", "2024-01-20", token = "t", by_days = 10)))
  expect_true(any(grepl("get_meteo_for_long_period.*deprecated", warned)))   # (and that nothing was returned)
  expect_length(calls, 2)
})

test_that("the NDVI downloads keep their old arguments", {
  urls <- character()
  fake <- fake_ndvi_download()
  local_mocked_bindings(gm_get = function(url, ...) { urls <<- c(urls, url); fake(url, ...) })
  poly <- square_4326(0.1, 0.1, 0.9, 0.9)

  expect_warning(old <- suppressMessages(download_avg_ndvi_month(poly, 2024, 2)), "download_avg_ndvi_month.*deprecated")
  new <- suppressMessages(gm_ndvi_month(poly, 2024, 2))
  expect_equal(names(old), names(new))

  expect_warning(stack <- suppressMessages(download_avg_ndvi_stack(poly, 2024, 1, 2024, 2)), "download_avg_ndvi_stack.*deprecated")
  expect_equal(names(stack), names(suppressMessages(gm_ndvi_stack(poly, c("2024-01-01", "2024-02-29")))))
})

test_that("arguments that were renamed in functions that kept their name work, with a warning", {
  # aoi -> roi
  expect_warning(s <- stac_wcs_subset_suffix(aoi = square_4326()), "argument `aoi` of stac_wcs_subset_suffix\\(\\) is deprecated")
  expect_identical(s, stac_wcs_subset_suffix(square_4326()))
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 1, ymin = 0, ymax = 1, crs = "EPSG:4326", vals = 1:16)
  expect_warning(c1 <- stac_clip_raster_to_aoi(r, aoi = square_4326()), "stac_clip_raster_to_aoi")
  expect_equal(terra::values(c1), terra::values(stac_clip_raster_to_aoi(r, square_4326())))
  local_stac_api(nitrogen_items())
  expect_warning(m <- stac_collect_metadata(aoi = c(5, 52, 6, 53), token = "t", endpoint = ndc_endpoint(), collection = "coll"),
                 "stac_collect_metadata")
  expect_equal(m$id, c("a", "b", "c"))

  # out_path -> output_path
  with_adc_stubs()
  p <- c(date = "20251225", xmin = "1", xmax = "2", ymin = "3", ymax = "4", format = "tiff")
  webmockr::stub_request("get", gm_url("NDVI", p)) |> webmockr::to_return(body = "x", status = 200)
  out <- withr::local_tempfile()
  expect_warning(res <- gm_get(url = gm_url("NDVI", p), out_path = out), "argument `out_path` of gm_get\\(\\) is deprecated")
  expect_equal(httr::status_code(res), 200L)

  # collection -> collections: the name still works as an abbreviation, without a warning
  local_stac_api()
  expect_no_warning(ndc_get(collection = "coll", token = "t"))
  expect_equal(unlist(last_request_body()$collections), "coll")
})
