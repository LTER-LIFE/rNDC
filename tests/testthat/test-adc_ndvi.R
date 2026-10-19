# NDVI statistics of agricultural fields from the AgroDataCube.

test_that("field ids: one request for every field and date, and the rows are tidy", {
  calls <- list()
  local_mocked_bindings(adc_get = function(option, params, token, ...) {
    calls[[length(calls) + 1]] <<- list(option = option, params = params, token = token)
    day <- params[["date"]]
    features <- if (day == "20240102") list() else list(ndvi_feature(as.integer(params[["fieldid"]]), paste0(substr(day, 1, 4), "-", substr(day, 5, 6), "-", substr(day, 7, 8))))
    list(features = features)
  })
  out <- adc_ndvi(fieldid = c(10, 20), trange = c("2024-01-01", "2024-01-03"), token = "t")
  expect_length(calls, 6)
  expect_equal(calls[[1]]$option, "NDVI")
  expect_equal(calls[[1]]$token, "t")
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("fieldid", "date", "ndvi_avg", "ndvi_stddev"))
  expect_equal(out$date, as.Date(c("2024-01-01", "2024-01-01", "2024-01-03", "2024-01-03")))   # no row for the 2nd
  expect_equal(out$fieldid, c(10L, 20L, 10L, 20L))
  expect_type(out$ndvi_avg, "double")
})

test_that("an area: the fields of the polygon for each date, all pages", {
  calls <- list()
  local_mocked_bindings(adc_get_all = function(option, params, token, page_size, ...) {
    calls[[length(calls) + 1]] <<- list(option = option, params = params, page_size = page_size)
    list(features = list(ndvi_feature(2L, "2024-01-01"), ndvi_feature(1L, "2024-01-01")))
  })
  out <- adc_ndvi(roi = square_4326(), trange = c("2024-01-01", "2024-01-01"), token = "t", page_size = 500)
  expect_length(calls, 1)
  expect_equal(calls[[1]]$option, "NDVI")
  expect_equal(calls[[1]]$page_size, 500)
  expect_match(calls[[1]]$params[["geometry"]], "^POLYGON")
  expect_equal(calls[[1]]$params[["epsg"]], "4326")
  expect_equal(calls[[1]]$params[["date"]], "20240101")
  expect_equal(out$fieldid, 1:2)                                   # in order of date and field
})

test_that("no data gives a table without rows", {
  local_mocked_bindings(adc_get = function(...) list(features = list()))
  out <- adc_ndvi(fieldid = 1, trange = c("2024-01-01", "2024-01-02"), token = "t")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("fieldid", "date", "ndvi_avg", "ndvi_stddev"))
})

test_that("the arguments are checked", {
  expect_error(adc_ndvi(trange = c("2024-01-01", "2024-01-02"), token = "t"), "either `roi`")
  expect_error(adc_ndvi(roi = square_4326(), fieldid = 1, trange = c("2024-01-01", "2024-01-02"), token = "t"), "either `roi`")
  expect_error(adc_ndvi(fieldid = 1, trange = c("2024-01-01", "2024-01-02"), token = ""), "ADC_TOKEN")
  expect_error(adc_ndvi(fieldid = 1, trange = c("2024-01-01", NA), token = "t"), "first and a last date")
  long <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = 5, ymin = 52, xmax = 5.1, ymax = 52.1), crs = 4326)))
  long <- sf::st_buffer(sf::st_transform(long, 28992), 100, nQuadSegs = 2000)
  expect_error(adc_ndvi(roi = long, trange = c("2024-01-01", "2024-01-02"), token = "t"), "too long for the AgroDataCube")
})

test_that("the requests are reported one by one", {
  local_mocked_bindings(adc_get = function(...) list(features = list()))
  seen <- list()
  suppressMessages(ndc_with_progress(
    adc_ndvi(fieldid = 7, trange = c("2024-01-01", "2024-01-03"), token = "t"),
    report = function(message, current, total) seen[[length(seen) + 1]] <<- list(message, current, total)
  ))
  expect_length(seen, 3)
  expect_equal(seen[[2]], list("Requesting NDVI 2024-01-02 of field 7 (2/3)", 2L, 3L))
})
