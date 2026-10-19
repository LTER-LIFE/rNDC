# NDVI rasters through the NatureDataCube: the items of the `ndvi` collection are the daily coverages.

local_ndvi_download <- function(env = parent.frame()) {
  urls <- new.env()
  urls$all <- character()
  testthat::local_mocked_bindings(
    stac_download_one = function(href, ...) { urls$all <- c(urls$all, href); mock_download(href, ...) },
    .env = env
  )
  urls
}

test_that("a month is the mean of the days of the items, asked with the subset of the area", {
  local_stac_api(ndvi_stac_items())
  urls <- local_ndvi_download()
  r <- suppressMessages(ndc_ndvi_month(square_4326(), 2024, 2, token = "t"))
  expect_s4_class(r, "SpatRaster")
  expect_equal(names(r), "ndvi_mean_202402")
  expect_equal(terra::nlyr(r), 1)
  expect_length(urls$all, 2)                                    # the item of March is not downloaded
  expect_true(all(grepl("^https://example.org/data/gm-ndvi_2024020[3-9]|^https://example.org/data/gm-ndvi_20240210", urls$all)))
  expect_true(all(grepl("&subset=E\\(.*&subset=N\\(", urls$all)))
})

test_that("the mean is NDVI (the numbers divided by 250) unless scale = FALSE", {
  local_stac_api(ndvi_stac_items())
  local_ndvi_download()
  numbers <- suppressMessages(ndc_ndvi_month(square_4326(), 2024, 2, token = "t", scale = FALSE))
  ndvi <- suppressMessages(ndc_ndvi_month(square_4326(), 2024, 2, token = "t"))
  expect_equal(terra::values(numbers)[, 1], as.numeric(1:16))              # the cells of the fake download
  expect_equal(terra::values(ndvi)[, 1], as.numeric(1:16) / 250)
  stack <- suppressMessages(ndc_ndvi_stack(square_4326(), c("2024-02-01", "2024-03-31"), token = "t"))
  expect_equal(terra::values(stack)[, 2], as.numeric(1:16) / 250)
})

test_that("a stack has a layer for each month with data, and reports each item", {
  local_stac_api(ndvi_stac_items())
  local_ndvi_download()
  seen <- list()
  r <- suppressMessages(ndc_with_progress(
    ndc_ndvi_stack(square_4326(), c("2024-01-15", "2024-03-20"), token = "t"),
    report = function(message, current, total) seen[[length(seen) + 1]] <<- list(message, current, total)
  ))
  expect_equal(names(r), c("ndvi_mean_202402", "ndvi_mean_202403"))   # January has no item
  expect_length(seen, 3)
  expect_equal(seen[[1]], list("Downloading NDVI 2024-02-03 (1/3)", 1L, 3L))
  expect_equal(seen[[3]], list("Downloading NDVI 2024-03-05 (3/3)", 3L, 3L))
})

test_that("a period without items gives NULL, and the search is limited to the period", {
  local_stac_api(list())
  expect_null(suppressMessages(ndc_ndvi_stack(square_4326(), c("2024-01-01", "2024-01-31"), token = "t")))
  body <- last_request_body()
  expect_equal(body$datetime, "2024-01-01T00:00:00Z/2024-01-31T23:59:59Z")
  expect_equal(body$collections[[1]], "ndvi")
})

test_that("clip = TRUE masks the cells outside the area", {
  local_stac_api(ndvi_stac_items())
  local_ndvi_download()
  area <- sf::st_as_sf(sf::st_as_sfc("POLYGON((0.1 0.1, 0.9 0.1, 0.5 0.9, 0.1 0.1))", crs = 4326))
  whole <- suppressMessages(ndc_ndvi_month(area, 2024, 2, token = "t"))
  clipped <- suppressMessages(ndc_ndvi_month(area, 2024, 2, token = "t", clip = TRUE))
  expect_gt(sum(is.na(terra::values(clipped))), sum(is.na(terra::values(whole))))
})

test_that("the daily files are removed when the download is stopped", {
  local_stac_api(ndvi_stac_items())
  local_ndvi_download()
  before <- list.files(tempdir(), "^ndvi_stac_")
  expect_error(
    suppressMessages(ndc_with_progress(ndc_ndvi_stack(square_4326(), c("2024-02-01", "2024-03-31"), token = "t"),
                                       interrupt = function() TRUE)),
    class = "rNDC_interrupted"
  )
  expect_identical(list.files(tempdir(), "^ndvi_stac_"), before)
})

test_that("a missing token and a period without dates are refused", {
  local_stac_api(ndvi_stac_items())
  expect_error(ndc_ndvi_stack(square_4326(), c("2024-02-01", "2024-03-31"), token = ""), "NDC_TOKEN")
  expect_error(ndc_ndvi_stack(square_4326(), c("2024-02-01", NA), token = "t"), "first and a last date")
})
