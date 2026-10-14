# Defaults and available years of the land use and nitrogen rasters.

test_that("the defaults of the rasters are available", {
  expect_equal(ndc_landuse_collection(), "lgn")
  expect_equal(ndc_landuse_default_year(), 2024L)
  expect_equal(ndc_nitrogen_layers(), c("ntot", "nox", "nh3"))
})

raster_item <- function(id, date) stac_item(id, layer = "ntot", date = date)

test_that("the years come from the items of the collections, sorted and without duplicates", {
  local_stac_api(items = list(raster_item("a", "2025-01-01"), raster_item("b", "2024-01-01"),
                              raster_item("c", "2040-01-01"), raster_item("d", "2024-06-01")))
  expect_equal(ndc_nitrogen_years(token = "t"), c("2024", "2025", "2040"))
  expect_equal(ndc_landuse_years(token = "t"), c("2024", "2025", "2040"))
})

test_that("all pages count", {
  local_stac_api(pages = list(list(raster_item("a", "2024-01-01")), list(raster_item("b", "2030-01-01"))))
  expect_equal(ndc_landuse_years(token = "t"), c("2024", "2030"))
})

test_that("the years are asked per collection", {
  local_stac_api(items = list(raster_item("a", "2024-01-01")))
  ndc_landuse_years(token = "t")
  expect_equal(unlist(last_request_body()$collections), "lgn")

  ndc_nitrogen_years(token = "t")
  expect_equal(unlist(last_request_body()$collections), "nh3")  # the last of ntot, nox, nh3
})

test_that("no items give no years, and API errors are passed on", {
  local_stac_api(items = list())
  expect_equal(ndc_landuse_years(token = "t"), character(0))

  local_stac_api(items = list(), status = 500L)
  expect_error(ndc_nitrogen_years(token = "t"))
})
