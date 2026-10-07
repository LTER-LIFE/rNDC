# Monthly NDVI statistics from the STAC NDVI collections.

ndvi_items <- function() {
  list(ndvi_item("a", "1", "2024-05-01", 0.5, 0.1), ndvi_item("b", "1", "2024-05-14", 0.7, 0.3),
       ndvi_item("c", "1", "2024-06-02", 0.8, 0.2),
       ndvi_item("d", "2", "2024-05-02", 0.9, 0.5))  # a neighbouring polygon
}

square <- function(...) {
  sf::st_sf(..., geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = 0.1, ymin = 0.1, xmax = 0.9, ymax = 0.9),
                                                       crs = sf::st_crs(4326))))
}

test_that("NDVI statistics are averaged per month for the polygon of the area only", {
  local_stac_api(items = ndvi_items())
  res <- get_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", from = "2024-05-01", to = "2024-06-30", token = "t")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("ndc_id", "month", "ndvi_mean", "ndvi_std", "n_obs"))
  expect_equal(res$ndc_id, c("1", "1"))
  expect_equal(res$month, c("2024-05", "2024-06"))
  expect_equal(res$ndvi_mean, c(0.6, 0.8))
  expect_equal(res$ndvi_std, c(0.2, 0.2))
  expect_equal(res$n_obs, c(2L, 1L))
})

test_that("without ndc_id the neighbouring polygons are kept; ndc_id can also be given", {
  local_stac_api(items = ndvi_items())
  res <- get_ndvi_stats(square(), "ndvi-lter", token = "t")
  expect_equal(res$ndc_id, c("1", "2", "1"))  # per month, then polygon
  expect_equal(res$month, c("2024-05", "2024-05", "2024-06"))

  res <- get_ndvi_stats(square(), "ndvi-lter", ndc_id = "2", token = "t")
  expect_equal(res$ndc_id, "2")
  # an explicit ndc_id wins over the column of the area
  res <- get_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", ndc_id = "2", token = "t")
  expect_equal(res$ndc_id, "2")
})

test_that("the period covers the whole last day, and can be open on a side", {
  local_stac_api(items = ndvi_items())
  get_ndvi_stats(square(), "ndvi-lter", from = "2024-05-01", to = "2024-05-31", token = "t")
  expect_equal(last_request_body()$datetime, "2024-05-01T00:00:00Z/2024-05-31T23:59:59Z")

  get_ndvi_stats(square(), "ndvi-lter", from = as.Date("2024-05-01"), token = "t")
  expect_equal(last_request_body()$datetime, "2024-05-01T00:00:00Z/..")

  get_ndvi_stats(square(), "ndvi-lter", token = "t")
  expect_null(last_request_body()$datetime)
})

test_that("all pages of the search are used", {
  local_stac_api(pages = list(ndvi_items()[1:2], ndvi_items()[3:4]))
  res <- get_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", token = "t")
  expect_equal(res$n_obs, c(2L, 1L))
})

test_that("a polygon without observations gives an empty table", {
  local_stac_api(items = ndvi_items())
  res <- get_ndvi_stats(square(ndc_id = "999"), "ndvi-lter", token = "t")
  expect_equal(nrow(res), 0)
  expect_named(res, c("ndc_id", "month", "ndvi_mean", "ndvi_std", "n_obs"))

  local_stac_api(items = list())
  expect_equal(nrow(get_ndvi_stats(square(), "ndvi-lter", token = "t")), 0)
})

test_that("the standard deviation is left out when the collection does not have it", {
  local_stac_api(items = list(ndvi_item("a", "1", "2024-05-01", 0.5), ndvi_item("b", "1", "2024-05-02", 0.7)))
  res <- get_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", token = "t")
  expect_named(res, c("ndc_id", "month", "ndvi_mean", "n_obs"))
  expect_equal(res$ndvi_mean, 0.6)
})

test_that("unexpected fields and API errors are reported", {
  local_stac_api(items = list(stac_feature("a", list(ndc_id = "1", ndvi_mean = 0.5))))
  expect_error(get_ndvi_stats(square(ndc_id = "1"), "ndvi-lter", token = "t"), "observation_date")

  local_stac_api(items = list(), status = 500L)
  expect_error(get_ndvi_stats(square(), "ndvi-lter", token = "t"))
})
