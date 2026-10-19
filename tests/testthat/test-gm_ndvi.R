# The monthly mean NDVI from GroenMonitor: the rasters hold digital numbers, and NDVI is number / 250.

test_that("the mean of a month is NDVI by default, and the digital numbers with scale = FALSE", {
  testthat::local_mocked_bindings(gm_get = fake_ndvi_download())
  poly <- square_4326(0.1, 0.1, 0.9, 0.9)
  numbers <- suppressMessages(gm_ndvi_month(poly, 2024, 2, scale = FALSE))
  ndvi <- suppressMessages(gm_ndvi_month(poly, 2024, 2))
  expect_equal(names(ndvi), "ndvi_mean_202402")
  expect_equal(terra::values(numbers)[, 1], c(0.2, 0.4, 0.6, 0.8), tolerance = 1e-6)   # the cells of the fake download
  expect_equal(terra::values(ndvi)[, 1], terra::values(numbers)[, 1] / 250)

  stack <- suppressMessages(gm_ndvi_stack(poly, c("2024-01-01", "2024-02-29")))
  expect_equal(terra::values(stack)[, 1], terra::values(numbers)[, 1] / 250)
  numbers_stack <- suppressMessages(gm_ndvi_stack(poly, c("2024-01-01", "2024-02-29"), scale = FALSE))
  expect_equal(terra::values(numbers_stack)[, 2], terra::values(numbers)[, 1])
})
