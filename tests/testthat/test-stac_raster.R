# The STAC helpers for downloaded rasters.

test_that("file prefixes are sanitised", {
  expect_equal(stac_make_file_prefix(NULL), "")
  expect_equal(stac_make_file_prefix("my site/1"), "my_site_1_")
})

test_that("rasters are stacked", {
  r <- terra::rast(nrows = 2, ncols = 2)
  expect_null(stac_build_stack(list()))
  expect_equal(terra::nlyr(stac_build_stack(list(a = r))), 1)
  expect_equal(terra::nlyr(stac_build_stack(list(a = r, b = r))), 2)
})
