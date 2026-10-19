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

test_that("the cell size is read from the title of an item", {
  expect_equal(stac_title_resolution(c("AHN3 Terrain Height (DTM) 2019 (5m)", "Objecthoogte (OHN, AHN4) 2022 (2.5m)",
                                       "Soil Map 2025 (5m, UTM31N)", "Land use")), c(5, 2.5, 5, NA))
})

test_that("the scale factor keeps a request under the most cells", {
  area <- square_4326(5, 52, 5.1, 52.1)                                # about 7 x 11 km
  expect_equal(stac_scalefactor(area, 5, 32631, max_cells = 1e8), 1)
  expect_message(f <- stac_scalefactor(area, 5, 32631, max_cells = 1e6), "cells of about")
  expect_lt(f, 1)
  expect_equal(stac_scalefactor(area, NA, 32631, max_cells = 1), 1)    # no cell size known: as it is
  expect_equal(stac_scalefactor(area, 5, 32631, scalefactor = 0.3, max_cells = 1), 0.3)
  expect_error(stac_scalefactor(area, 5, 32631, scalefactor = 1.5), "above 0")
})
