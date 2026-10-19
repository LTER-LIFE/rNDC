# The AHN rasters (terrain and object height) of the NatureDataCube, and their statistics.

test_that("the items are chosen by layer and year", {
  local_stac_api(c(ahn_items(), list(height_item("dtm", "2014-01-01"))))
  expect_equal(ahn_collect_metadata(c(5, 52, 6, 53), "t")$id, c("ahn-dtm-2014", "ahn-dtm-2019", "ahn-ohn-2022"))
  expect_equal(ahn_collect_metadata(c(5, 52, 6, 53), "t", layers = "ohn")$year, "2022")
  expect_equal(ahn_collect_metadata(c(5, 52, 6, 53), "t", year = 2014)$id, "ahn-dtm-2014")
  expect_error(ahn_collect_metadata(c(5, 52, 6, 53), "t", year = 1999), "year \\(1999\\)")
  expect_error(ahn_collect_metadata(c(5, 52, 6, 53), "t", layers = "dtm", year = 2022), "layer\\(s\\) \\(dtm\\) and year \\(2022\\)")

  local_stac_api(list())
  expect_error(ahn_collect_metadata(c(5, 52, 6, 53), "t"), "No AHN raster items were found")
})

test_that("the rasters are named after layer and year, in metres, clipped to the area", {
  local_stac_api(ahn_items())
  urls <- new.env()
  local_mocked_bindings(stac_download_one = mock_height_download(urls))
  res <- ndc_ahn_raster(square_4326(), token = "t", max_cells = Inf, output_dir = withr::local_tempdir())
  expect_equal(names(res$rasters), c("AHN_DTM_2019", "AHN_OHN_2022"))
  expect_equal(names(res$files), names(res$rasters))
  expect_equal(res$metadata$layer, c("dtm", "ohn"))
  expect_true(all(grepl("^https://example.org/data/ahn-.*&subset=E\\(.*&subset=N\\(", urls$all)))
  expect_false(any(grepl("scalefactor", urls$all)))
  expect_equal(max(terra::values(res$rasters[[1]]), na.rm = TRUE), 16 / 100)            # centimetres to metres
  expect_equal(unname(unlist(lapply(res$files, function(f) terra::global(terra::rast(f), "max")[[1]]))), c(16, 16))
  expect_equal(max(terra::values(ndc_ahn_raster(square_4326(), "dtm", token = "t", scale = FALSE, max_cells = Inf,
                                                 output_dir = withr::local_tempdir())$rasters[[1]]), na.rm = TRUE), 16)
  expect_true(terra::nlyr(res$stack) == 2)                                                # the same grid in this test
})

test_that("rasters of different resolutions are not stacked", {
  local_stac_api(ahn_items())
  local_mocked_bindings(stac_download_one = mock_height_download(rows = function(href) if (grepl("ohn", href)) 8L else 4L))
  res <- ndc_ahn_raster(square_4326(), token = "t", max_cells = Inf, output_dir = withr::local_tempdir())
  expect_null(res$stack)
  expect_length(res$rasters, 2)
  one <- ndc_ahn_raster(square_4326(), layers = "dtm", token = "t", max_cells = Inf, output_dir = withr::local_tempdir())
  expect_s4_class(one$stack, "SpatRaster")
})

test_that("a large area is requested at a lower resolution, or at the one that is asked for", {
  local_stac_api(ahn_items())
  urls <- new.env()
  local_mocked_bindings(stac_download_one = mock_height_download(urls))
  area <- square_4326()                                              # about 90 x 90 km
  expect_message(ndc_ahn_raster(area, layers = "ohn", token = "t", max_cells = 1e6, output_dir = withr::local_tempdir()),
                 "cells of about")
  expect_match(urls$all, "&scalefactor=0\\.0[0-9]+$")

  urls$all <- character()
  expect_silent(ndc_ahn_raster(area, layers = "ohn", token = "t", scalefactor = 0.5, output_dir = withr::local_tempdir()))
  expect_match(urls$all, "&scalefactor=0.5$")
  expect_error(ndc_ahn_raster(area, token = "t", scalefactor = 2), "above 0 and up to 1")
  expect_error(ndc_ahn_raster(area, token = "t", scalefactor = 0), "above 0 and up to 1")
})

test_that("the layers are checked", {
  expect_error(ndc_ahn_raster(square_4326(), layers = "xyz", token = "t"), "one or more of: dtm, ohn \\(not known: xyz\\)")
  expect_error(ndc_ahn_raster(square_4326(), layers = character(), token = "t"), "one or more of")
  expect_error(ndc_ahn_raster(square_4326(), token = ""), "NDC_TOKEN")
  expect_equal(ndc_ahn_layers(), c("dtm", "ohn"))
  expect_equal(ndc_ahn_collection(), "ahnmetrics")
})

test_that("the statistics are those of the cells inside the area, and the downloads are removed", {
  local_stac_api(ahn_items())
  local_mocked_bindings(stac_download_one = mock_height_download())
  before <- list.files(tempdir(), "^ahn_stats_")
  stats <- ndc_ahn_stats(square_4326(0, 0, 1, 1), token = "t", max_cells = Inf)
  expect_identical(list.files(tempdir(), "^ahn_stats_"), before)
  expect_s3_class(stats, "tbl_df")
  expect_equal(names(stats), c("layer", "year", "area", "min", "max", "mean", "sd", "n_cells"))
  expect_equal(stats$layer, c("dtm", "ohn"))
  expect_equal(stats$year, c("2019", "2022"))
  expect_equal(stats$min[1], 1 / 100)
  expect_equal(stats$max[1], 16 / 100)
  expect_equal(stats$mean[1], mean(1:16) / 100)
  expect_equal(stats$sd[1], stats::sd(1:16) / 100)
  expect_equal(stats$n_cells[1], 16)
  expect_equal(stats$area[1], 16 * 0.25^2)                         # cells of 0.25 degrees in this test

  raw <- ndc_ahn_stats(square_4326(0, 0, 1, 1), layers = "dtm", token = "t", scale = FALSE, max_cells = Inf)
  expect_equal(raw$max, 16)
  expect_equal(nrow(raw), 1)
})

test_that("the downloads are reported one by one", {
  local_stac_api(ahn_items())
  local_mocked_bindings(stac_download_one = mock_height_download())
  seen <- list()
  suppressMessages(ndc_with_progress(
    ndc_ahn_raster(square_4326(), token = "t", max_cells = Inf, output_dir = withr::local_tempdir()),
    report = function(message, current, total) seen[[length(seen) + 1]] <<- list(message, current, total)
  ))
  expect_equal(seen[[2]], list("Downloading AHN_OHN_2022 (2/2)", 2L, 2L))
})
