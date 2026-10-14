# ndc_nitrogen_raster() and the metadata of the nitrogen items.

test_that("nitrogen metadata is filtered by layer and year", {
  local_stac_api(nitrogen_items())
  aoi <- c(5, 52, 6, 53)
  expect_equal(nitrogen_collect_metadata(aoi, "t", year = "2024")$id, c("a", "b"))
  expect_equal(last_request_body()$collections, list("nh3"))  # one search per layer collection
  expect_equal(nitrogen_collect_metadata(aoi, "t", layers = "ntot")$id, c("a", "c"))
  expect_error(nitrogen_collect_metadata(aoi, "t", year = "2040"), "matched")
})

test_that("nitrogen metadata errors when no items are found", {
  local_stac_api(list())
  expect_error(nitrogen_collect_metadata(c(5, 52, 6, 53), "t"), "No Nature Data Cube raster items")
})

# File downloads cannot be simulated with webmockr (it does not write `write_disk()` targets),
# so the download step is replaced while the STAC search still goes through the stubbed API.
test_that("ndc_nitrogen_raster downloads only the AOI subset and creates output_dir", {
  local_stac_api(nitrogen_items())
  urls <- character()
  local_mocked_bindings(
    stac_download_one = function(href, ...) { urls <<- c(urls, href); mock_download(href, ...) }
  )
  output_dir <- file.path(withr::local_tempdir(), "new", "dir")
  res <- ndc_nitrogen_raster(square_4326(), 2024, token = "t", output_dir = output_dir)
  expect_equal(names(res$stack), c("ntot_2024", "nox_2024"))
  expect_true(all(file.exists(unlist(res$files))))
  expect_match(urls, "^https://example.org/data/[ab]&subset=E\\(")
})

test_that("ndc_nitrogen_raster validates its input", {
  expect_error(ndc_nitrogen_raster(square_4326(), year = "", token = "t"), "select a year")
})
