# get_landuse_raster() and the metadata of the land use items.

test_that("land use items are filtered by year", {
  items <- list(stac_item("a", date = "2024-01-01", keywords = list("lgn")),
                stac_item("c", date = "2023-01-01", keywords = list("lgn")))
  local_stac_api(items)
  m <- landuse_collect_metadata(c(5, 52, 6, 53), "t")
  expect_equal(m$id, c("c", "a"))  # in date order
  expect_s3_class(m$obs_date, "Date")

  urls <- character()
  local_mocked_bindings(
    stac_download_one = function(href, ...) { urls <<- c(urls, href); mock_download(href, ...) }
  )
  res <- get_landuse_raster(square_4326(), year = 2024, token = "t", out_dir = withr::local_tempdir(),
                             min_file_size = 0)
  expect_equal(names(res$stack), "LGN_2024")
  expect_match(urls, "^https://example.org/data/a&subset=")
  expect_error(get_landuse_raster(square_4326(), year = 1999, token = "t"), "year 1999")
})
