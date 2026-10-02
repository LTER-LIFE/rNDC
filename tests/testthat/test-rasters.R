mock_download <- function(href, outfile, headers, overwrite = TRUE, ...) {
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                   crs = "EPSG:4326", vals = 1:16)
  terra::writeRaster(r, outfile, overwrite = TRUE)
  outfile
}

nitrogen_items <- function() {
  list(stac_item("a", layer = "ntot", date = "2024-01-01"),
       stac_item("b", layer = "nox", date = "2024-01-01"),
       stac_item("c", layer = "ntot", date = "2025-01-01"))
}

test_that("stac_collect_metadata reads items from the STAC API", {
  local_stac_api(nitrogen_items())
  m <- stac_collect_metadata(c(5, 52, 6, 53), token = "t", endpoint = ndc_endpoint(), collection = "coll")
  expect_equal(m$id, c("a", "b", "c"))
  expect_equal(m$layer, c("ntot", "nox", "ntot"))
  expect_equal(m$year, c("2024", "2024", "2025"))
  expect_equal(m$href, paste0("https://example.org/data/", c("a", "b", "c")))
  expect_equal(last_request_body()$collections, list("coll"))
})

test_that("stac_collect_metadata returns an empty tibble when nothing matches", {
  local_stac_api(list())
  m <- stac_collect_metadata(c(5, 52, 6, 53), token = "t", endpoint = ndc_endpoint(), collection = "coll")
  expect_equal(nrow(m), 0)
})

test_that("stac_collect_metadata fails with a rejected token", {
  local_stac_api(nitrogen_items())
  expect_error(stac_collect_metadata(c(5, 52, 6, 53), token = "wrong", endpoint = ndc_endpoint(),
                                     collection = "coll"))
})

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
test_that("get_nitrogen_raster downloads only the AOI subset and creates out_dir", {
  local_stac_api(nitrogen_items())
  urls <- character()
  local_mocked_bindings(
    stac_download_one = function(href, ...) { urls <<- c(urls, href); mock_download(href, ...) }
  )
  out_dir <- file.path(withr::local_tempdir(), "new", "dir")
  res <- get_nitrogen_raster(square_4326(), 2024, token = "t", out_dir = out_dir)
  expect_equal(names(res$stack), c("ntot_2024", "nox_2024"))
  expect_true(all(file.exists(unlist(res$files))))
  expect_match(urls, "^https://example.org/data/[ab]&subset=E\\(")
})

test_that("get_nitrogen_raster validates its input", {
  expect_error(get_nitrogen_raster(square_4326(), year = "", token = "t"), "select a year")
})

test_that("land use items are filtered by keyword and year", {
  items <- list(stac_item("a", date = "2024-01-01", keywords = list("lgn")),
                stac_item("b", date = "2024-01-01", keywords = list("other")),
                stac_item("c", date = "2023-01-01", keywords = list("lgn")))
  local_stac_api(items)
  m <- landuse_collect_metadata(c(5, 52, 6, 53), "t")
  expect_equal(m$id, c("c", "a"))
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

test_that("stac_download_one retries on HTTP errors and then fails", {
  withr::defer({ webmockr::stub_registry_clear(); webmockr::disable(adapter = "httr", quiet = TRUE) })
  webmockr::enable(adapter = "httr", quiet = TRUE)
  webmockr::stub_request("get", "https://example.org/data/missing") |>
    webmockr::to_return(status = 404, body = "nope")
  expect_error(stac_download_one("https://example.org/data/missing", file.path(withr::local_tempdir(), "x.tif"),
                                 stac_make_headers("t"), retries = 1L),
               "HTTP 404")
})

test_that("stac_download_one sends the token and returns the output path", {
  withr::defer({ webmockr::stub_registry_clear(); webmockr::disable(adapter = "httr", quiet = TRUE) })
  webmockr::enable(adapter = "httr", quiet = TRUE)
  webmockr::stub_request("get", "https://example.org/data/ok") |>
    webmockr::wi_th(headers = list(Authorization = "Bearer t")) |>
    webmockr::to_return(status = 200, body = "data")
  out <- file.path(withr::local_tempdir(), "x.tif")
  expect_equal(stac_download_one("https://example.org/data/ok", out, stac_make_headers("t"), retries = 1L), out)
})
