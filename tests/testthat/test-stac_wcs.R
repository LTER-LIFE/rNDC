# The STAC helpers for the WCS assets: the subset of an area, and the download.

test_that("WCS subset suffix is built in the target CRS", {
  s <- stac_wcs_subset_suffix(c(5, 52, 6, 53), target_crs = 32631)
  expect_match(s, "^&subset=E\\([0-9.]+,[0-9.]+\\)&subset=N\\([0-9.]+,[0-9.]+\\)$")
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
