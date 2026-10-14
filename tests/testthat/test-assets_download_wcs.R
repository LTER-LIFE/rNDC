# assets_download_wcs(): the assets that come from WCS servers.

test_that("assets_download_wcs requests the asset URLs", {
  local_stac_api()
  webmockr::stub_request("get", "https://example.org/data/a") |>
    webmockr::to_return(body = "x", status = 200)
  items <- ndc_get("coll", token = "t")
  res <- assets_download_wcs(items, asset_names = "wcs", output_dir = withr::local_tempdir())
  expect_equal(httr::status_code(res), 200L)
  expect_equal(res$url, "https://example.org/data/a")
})

test_that("assets_download_wcs downloads several assets and checks every response", {
  local_stac_api(list(stac_item("a"), stac_item("b")))
  items <- ndc_get("coll", token = "t")
  webmockr::stub_request("get", "https://example.org/data/a") |> webmockr::to_return(status = 200, body = "x")
  webmockr::stub_request("get", "https://example.org/data/b") |> webmockr::to_return(status = 200, body = "y")
  res <- assets_download_wcs(items, output_dir = file.path(withr::local_tempdir(), "new", "dir"))
  expect_length(res, 2)
  expect_true(all(vapply(res, inherits, NA, "response")))

  local_stac_api(list(stac_item("c")))
  items <- ndc_get("coll", token = "t")
  webmockr::stub_request("get", "https://example.org/data/c") |> webmockr::to_return(status = 404)
  out <- file.path(withr::local_tempdir(), "o")
  expect_error(assets_download_wcs(items, output_dir = out), "404")
  expect_false(file.exists(file.path(out, "c.tif")))
})
