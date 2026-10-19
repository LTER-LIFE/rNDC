# The soil map raster of the NatureDataCube, and its legend.

test_that("the legend gives the code, name and colour of each soil type", {
  local_stac_api(list(soil_item()))
  local_legend()
  legend <- ndc_soil_legend(token = "t")
  expect_equal(legend, tibble::tibble(code = c(1L, 14L), label = c("Veengronden", "Leemgronden"), color = c("#1920A7", "#FE021E")))
})

test_that("the raster is named after the year, with the legend", {
  local_stac_api(list(soil_item(), soil_item("2020-01-01")))
  local_legend()
  urls <- new.env()
  local_mocked_bindings(stac_download_one = mock_height_download(urls))
  res <- ndc_soil_raster(square_4326(), year = 2025, token = "t", output_dir = withr::local_tempdir())
  expect_equal(names(res$rasters), "SOIL_2025")
  expect_equal(res$metadata$id, "soil-2025")
  expect_match(urls$all, "^https://example.org/data/soil-2025&subset=")
  expect_equal(max(terra::values(res$rasters[[1]]), na.rm = TRUE), 16)       # the codes are not scaled
  expect_equal(res$legend$code, c(1L, 14L))
  expect_equal(names(ndc_soil_raster(square_4326(), token = "t", output_dir = withr::local_tempdir())$rasters),
               c("SOIL_2020", "SOIL_2025"))
  expect_error(ndc_soil_raster(square_4326(), year = 1999, token = "t"), "year 1999|year \\(1999\\)")
})

test_that("a legend that cannot be read does not stop the raster", {
  local_stac_api(list(soil_item()))
  local_mocked_bindings(stac_download_one = mock_height_download())
  webmockr::stub_request("get", "https://example.org/geoserver/ows?service=WMS&layer=ndc%3Asoil&request=GetLegendGraphic&format=application%2Fjson&width=20") |>
    webmockr::to_return(status = 500)
  res <- ndc_soil_raster(square_4326(), token = "t", output_dir = withr::local_tempdir())
  expect_null(res$legend)
  expect_s4_class(res$stack, "SpatRaster")
})

test_that("no items, no raster", {
  local_stac_api(list())
  expect_error(ndc_soil_raster(square_4326(), token = "t"), "No soil map items were found")
  expect_equal(ndc_soil_collection(), "soiltypes")
})
