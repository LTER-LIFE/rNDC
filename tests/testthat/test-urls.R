test_that("adc_url composes request URLs", {
  expect_equal(adc_url("Health_check", params = NULL), "https://agrodatacube.wur.nl/api/v2/rest/lifeprobe")
  u <- adc_url("Fields", params = c(output_epsg = "4326", page_size = "10"))
  expect_equal(u, "https://agrodatacube.wur.nl/api/v2/rest/fields?output_epsg=4326&page_size=10")
})

test_that("adc_url moves fieldid and meteostation into the path", {
  u <- adc_url("NDVI", params = c(fieldid = "123", year = "2020"))
  expect_equal(u, "https://agrodatacube.wur.nl/api/v2/rest/fields/123/ndvi?year=2020")
  u <- adc_url("Meteo_stations", params = c(meteostation = "260", output_epsg = "4326"))
  expect_equal(u, "https://agrodatacube.wur.nl/api/v2/rest/meteostations/260?output_epsg=4326")
})

test_that("gm_url composes a WCS GetCoverage URL", {
  u <- gm_url("NDVI", params = c(date = "20240101", xmin = "1", xmax = "2",
                                 ymin = "3", ymax = "4", format = "tiff"))
  expect_match(u, "request=GetCoverage", fixed = TRUE)
  expect_match(u, "coverageId=groenmonitor__ndvi_20240101", fixed = TRUE)
  expect_match(u, "subset=E(1,2)", fixed = TRUE)
  expect_match(u, "subset=N(3,4)", fixed = TRUE)
  expect_match(u, "format=image/tiff", fixed = TRUE)
})

test_that("ndc_endpoint can be overridden by an option", {
  expect_match(ndc_endpoint(), "^https://")
  withr::local_options(rNDC.endpoint = "https://example.org/api/")
  expect_equal(ndc_endpoint(), "https://example.org/api/")
})
