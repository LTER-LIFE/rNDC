# adc_url(): the URLs of the AgroDataCube REST API.

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

test_that("adc_url encodes reserved characters in values and rejects unknown options", {
  u <- adc_url("Fields", params = c(geometry = "a&b=c/d", page_size = "10"))
  expect_equal(u, "https://agrodatacube.wur.nl/api/v2/rest/fields?geometry=a%26b%3Dc%2Fd&page_size=10")
  expect_error(adc_url("Nope", params = c(a = "1")), "Unknown `option`")
})
