# ndc_endpoint(): the STAC endpoint.

test_that("ndc_endpoint can be overridden by an option", {
  expect_match(ndc_endpoint(), "^https://")
  withr::local_options(rNDC.endpoint = "https://example.org/api/")
  expect_equal(ndc_endpoint(), "https://example.org/api/")
})
