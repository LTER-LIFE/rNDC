# gm_url(): the URLs of the GroenMonitor WCS server.

test_that("gm_url composes a WCS GetCoverage URL", {
  u <- gm_url("NDVI", params = c(date = "20240101", xmin = "1", xmax = "2",
                                 ymin = "3", ymax = "4", format = "tiff"))
  expect_match(u, "request=GetCoverage", fixed = TRUE)
  expect_match(u, "coverageId=groenmonitor__ndvi_20240101", fixed = TRUE)
  expect_match(u, "subset=E(1,2)", fixed = TRUE)
  expect_match(u, "subset=N(3,4)", fixed = TRUE)
  expect_match(u, "format=image/tiff", fixed = TRUE)
})
