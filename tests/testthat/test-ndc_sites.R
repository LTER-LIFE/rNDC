# ndc_sites(): the bundled study sites.

test_that("ndc_sites lists and loads study sites", {
  layers <- ndc_sites()
  expect_type(layers, "character")
  expect_s3_class(ndc_sites(layers[1]), "sf")
  expect_error(ndc_sites("nope"), "not found")
})
