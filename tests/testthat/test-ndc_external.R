# The datasets of other services and the outline of the Netherlands.

test_that("the external datasets are described", {
  ext <- ndc_external_datasets()
  expect_s3_class(ext, "tbl_df")
  expect_true(all(c("weather", "soil-map", "ahn", "agricultural-fields", "groenmonitor-ndvi") %in% ext$id))
  expect_s3_class(ext$start, "Date")
  expect_equal(ext$start[ext$id == "weather"], as.Date("1970-01-01"))
  expect_equal(ext$start[ext$id == "agricultural-fields"], as.Date("2009-01-01"))
  expect_equal(ext$start[ext$id == "groenmonitor-ndvi"], as.Date("2017-05-26"))
  expect_true(is.na(ext$start[ext$id == "ahn"]))
  expect_equal(ext$stac_collection[ext$id == "groenmonitor-ndvi"], "ndvi")
})

test_that("the outline of the Netherlands is bundled", {
  nl <- ndc_netherlands()
  expect_s3_class(nl, "sf")
  expect_equal(sf::st_crs(nl)$epsg, 28992L)
})
