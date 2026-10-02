test_that("keywords are normalised", {
  expect_equal(stac_keywords_to_vec(NULL), character(0))
  expect_equal(stac_keywords_to_vec(" LGN, Land Use "), c("lgn", "land use"))
  expect_equal(stac_keywords_to_vec(list("A", "a", "")), "a")
})

test_that("file prefixes are sanitised", {
  expect_equal(stac_make_file_prefix(NULL), "")
  expect_equal(stac_make_file_prefix("my site/1"), "my_site_1_")
})

test_that("WCS subset suffix is built in the target CRS", {
  s <- stac_wcs_subset_suffix(c(5, 52, 6, 53), target_crs = 32631)
  expect_match(s, "^&subset=E\\([0-9.]+,[0-9.]+\\)&subset=N\\([0-9.]+,[0-9.]+\\)$")
})

test_that("STAC features are summarised", {
  feat <- list(id = "x",
               properties = list(`ndc:observation_date` = "2024-05-01", `ndc:layer_type` = "ntot",
                                 title = "T", keywords = list("A", "B")),
               assets = list(wcs = list(href = "http://h/x")))
  m <- stac_feature_meta(feat)
  expect_equal(m$id, "x")
  expect_equal(m$layer, "ntot")
  expect_equal(m$year, "2024")
  expect_equal(m$keywords[[1]], c("a", "b"))
  expect_equal(m$href, "http://h/x")
})

test_that("headers require a token", {
  expect_error(stac_make_headers(""), "token is missing")
  expect_s3_class(stac_make_headers("abc"), "request")
})

test_that("rasters are stacked", {
  r <- terra::rast(nrows = 2, ncols = 2)
  expect_null(stac_build_stack(list()))
  expect_equal(terra::nlyr(stac_build_stack(list(a = r))), 1)
  expect_equal(terra::nlyr(stac_build_stack(list(a = r, b = r))), 2)
})
