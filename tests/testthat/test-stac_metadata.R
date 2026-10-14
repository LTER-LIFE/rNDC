# The STAC helpers for headers, items and the metadata of a search.

test_that("stac_collect_metadata reads items from the STAC API", {
  local_stac_api(nitrogen_items())
  m <- stac_collect_metadata(c(5, 52, 6, 53), token = "t", endpoint = ndc_endpoint(), collection = "coll")
  expect_equal(m$id, c("a", "b", "c"))
  expect_equal(m$layer, c("ntot", "nox", "ntot"))
  expect_equal(m$year, c("2024", "2024", "2025"))
  expect_equal(m$href, paste0("https://example.org/data/", c("a", "b", "c")))
  expect_equal(last_request_body()$collections, list("coll"))
})

test_that("stac_year_trange builds a time range from the years", {
  expect_equal(stac_year_trange(2024), c("2024-01-01T00:00:00Z", "2024-12-31T23:59:59Z"))
  expect_equal(stac_year_trange(c("2025", "2024")), c("2024-01-01T00:00:00Z", "2025-12-31T23:59:59Z"))
  expect_null(stac_year_trange(NULL))
  expect_null(stac_year_trange(NA))
})

test_that("the year is sent to the STAC API as a time range", {
  local_stac_api(nitrogen_items())
  aoi <- c(5, 52, 6, 53)
  stac_collect_metadata(aoi, "t", ndc_endpoint(), "coll", trange = stac_year_trange(2024))
  expect_equal(last_request_body()$datetime, "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")

  stac_collect_metadata(aoi, "t", ndc_endpoint(), "coll")
  expect_null(last_request_body()$datetime)

  nitrogen_collect_metadata(aoi, "t", year = "2024")
  expect_equal(last_request_body()$datetime, "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")
  landuse_collect_metadata(aoi, "t", year = 2024)
  expect_equal(last_request_body()$datetime, "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")
  landuse_collect_metadata(aoi, "t")
  expect_null(last_request_body()$datetime)
})

test_that("stac_collect_metadata returns an empty tibble when nothing matches", {
  local_stac_api(list())
  m <- stac_collect_metadata(c(5, 52, 6, 53), token = "t", endpoint = ndc_endpoint(), collection = "coll")
  expect_equal(nrow(m), 0)
})

test_that("stac_collect_metadata fails with a rejected token", {
  local_stac_api(nitrogen_items())
  expect_error(stac_collect_metadata(c(5, 52, 6, 53), token = "wrong", endpoint = ndc_endpoint(),
                                     collection = "coll"))
})

test_that("keywords are normalised", {
  expect_equal(stac_keywords_to_vec(NULL), character(0))
  expect_equal(stac_keywords_to_vec(" LGN, Land Use "), c("lgn", "land use"))
  expect_equal(stac_keywords_to_vec(list("A", "a", "")), "a")
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
