# ndc_datasets() and ndc_count().

test_that("ndc_count and ndc_datasets use the matched counts", {
  local_stac_api(list(stac_item("a"), stac_item("b"), stac_item("c")))
  expect_equal(ndc_count(collection = "coll", token = "t"), 3)

  expect_equal(ndc_datasets(token = "t"), c("a", "b"))
  d <- ndc_datasets(token = "t", total = TRUE)
  expect_equal(d$dataset_id, c("a", "b"))
  expect_equal(d$n_total, c(3, 3))
  d <- ndc_datasets(roi = c(5, 52, 6, 53), token = "t")
  expect_equal(d$n_matched, c(3, 3))
})

test_that("ndc_datasets can be limited to some collections, without listing them", {
  local_stac_api(list(stac_item("a"), stac_item("b"), stac_item("c")))
  webmockr::request_registry_clear()
  d <- ndc_datasets(roi = c(5, 52, 6, 53), trange = c("2024-01-01", "2024-12-31"), token = "t",
                    collections = c("x", "y", "x"))
  expect_equal(d$dataset_id, c("x", "y"))
  expect_equal(d$n_matched, c(3, 3))
  expect_equal(last_request_body()$collections, list("y"))
  expect_equal(last_request_body()$datetime, "2024-01-01T00:00:00Z/2024-12-31T00:00:00Z")
  # the list of collections of the API was not requested
  expect_false(any(grepl("/collections", vapply(webmockr::request_registry()$request_signatures$hash,
                                                function(r) r$sig$uri, ""))))
  expect_equal(ndc_datasets(token = "t", collections = "x"), "x")
})
