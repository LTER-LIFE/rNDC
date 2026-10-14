# ndc_get(): the search, the output modes and the pages of results.

test_that("ndc_get sends token, roi and datetime in the STAC search", {
  local_stac_api()
  res <- ndc_get("coll", roi = c(5, 52, 6, 53), trange = c("2024-01-01", "2024-02-01"), token = "t")
  expect_s3_class(res, "doc_items")
  expect_equal(res$numberMatched, 1)

  body <- last_request_body()
  expect_equal(body$collections, list("coll"))
  expect_equal(body$datetime, "2024-01-01T00:00:00Z/2024-02-01T00:00:00Z")
  expect_equal(body$intersects$type, "Polygon")
})

test_that("ndc_get does not warn when the first page is complete", {
  local_stac_api(list(stac_item("a"), stac_item("b")))
  expect_no_warning(ndc_get("coll", token = "t", mode = "tibble"))
})

test_that("ndc_get warns when a conversion mode returns an incomplete first page", {
  local_stac_api(list(stac_item("a"), stac_item("b")), matched = 5)
  expect_warning(ndc_get("coll", token = "t", mode = "sf"), "Only 2 of 5 matched items")
  expect_no_warning(ndc_get("coll", token = "t", mode = "items"))
})

test_that("ndc_get does not add filters when roi and trange are NULL", {
  local_stac_api()
  ndc_get("coll", token = "t")
  body <- last_request_body()
  expect_null(body$datetime)
  expect_null(body$intersects)
})

test_that("ndc_get fails when the token is rejected", {
  local_stac_api()
  expect_error(ndc_get("coll", token = "wrong"))
})

test_that("ndc_get output modes", {
  local_stac_api(list(stac_item("a"), stac_item("b")))
  tb <- ndc_get("coll", token = "t", mode = "tibble")
  expect_s3_class(tb, "tbl_df")
  expect_equal(nrow(tb), 2)
  sfo <- ndc_get("coll", token = "t", mode = "sf")
  expect_s3_class(sfo, "sf")
  expect_equal(nrow(sfo), 2)
})

test_that("ndc_get rejects invalid trange and mode before sending anything", {
  local_stac_api()
  expect_error(ndc_get("coll", trange = c(NA, NA), token = "t"), "Invalid `trange`")
  expect_error(ndc_get("coll", mode = "nope", token = "t"), "should be one of")
})

three_pages <- function() list(list(stac_item("a"), stac_item("b")), list(stac_item("c"), stac_item("d")),
                               list(stac_item("e")))

test_that("ndc_get converts only the first page unless all_pages is TRUE", {
  local_stac_api(pages = three_pages())
  expect_warning(first <- ndc_get("coll", token = "t", mode = "sf", limit = 2), "Only 2 of 5 matched items")
  expect_equal(nrow(first), 2)

  local_stac_api(pages = three_pages())
  expect_no_warning(all <- ndc_get("coll", token = "t", mode = "sf", limit = 2, all_pages = TRUE))
  expect_s3_class(all, "sf")
  expect_equal(nrow(all), 5)
  expect_equal(all$title, paste("Item", c("a", "b", "c", "d", "e")))
})

test_that("all_pages works for the tibble and sfc modes, and does not change the other modes", {
  local_stac_api(pages = three_pages())
  expect_equal(nrow(ndc_get("coll", token = "t", mode = "tibble", all_pages = TRUE)), 5)
  local_stac_api(pages = three_pages())
  expect_length(ndc_get("coll", token = "t", mode = "sfc", all_pages = TRUE), 5)

  local_stac_api(pages = three_pages())
  expect_length(ndc_get("coll", token = "t", mode = "items", all_pages = TRUE)$features, 2)  # first page, as before
  local_stac_api(pages = three_pages())
  expect_length(ndc_get("coll", token = "t", mode = "fetch", all_pages = TRUE)$features, 5)
})

test_that("an all-pages search that matches nothing gives an empty result", {
  local_stac_api(items = list())
  res <- ndc_get("coll", token = "t", mode = "sf", all_pages = TRUE)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
})

test_that("an all-pages search passes errors on", {
  local_stac_api(items = list(), status = 500L)
  expect_error(ndc_get("coll", token = "t", mode = "sf", all_pages = TRUE))
})
