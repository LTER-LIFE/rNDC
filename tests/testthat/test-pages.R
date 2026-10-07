# Retrieving all pages of results: ndc_get(all_pages = TRUE) and adc_get_all().

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

fields_stub <- function(...) {
  stub <- webmockr::stub_request("get", uri_regex = "agrodatacube.wur.nl/api/v2/rest/fields")
  for (page in list(...)) stub <- webmockr::to_return(stub, body = json_body(page), headers = json_header)
  invisible(stub)
}

feature_collection <- function(ids) {
  list(type = "FeatureCollection", features = lapply(ids, function(i) {
    list(type = "Feature", geometry = list(type = "Point", coordinates = c(5, 52)), properties = list(fieldid = i))
  }))
}

request_uris <- function() {
  reqs <- webmockr::request_registry()$request_signatures$hash
  vapply(reqs, function(r) as.character(r$sig$uri), character(1), USE.NAMES = FALSE)
}

test_that("adc_get_all combines the pages until one comes back short", {
  with_adc_stubs()
  fields_stub(feature_collection(1:2), feature_collection(3:4), feature_collection(5))
  res <- adc_get_all("Fields", c(epsg = "4326"), token = "t", page_size = 2)
  expect_length(res$features, 5)
  expect_equal(vapply(res$features, function(f) f$properties$fieldid, numeric(1)), 1:5)

  uris <- request_uris()
  expect_length(uris, 3)
  for (i in 0:2) expect_match(uris[i + 1], paste0("page_offset=", i, "($|&)"))
  expect_match(uris, "page_size=2", all = TRUE)
})

test_that("adc_get_all stops after an exact multiple of the page size", {
  with_adc_stubs()
  fields_stub(feature_collection(1:2), feature_collection(3:4), feature_collection(integer()))
  expect_length(adc_get_all("Fields", c(epsg = "4326"), token = "t", page_size = 2)$features, 4)
  expect_length(request_uris(), 3)
})

test_that("adc_get_all makes one request when the first page is short, and warns at the page limit", {
  with_adc_stubs()
  fields_stub(feature_collection(1:3))
  expect_length(adc_get_all("Fields", c(epsg = "4326"), token = "t")$features, 3)
  expect_length(request_uris(), 1)

  webmockr::stub_registry_clear()
  fields_stub(feature_collection(1:2))  # the last response repeats
  expect_warning(res <- adc_get_all("Fields", c(epsg = "4326"), token = "t", page_size = 2, max_pages = 3),
                 "stopped after 3 pages")
  expect_length(res$features, 6)
})

test_that("adc_get_all reports HTTP errors with the message of the server", {
  with_adc_stubs()
  webmockr::stub_request("get", uri_regex = "agrodatacube.wur.nl/api/v2/rest/fields") |>
    webmockr::to_return(body = json_body(list(status = "Geometry area too large")), status = 403,
                        headers = json_header)
  expect_error(adc_get_all("Fields", c(epsg = "4326"), token = "t"), "HTTP 403.*Geometry area too large")
})
