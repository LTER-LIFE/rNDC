# adc_get() and adc_get_all(): requests to the AgroDataCube REST API.

test_that("adc_get sends the token header, returns parsed content and fails on HTTP errors", {
  withr::defer({ webmockr::stub_registry_clear(); webmockr::disable(adapter = "httr", quiet = TRUE) })
  webmockr::enable(adapter = "httr", quiet = TRUE)
  webmockr::stub_request("get", "https://agrodatacube.wur.nl/api/v2/rest/fields?output_epsg=4326") |>
    webmockr::wi_th(headers = list(token = "secret")) |>
    webmockr::to_return(body = json_body(list(features = list(list(id = 1)))), headers = json_header)
  webmockr::stub_request("get", "https://agrodatacube.wur.nl/api/v2/rest/fields?output_epsg=4326") |>
    webmockr::to_return(body = json_body(list(message = "denied")), status = 401, headers = json_header)

  res <- adc_get(option = "Fields", params = c(output_epsg = "4326"), token = "secret")
  expect_equal(res$features[[1]]$id, 1)
  expect_error(adc_get(option = "Fields", params = c(output_epsg = "4326"), token = "bad"), "401")
})

test_that("adc_get rejects unknown servers", {
  expect_error(adc_get(option = "Fields", params = c(a = "1"), server = "zzz"), "Unknown `server`")
})

test_that("adc_get includes the API message in HTTP errors", {
  with_adc_stubs()
  webmockr::stub_request("get", "https://agrodatacube.wur.nl/api/v2/rest/fields?output_epsg=4326") |>
    webmockr::to_return(body = json_body(list(status = "Geometry area too large")), status = 403,
                        headers = json_header)
  expect_error(adc_get(option = "Fields", params = c(output_epsg = "4326"), token = "t"),
               "HTTP 403\\).*Geometry area too large")
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
