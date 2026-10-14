# The helpers that the whole package uses: the check of tokens, and the parameters files of the retrieval functions
# (tested through ndc_get(); the other functions are in test-ndc_repeat.R).

test_that("an empty token is refused before any request is sent", {
  local_stac_api()
  withr::local_envvar(NDC_TOKEN = "", ADC_TOKEN = "")
  expect_error(ndc_get("coll"), "NatureDataCube token is missing")
  expect_error(ndc_get("coll", token = NA_character_), "token is missing")
  expect_error(ndc_get("coll", token = "  "), "token is missing")
  expect_error(ndc_datasets(), "NDC_TOKEN")
  expect_error(ndc_count(collection = "coll"), "token is missing")
  expect_error(adc_get(option = "Fields", params = c(page_size = "1")), "AgroDataCube token is missing")
  expect_error(adc_get(url = "https://agrodatacube.wur.nl/api/v2/rest/fields"), "ADC_TOKEN")
  expect_error(adc_get(), "Provide either")

  # the health check needs no token
  webmockr::stub_request("get", "https://agrodatacube.wur.nl/api/v2/rest/lifeprobe") |>
    webmockr::to_return(status = 200, body = "{}", headers = json_header)
  expect_no_error(adc_get(option = "Health_check", params = NULL))
})

test_that("a search can be saved, and the file describes it", {
  local_stac_api(list(stac_item("a"), stac_item("b")))
  f <- withr::local_tempfile(fileext = ".json")
  res <- ndc_get("coll", roi = c(5, 52, 6, 53), trange = c("2024-01-01", "2024-02-01"), limit = 50, token = "t",
                 mode = "tibble", all_pages = TRUE, save_params = f)
  expect_equal(nrow(res), 2)   # the search itself is not changed

  p <- read_file(f)
  expect_equal(p$schema, "rNDC-ndc_get/1")
  expect_equal(unlist(p$collection), "coll")
  expect_equal(p$roi$type, "Polygon")
  expect_equal(p$trange, "2024-01-01T00:00:00Z/2024-02-01T00:00:00Z")
  expect_equal(p$limit, 50)
  expect_equal(p$mode, "tibble")
  expect_true(p$all_pages)
  expect_equal(p$endpoint, "https://example.org/api/")
  expect_equal(p$rNDC, as.character(utils::packageVersion("rNDC")))
  expect_equal(p$matched, 2)
  expect_match(p$created, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$")
  expect_null(p$asset_names)   # not set: not written
  expect_null(p$items)
})

test_that("the file never holds the token, nor where files go", {
  local_stac_api()
  f <- withr::local_tempfile(fileext = ".json")
  ndc_get("coll", token = "t", output_dir = "/some/where", overwrite = FALSE, save_params = f)
  text <- paste(readLines(f), collapse = "\n")
  expect_false(grepl("token|Bearer|output_dir|overwrite|/some/where", text, ignore.case = TRUE))
  expect_named(read_file(f), c("schema", "created", "rNDC", "endpoint", "collection", "limit", "mode", "all_pages", "matched"),
               ignore.order = TRUE)
})

test_that("a search from the file sends the same search", {
  local_stac_api()
  f <- withr::local_tempfile(fileext = ".json")
  ndc_get(c("a", "b"), roi = c(5, 52, 6, 53), trange = c("2024-01-01", NA), limit = 7, token = "t", save_params = f)
  first <- sent()
  expect_equal(unlist(first$collections), c("a", "b"))

  res <- ndc_get(params_file = f, token = "t")
  expect_equal(sent(), first)
})

test_that("any form of the area gives the same file, and coordinates survive the file", {
  local_stac_api()
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(c(5.123456789012345, 52.1), c(5.9, 52.1), c(5.9, 52.987654321098765),
                                               c(5.123456789012345, 52.1)))), crs = 4326)
  gpkg <- withr::local_tempfile(fileext = ".gpkg")
  sf::st_write(sf::st_sf(geometry = poly), gpkg, quiet = TRUE)
  f1 <- withr::local_tempfile(fileext = ".json"); f2 <- withr::local_tempfile(fileext = ".json")
  f3 <- withr::local_tempfile(fileext = ".json")
  ndc_get("coll", roi = poly, token = "t", save_params = f1)
  first <- sent()
  ndc_get("coll", roi = sf::st_sf(geometry = poly), token = "t", save_params = f2)
  ndc_get("coll", roi = gpkg, token = "t", save_params = f3)
  expect_equal(read_file(f2)$roi, read_file(f1)$roi)
  expect_equal(read_file(f3)$roi, read_file(f1)$roi)   # the path is not in the file: the geometry is

  ndc_get(params_file = f1, token = "t")
  # the file has 15 significant digits: the same to the nanometre, not to the last bit
  expect_equal(unlist(sent()$intersects$coordinates), unlist(first$intersects$coordinates), tolerance = 1e-13)
  expect_equal(sent()$intersects$type, first$intersects$type)

  # another CRS is written in EPSG:4326
  rd <- sf::st_transform(poly, 28992)
  f4 <- withr::local_tempfile(fileext = ".json")
  ndc_get("coll", roi = rd, token = "t", save_params = f4)
  expect_equal(read_file(f4)$roi$coordinates[[1]][[1]][[1]], 5.123456789012345, tolerance = 1e-6)
})

test_that("what the call gives overrides the file, also NULL", {
  local_stac_api(list(stac_item("a")))
  f <- withr::local_tempfile(fileext = ".json")
  ndc_get("coll", roi = c(5, 52, 6, 53), trange = c("2024-01-01", "2024-02-01"), limit = 50, mode = "tibble",
          token = "t", save_params = f)

  expect_s3_class(ndc_get(params_file = f, token = "t"), "tbl_df")                # the mode of the file
  expect_s3_class(ndc_get(params_file = f, mode = "sf", token = "t"), "sf")       # another mode
  ndc_get(params_file = f, limit = 5, token = "t")
  expect_equal(last_request_body()$limit, 5)
  expect_equal(last_request_body()$datetime, "2024-01-01T00:00:00Z/2024-02-01T00:00:00Z")   # the rest from the file
  ndc_get(params_file = f, roi = NULL, token = "t")
  expect_null(last_request_body()$intersects)                                 # the area left out
  ndc_get("other", params_file = f, token = "t")
  expect_equal(unlist(last_request_body()$collections), "other")
})

test_that("the ids of the items can be saved", {
  local_stac_api(list(stac_item("a"), stac_item("b")))
  f <- withr::local_tempfile(fileext = ".json")
  ndc_get("coll", token = "t", save_params = f, save_ids = TRUE)
  expect_equal(unlist(read_file(f)$items), c("a", "b"))
  ndc_get("coll", token = "t", save_params = f)
  expect_null(read_file(f)$items)
})

test_that("nothing is written when the search fails, and the token is still needed", {
  local_stac_api(status = 500L)
  f <- withr::local_tempfile(fileext = ".json")
  expect_error(ndc_get("coll", token = "t", save_params = f))
  expect_false(file.exists(f))

  local_stac_api()
  ndc_get("coll", token = "t")   # no file asked for: none made
  expect_false(file.exists(f))
  writeLines('{"schema": "rNDC-ndc_get/1", "collection": ["coll"]}', f)
  expect_error(ndc_get(params_file = f, token = ""), "token is missing")
})

test_that("a file in a folder that does not exist is a clear error", {
  local_stac_api()
  expect_error(ndc_get("coll", token = "t", save_params = file.path(withr::local_tempdir(), "nope", "q.json")),
               "Could not write the parameters")
})

test_that("only the fields that make up the search are taken, the others are ignored with a warning", {
  local_stac_api()
  f <- withr::local_tempfile(fileext = ".json")
  writeLines(paste0('{"schema": "rNDC-ndc_get/1", "collection": ["coll"], "limit": 9, "token": "secret", ',
                    '"output_dir": "/etc", "overwrite": true, "extra": 1}'), f)
  expect_warning(res <- ndc_get(params_file = f, token = "t"), "token, output_dir, overwrite, extra")
  expect_equal(last_request_body()$limit, 9)
  expect_s3_class(res, "doc_items")   # it ran with the token of the call (the stub refuses another), and made no files
})

test_that("a file for another endpoint is run at the current one, with a warning", {
  local_stac_api()
  f <- withr::local_tempfile(fileext = ".json")
  writeLines('{"schema": "rNDC-ndc_get/1", "endpoint": "https://elsewhere.org/api/", "collection": ["coll"]}', f)
  expect_warning(ndc_get(params_file = f, token = "t"), "another endpoint \\(https://elsewhere.org/api/\\)")
  expect_no_warning({
    writeLines('{"schema": "rNDC-ndc_get/1", "endpoint": "https://example.org/api", "collection": ["coll"]}', f)
    ndc_get(params_file = f, token = "t")   # a slash more or less is the same endpoint
  })
})

test_that("files that are not the parameters of a search are refused", {
  local_stac_api()
  f <- withr::local_tempfile(fileext = ".json")
  expect_error(ndc_get(params_file = file.path(tempdir(), "none.json"), token = "t"), "not found")
  writeLines("{not json", f)
  expect_error(ndc_get(params_file = f, token = "t"), "not valid JSON")
  writeLines('{"collection": ["coll"]}', f)
  expect_error(ndc_get(params_file = f, token = "t"), "not a file with the parameters of ndc_get")
  writeLines('{"schema": "rNDC-ndc_get/2", "collection": ["coll"]}', f)
  expect_error(ndc_get(params_file = f, token = "t"), "newer version of rNDC")
  writeLines('[1, 2]', f)
  expect_error(ndc_get(params_file = f, token = "t"), "does not hold the parameters")

  bad <- function(body) { writeLines(paste0('{"schema": "rNDC-ndc_get/1", ', body, '}'), f); ndc_get(params_file = f, token = "t") }
  expect_error(bad('"collection": [1]'), "`collection`")
  expect_error(bad('"collection": ["a"], "limit": 0'), "`limit`")
  expect_error(bad('"collection": ["a"], "limit": "many"'), "`limit`")
  expect_error(bad('"collection": ["a"], "all_pages": "yes"'), "`all_pages`")
  expect_error(bad('"collection": ["a"], "trange": 5'), "`trange`")
  expect_error(bad('"collection": ["a"], "roi": {"type": "Nope"}'), "`roi`")
  expect_error(bad('"collection": ["a"], "mode": "teleport"'), "should be one of")
  expect_error(bad('"limit": 5'), "collection")                      # nothing to search
})
