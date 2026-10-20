# ndc_available(): the datasets with data for an area and period, and the periods of the external datasets.

nl_roi <- c(5.74, 52.05, 5.76, 52.07)        # in the Netherlands

sea_roi <- c(-30, 40, -29, 41)               # the Atlantic

# an API with the collections lgn (items), ntot (items) and nox (no items)
local_api <- function(env = parent.frame()) {
  local_stac_api(items = list(in_collection("lgn"), in_collection("ntot", "2024-06-01")),
                 collections = list(stac_collection("lgn"), stac_collection("ntot"), stac_collection("nox")),
                 env = env)
}

test_that("periods are read from anything that ndc_trange() accepts", {
  utc <- function(x) as.POSIXct(x, tz = "UTC")
  expect_equal(trange_bounds(NULL), utc(c(NA, NA)))
  expect_equal(trange_bounds(c("2024-01-01", "2024-12-31")), utc(c("2024-01-01", "2024-12-31")))
  expect_equal(trange_bounds(c("2024-01-01", NA)), utc(c("2024-01-01", NA)))
  expect_equal(trange_bounds(c(NA, "2024-12-31")), utc(c(NA, "2024-12-31")))
  expect_equal(trange_bounds("2024-05-01"), utc(c("2024-05-01", "2024-05-01")))
  expect_equal(trange_bounds("2024-01-01T00:00:00Z/.."), utc(c("2024-01-01", NA)))
})

test_that("external datasets are in the period when it overlaps the years that they cover", {
  start <- as.Date(c("1970-01-01", "2009-01-01", NA))   # weather, fields, soil (no time dimension)
  end <- as.Date(c(NA, NA, NA))
  in_period <- function(trange) external_in_period(start, end, trange)

  expect_equal(in_period(NULL), c(TRUE, TRUE, TRUE))
  expect_equal(in_period(c("2024-01-01", "2024-12-31")), c(TRUE, TRUE, TRUE))
  expect_equal(in_period(c("1990-01-01", "1990-12-31")), c(TRUE, FALSE, TRUE))   # before the fields
  expect_equal(in_period(c("1950-01-01", "1960-12-31")), c(FALSE, FALSE, TRUE))   # before everything
  expect_equal(in_period(c(NA, "2008-12-31")), c(TRUE, FALSE, TRUE))              # open start
  expect_equal(in_period(c("2008-01-01", NA)), c(TRUE, TRUE, TRUE))               # open end
  expect_equal(in_period(c("2999-01-01", "2999-12-31")), c(FALSE, FALSE, TRUE))   # in the future
})

test_that("the collections are checked against their items, and the other services by their coverage", {
  local_api()
  a <- ndc_available(nl_roi, token = "t")

  expect_s3_class(a, "tbl_df")
  expect_named(a, c("id", "title", "source", "available", "verified", "n_items", "start", "end", "license", "doi", "note"))
  stac <- a[a$source == "NatureDataCube", ]
  expect_equal(stac$id, c("lgn", "ntot", "nox"))
  expect_equal(stac$available, c(TRUE, TRUE, FALSE))
  expect_equal(stac$n_items, c(1, 1, 0))
  expect_true(all(stac$verified))
  expect_equal(stac$license, rep("proprietary", 3))   # from the metadata of the collections

  ext <- a[a$source != "NatureDataCube", ]
  expect_true(all(ext$available))                      # in the Netherlands, no period
  expect_false(any(ext$verified))
  expect_true(all(is.na(ext$n_items)))
  expect_true(all(grepl("not checked against the service", ext$note)))
  expect_true("groenmonitor-ndvi" %in% ext$id)         # the collection 'ndvi' is not among the ones asked
  expect_equal(last_request_body()$intersects$type, "Polygon")
})

test_that("the period goes to the search, and limits the external datasets", {
  local_api()
  a <- ndc_available(nl_roi, trange = c("1990-01-01", "1990-12-31"), token = "t")
  expect_equal(last_request_body()$datetime, "1990-01-01T00:00:00Z/1990-12-31T00:00:00Z")
  expect_equal(a$available[a$id == "agricultural-fields"], FALSE)
  expect_equal(a$available[a$id == "weather"], TRUE)
  expect_false(any(c("soil-map", "ahn") %in% a$id))    # they are collections of the NatureDataCube now
})

test_that("an area outside the Netherlands has none of the external datasets", {
  local_api()
  a <- ndc_available(sea_roi, token = "t")
  expect_false(any(a$available[a$source != "NatureDataCube"]))
  expect_true(all(a$available[a$id %in% c("lgn", "ntot")]))   # what the (stubbed) search answers
})

test_that("without an area the whole country counts, and a collection with the same data as an external dataset hides it", {
  local_stac_api(items = list(in_collection("ndvi")), collections = list(stac_collection("ndvi"), stac_collection("lgn")))
  a <- ndc_available(token = "t")
  expect_null(last_request_body()$intersects)
  expect_false("groenmonitor-ndvi" %in% a$id)          # 'ndvi' is in the result
  expect_true(all(a$available[a$source != "NatureDataCube"]))
})

test_that("ndc_available can be limited to some collections, and leave out the external datasets", {
  local_api()
  a <- ndc_available(nl_roi, collections = c("ntot"), external = FALSE, token = "t")
  expect_equal(a$id, "ntot")
  expect_equal(unlist(last_request_body()$collections), "ntot")
  expect_error(ndc_available(nl_roi, collections = "nope", token = "t"), "Unknown collection")
})

test_that("a collection whose items could not be counted is NA, and bad input is refused", {
  local_stac_api(pages = list(list(in_collection("lgn")), list(in_collection("lgn", "2025-01-01"))),
                 collections = list(stac_collection("lgn"), stac_collection("ntot")))
  local_mocked_bindings(ndc_count = function(...) stop("boom"))
  a <- ndc_available(nl_roi, external = FALSE, token = "t")
  expect_equal(a$available, c(TRUE, NA))   # lgn had items on the first page; ntot could not be asked
  expect_true(all(is.na(a$n_items)))      # and lgn could not be counted again

  expect_error(ndc_available(nl_roi, trange = c(NA, NA), token = "t"), "Invalid `trange`")
  expect_error(ndc_available(token = ""), "token is missing")
})

test_that("stac_availability can be limited to a period and needs no area", {
  local_stac_api(items = list(in_collection("lgn")))
  stac_availability("lgn", token = "t", trange = c("2024-01-01", "2024-12-31"))
  body <- last_request_body()
  expect_null(body$intersects)
  expect_equal(body$datetime, "2024-01-01T00:00:00Z/2024-12-31T00:00:00Z")
})

test_that("the number of items is exact when one search cannot return them all", {
  # the first page has 1 of the 3 items of lgn; counted on its own the stub says 3
  local_stac_api(pages = list(list(in_collection("lgn")), list(in_collection("lgn", "2025-01-01")),
                              list(in_collection("lgn", "2026-01-01"))),
                 collections = list(stac_collection("lgn"), stac_collection("ntot")))
  a <- stac_availability(c("lgn", "ntot"), nl_roi, token = "t")
  expect_false(attr(a, "complete"))
  expect_equal(a$n_items[a$collection == "lgn"], 1)    # only the first page

  counts <- character()
  local_mocked_bindings(ndc_count = function(collection, ..., limit) { counts <<- c(counts, collection); 3 })
  e <- ndc_available(nl_roi, external = FALSE, token = "t")
  expect_equal(e$n_items, c(3, 3))                       # exact: lgn recounted, ntot asked as hidden
  expect_equal(sort(counts), c("lgn", "ntot"))
})

test_that("nothing is recounted when the search returned everything", {
  local_stac_api(items = list(in_collection("lgn"), in_collection("lgn", "2025-01-01")),
                 collections = list(stac_collection("lgn")))
  local_mocked_bindings(ndc_count = function(...) stop("not needed"))
  a <- stac_availability("lgn", nl_roi, token = "t")
  expect_true(attr(a, "complete"))
  expect_equal(ndc_available(nl_roi, external = FALSE, token = "t")$n_items, 2)
})

test_that("the counts of the collections are reported as one count of all of them, from both steps", {
  # the first page has lgn only, with more items matched: ntot and nox are counted one by one, then lgn again (exactly)
  local_stac_api(pages = list(list(in_collection("lgn")), list(in_collection("lgn", "2025-01-01"))),
                 collections = list(stac_collection("lgn"), stac_collection("ntot"), stac_collection("nox")))
  local_mocked_bindings(ndc_count = function(...) 1)
  seen <- list()
  ndc_with_progress(ndc_available(nl_roi, token = "t", external = FALSE),
                    report = function(message, current, total) seen[[length(seen) + 1]] <<- list(message, current, total))
  expect_equal(vapply(seen, function(x) x[[2]], 0L), 1:3)            # not 1, 2 and then 1 again
  expect_equal(unique(vapply(seen, function(x) x[[3]], 0L)), 3L)     # of the three collections
  expect_equal(seen[[3]][[1]], "Counting the items of lgn")

  # without a report function nothing changes (and the option is put back)
  expect_null(getOption("rNDC.progress"))
  expect_equal(nrow(ndc_available(nl_roi, token = "t", external = FALSE)), 3)
})
