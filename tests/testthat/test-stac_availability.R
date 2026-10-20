# Which collections have items for an area (stac_availability()).

roi <- c(5.74, 52.05, 5.76, 52.07)

test_that("all collections are asked in one search, and the items are counted per collection and year", {
  local_stac_api(items = list(in_collection("lgn"), in_collection("ntot", "2024-01-01"),
                              in_collection("ntot", "2025-01-01"), in_collection("ntot", "2025-06-01")))
  table <- stac_availability(c("lgn", "ntot", "nox"), roi, token = "t")

  body <- last_request_body()
  expect_setequal(unlist(body$collections), c("lgn", "ntot", "nox"))
  expect_equal(body$intersects$type, "Polygon")
  expect_equal(table$collection, c("lgn", "ntot", "ntot"))
  expect_equal(table$year, c("2024", "2024", "2025"))
  expect_equal(table$n_items, c(1, 1, 2))  # nox has no items: no row
  expect_equal(rownames(table), as.character(1:3))
})

test_that("no items give an empty table, with the columns", {
  local_stac_api(items = list())
  table <- stac_availability("lgn", roi, token = "t")
  expect_equal(nrow(table), 0)
  expect_named(table, c("collection", "year", "n_items"))
})

test_that("a collection given twice is asked once", {
  local_stac_api(items = list(in_collection("lgn")))
  stac_availability(c("lgn", "lgn"), roi, token = "t")
  expect_equal(unlist(last_request_body()$collections), "lgn")
})

test_that("when the page is full, the collections that did not show are asked one by one", {
  # the first page has lgn only, the search says there are more items (2 matched)
  local_stac_api(pages = list(list(in_collection("lgn")), list(in_collection("lgn", "2025-01-01"))))
  table <- stac_availability(c("lgn", "ntot"), roi, token = "t")
  expect_equal(table$collection, c("lgn", "ntot"))
  expect_equal(table$year, c("2024", NA))
  expect_equal(table$n_items, c(1, 2))  # ntot: the number matched by its own question (the stub answers 2)
})

test_that("a collection asked separately whose request fails has NA items; one without items has no row", {
  local_stac_api(pages = list(list(in_collection("lgn")), list(in_collection("lgn", "2025-01-01"))))
  local_mocked_bindings(ndc_count = function(collection, ...) if (collection == "ntot") stop("boom") else 0)
  table <- stac_availability(c("lgn", "ntot", "nox"), roi, token = "t")
  expect_equal(table$collection, c("lgn", "ntot"))
  expect_true(is.na(table$n_items[table$collection == "ntot"]))
})

test_that("errors of the search are passed on, and a token is needed", {
  local_stac_api(items = list(), status = 500L)
  expect_error(stac_availability("lgn", roi, token = "t"))
  expect_error(stac_availability("lgn", roi, token = ""), "NDC_TOKEN")
})

test_that("the collections that are asked one by one are reported, to follow how far the question is", {
  local_stac_api(pages = list(list(in_collection("lgn")), list(in_collection("lgn", "2025-01-01"))))
  local_mocked_bindings(ndc_count = function(...) 0)
  seen <- list()
  table <- ndc_with_progress(
    stac_availability(c("lgn", "ntot", "nox"), roi, token = "t"),
    report = function(message, current, total) seen[[length(seen) + 1]] <<- list(message, current, total)
  )
  expect_equal(seen, list(list("Counting the items of ntot", 1L, 2L), list("Counting the items of nox", 2L, 2L)))
  expect_equal(table$collection, "lgn")                              # (and without the reports nothing changes)
  expect_equal(stac_availability(c("lgn", "ntot", "nox"), roi, token = "t")$collection, "lgn")
})
