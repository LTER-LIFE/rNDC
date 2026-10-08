# Progress and interruption of the functions that make many requests (R/ndc_progress.R).

test_that("without options nothing is reported and nothing stops", {
  expect_null(getOption("rNDC.progress"))
  expect_null(rNDC:::ndc_progress("x", 1, 2))
})

test_that("ndc_with_progress sets the options for the expression only, and returns its value", {
  report <- function(message, current, total) NULL
  stop_now <- function() FALSE
  expect_equal(ndc_with_progress({
    expect_identical(getOption("rNDC.progress"), report)
    expect_identical(getOption("rNDC.interrupt"), stop_now)
    42
  }, report = report, interrupt = stop_now), 42)
  expect_null(getOption("rNDC.progress"))
  expect_null(getOption("rNDC.interrupt"))

  # options that were set before come back, also after an error
  withr::local_options(rNDC.progress = report)
  expect_error(ndc_with_progress(stop("boom"), report = function(...) NULL), "boom")
  expect_identical(getOption("rNDC.progress"), report)

  expect_error(ndc_with_progress(1, report = "not a function"))
})

test_that("the report gets the text and the position as integers, and its errors are ignored", {
  seen <- list()
  ndc_with_progress(rNDC:::ndc_progress("Downloading a", 3, 10),
                    report = function(message, current, total) seen <<- list(message, current, total))
  expect_identical(seen, list("Downloading a", 3L, 10L))

  expect_null(ndc_with_progress(rNDC:::ndc_progress("x", 1, 2), report = function(...) stop("display broke")))
})

test_that("the interrupt function stops the download with an error of its own class", {
  expect_null(ndc_with_progress(rNDC:::ndc_progress("x"), interrupt = function() FALSE))
  expect_null(ndc_with_progress(rNDC:::ndc_progress("x"), interrupt = function() stop("broken")))  # not a reason to stop
  err <- tryCatch(ndc_with_progress(rNDC:::ndc_progress("x"), interrupt = function() TRUE), error = function(e) e)
  expect_s3_class(err, "rNDC_interrupted")
  expect_match(conditionMessage(err), "interrupted")
})

test_that("get_meteo_for_long_period reports each chunk, and can be stopped between chunks", {
  calls <- 0
  testthat::local_mocked_bindings(
    get_meteo_for_period = function(...) {
      calls <<- calls + 1
      sf::st_sf(a = calls, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
    }
  )
  seen <- list()
  res <- suppressMessages(ndc_with_progress(
    get_meteo_for_long_period(310, "2024-01-01", "2024-01-20", token = "t", by_days = 7),
    report = function(message, current, total) seen[[length(seen) + 1]] <<- list(message, current, total)
  ))
  expect_equal(nrow(res), 3)
  expect_equal(vapply(seen, `[[`, 1L, 2), 1:3)
  expect_equal(vapply(seen, `[[`, 1L, 3), rep(3L, 3))
  expect_equal(seen[[1]][[1]], "Downloading 2024-01-01 -> 2024-01-07 (1/3)")

  calls <- 0
  stopped <- tryCatch(
    suppressMessages(ndc_with_progress(
      get_meteo_for_long_period(310, "2024-01-01", "2024-01-20", token = "t", by_days = 7),
      interrupt = function() calls >= 1  # stop before the second request
    )),
    rNDC_interrupted = function(e) "stopped"
  )
  expect_equal(stopped, "stopped")
  expect_equal(calls, 1)
})

# an error as raised by gm_get() for a failed request
http_error <- function(status) {
  structure(class = c("rNDC_http_error", "error", "condition"),
            list(message = paste("GroenMonitor request failed (HTTP", status, ")"), call = NULL, status = status))
}

# a fake gm_get that writes a small GeoTIFF, as the WCS of GroenMonitor would; the days in `missing` have no coverage
fake_ndvi_download <- function(missing = character(0)) {
  function(url, option, params, out_path, overwrite = TRUE) {
    if (any(vapply(missing, grepl, NA, x = url, fixed = TRUE))) stop(http_error(404))
    terra::writeRaster(terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = c(0.2, 0.4, 0.6, 0.8)),
                       out_path, overwrite = TRUE)
    invisible(NULL)
  }
}

test_that("the NDVI downloads report each day, and a stop removes the files", {
  testthat::local_mocked_bindings(gm_get = fake_ndvi_download())
  poly <- square_4326(0.1, 0.1, 0.9, 0.9)
  seen <- list()
  r <- suppressMessages(ndc_with_progress(
    download_avg_ndvi_month(poly, 2024, 2),
    report = function(message, current, total) seen[[length(seen) + 1]] <<- list(message, current, total)
  ))
  expect_s4_class(r, "SpatRaster")
  expect_length(seen, 29)  # February 2024
  expect_equal(seen[[3]], list("Downloading NDVI 2024-02-03 (3/29)", 3L, 29L))

  # stopped on the 5th day: the tile folder is gone
  before <- list.files(tempdir(), "^ndvi_month_")
  n <- 0
  expect_error(
    suppressMessages(ndc_with_progress(
      download_avg_ndvi_month(poly, 2024, 2),
      report = function(...) n <<- n + 1,
      interrupt = function() n >= 5
    )),
    class = "rNDC_interrupted"
  )
  expect_equal(n, 5)
  expect_identical(list.files(tempdir(), "^ndvi_month_"), before)
})

test_that("the NDVI stack counts the days of all months, and cleans up when stopped", {
  testthat::local_mocked_bindings(gm_get = fake_ndvi_download())
  poly <- square_4326(0.1, 0.1, 0.9, 0.9)
  seen <- list()
  r <- suppressMessages(ndc_with_progress(
    download_avg_ndvi_stack(poly, 2024, 1, 2024, 2),
    report = function(message, current, total) seen[[length(seen) + 1]] <<- list(message, current, total)
  ))
  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 2)
  expect_length(seen, 31 + 29)
  expect_equal(seen[[32]], list("Downloading NDVI 2024-02-01 (32/60)", 32L, 60L))
  expect_equal(seen[[60]][[3]], 60L)

  before <- list.files(tempdir(), "^ndvi_temp_")
  expect_error(
    suppressMessages(ndc_with_progress(download_avg_ndvi_stack(poly, 2024, 1, 2024, 2), interrupt = function() TRUE)),
    class = "rNDC_interrupted"
  )
  expect_identical(list.files(tempdir(), "^ndvi_temp_"), before)
})

test_that("days without coverage are skipped, other failures are raised", {
  poly <- square_4326(0.1, 0.1, 0.9, 0.9)

  # 404 (no coverage) for some days: averaged over the others
  testthat::local_mocked_bindings(gm_get = fake_ndvi_download(missing = c("_20240201", "_20240202")))
  r <- suppressMessages(download_avg_ndvi_month(poly, 2024, 2))
  expect_equal(names(r), "ndvi_mean_202402")

  # no coverage at all: NULL
  testthat::local_mocked_bindings(gm_get = function(...) stop(http_error(404)))
  expect_null(suppressMessages(download_avg_ndvi_month(poly, 2024, 2)))

  # a client error (e.g. 400) is not "no data"
  testthat::local_mocked_bindings(gm_get = function(...) stop(http_error(400)))
  expect_error(suppressMessages(download_avg_ndvi_month(poly, 2024, 2)), "HTTP 400")
})

test_that("gm_download_day retries server failures and then raises them", {
  dir <- withr::local_tempdir()
  bbox <- c(xmin = "0", xmax = "1", ymin = "0", ymax = "1")
  calls <- 0
  testthat::local_mocked_bindings(gm_get = function(url, out_path, ...) {
    calls <<- calls + 1
    if (calls < 3) stop(http_error(503))
    file.create(out_path)
  })
  path <- gm_download_day(as.Date("2024-02-01"), bbox, dir, pause = 0)
  expect_match(path, "ndvi_20240201.tif$")
  expect_equal(calls, 3)

  calls <- 0
  testthat::local_mocked_bindings(gm_get = function(...) { calls <<- calls + 1; stop(http_error(500)) })
  expect_error(gm_download_day(as.Date("2024-02-01"), bbox, dir, retries = 1, pause = 0), "HTTP 500")
  expect_equal(calls, 2)
})
