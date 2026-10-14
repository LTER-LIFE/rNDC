# gm_get(): downloads from the GroenMonitor WCS server.

test_that("gm_get writes the file, and removes it and reports the message on errors", {
  with_adc_stubs()
  p <- c(date = "20251225", xmin = "1", xmax = "2", ymin = "3", ymax = "4", format = "tiff")
  ok <- gm_url("NDVI", p)
  bad <- gm_url("WDVI", p)
  webmockr::stub_request("get", ok) |> webmockr::to_return(body = "x", status = 200)
  webmockr::stub_request("get", bad) |>
    webmockr::to_return(
      body = "<ows:ExceptionReport><ows:Exception><ows:ExceptionText>Could not locate coverage</ows:ExceptionText></ows:Exception></ows:ExceptionReport>",
      status = 404, headers = list("Content-Type" = "application/xml"))

  expect_equal(httr::status_code(gm_get(url = ok, out_path = withr::local_tempfile())), 200L)

  out <- withr::local_tempfile()
  expect_error(gm_get(url = bad, out_path = out), "HTTP 404\\): Could not locate coverage")
  expect_false(file.exists(out))
})
