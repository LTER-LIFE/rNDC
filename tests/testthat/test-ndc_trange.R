# as_rfc3339() and ndc_trange(): dates for STAC searches.

test_that("as_rfc3339 formats in UTC", {
  expect_equal(as_rfc3339("2024-01-31"), "2024-01-31T00:00:00Z")
})

test_that("ndc_trange builds STAC datetime values", {
  expect_equal(ndc_trange("2024-01-01"), "2024-01-01T00:00:00Z")
  expect_equal(ndc_trange(c("2024-01-01", "2024-02-01")),
               "2024-01-01T00:00:00Z/2024-02-01T00:00:00Z")
  expect_equal(ndc_trange(c("2024-02-01", "2024-01-01")),
               "2024-01-01T00:00:00Z/2024-02-01T00:00:00Z")
  expect_equal(ndc_trange(c(NA, "2024-02-01")), "../2024-02-01T00:00:00Z")
  expect_equal(ndc_trange(c("2024-01-01", NA)), "2024-01-01T00:00:00Z/..")
  expect_true(is.na(ndc_trange(c(NA, NA))))

  # idempotent: already formatted values are kept
  for (x in c("2024-01-01T00:00:00Z", "2024-01-01T00:00:00Z/2024-02-01T00:00:00Z",
              "../2024-02-01T00:00:00Z", "2024-01-01T00:00:00Z/..")) {
    expect_equal(ndc_trange(x), x)
  }
  expect_true(is.na(ndc_trange(c("2024-01-01", "2024-02-01", "2024-03-01"))))
})
