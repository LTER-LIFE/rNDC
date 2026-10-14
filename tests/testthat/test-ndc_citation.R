# ndc_citation(): the reference of a dataset, from its metadata.

test_that("ndc_citation writes the reference of a dataset", {
  local_stac_api(collections = list(rich_collection("lgn"), stac_collection("plain")))
  expect_equal(
    ndc_citation("lgn", token = "t"),
    c(lgn = "Hazeu, G. (2026). LGN2024 [Data set]. 4TU.ResearchData. https://doi.org/10.4121/abc License: CC-BY-4.0.")
  )
  expect_equal(ndc_citation("lgn", license = FALSE, token = "t"),
               c(lgn = "Hazeu, G. (2026). LGN2024 [Data set]. 4TU.ResearchData. https://doi.org/10.4121/abc"))
  expect_match(ndc_citation("lgn", publications = TRUE, token = "t"),
               "\nRelated: Hazeu et al. (2026). A paper.\nRelated: A report without DOI.", fixed = TRUE)

  # no citation: NA; the order and the repeats of the request are kept; unknown collections are refused
  expect_equal(ndc_citation(c("plain", "lgn", "plain"), token = "t")[["plain"]], NA_character_)
  expect_equal(names(ndc_citation(c("plain", "lgn", "plain"), token = "t")), c("plain", "lgn", "plain"))
  expect_error(ndc_citation("nope", token = "t"), "Unknown collection")
})

test_that("ndc_citation does not repeat a DOI that the citation has, and can use metadata that was read already", {
  doc <- rich_collection()
  doc[["sci:citation"]] <- "Hazeu, G. (2026). LGN2024. https://doi.org/10.4121/abc"
  local_stac_api(collections = list(doc))
  m <- ndc_metadata(token = "t")
  n <- length(webmockr::request_registry()$request_signatures$hash)
  txt <- ndc_citation("lgn", metadata = m, token = "t")
  expect_equal(txt[["lgn"]], "Hazeu, G. (2026). LGN2024. https://doi.org/10.4121/abc License: CC-BY-4.0.")
  expect_length(webmockr::request_registry()$request_signatures$hash, n)  # no new request
})
