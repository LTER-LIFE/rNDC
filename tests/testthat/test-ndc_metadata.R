# ndc_metadata(): the metadata of the collections.

test_that("ndc_metadata reads the metadata of the collections, including the fields of extensions", {
  local_stac_api(collections = list(rich_collection(), stac_collection("plain")))
  m <- ndc_metadata(token = "t")

  expect_s3_class(m, "tbl_df")
  expect_equal(m$id, c("lgn", "plain"))
  expect_equal(m$title, c("Land Use (LGN)", NA))
  expect_equal(m$license, c("CC-BY-4.0", "proprietary"))  # the plain collection of the fixtures
  expect_equal(m$keywords[[1]], c("lgn", "land-use"))
  expect_equal(m$keywords[[2]], character(0))
  expect_equal(c(m$xmin[1], m$ymin[1], m$xmax[1], m$ymax[1]), c(3.31, 50.75, 7.23, 53.55))
  expect_equal(m$start[1], as.POSIXct("2024-01-01", tz = "UTC"))
  expect_equal(m$end[1], as.POSIXct("2024-12-31", tz = "UTC"))
  expect_true(is.na(m$start[2]) && is.na(m$end[2]))  # open interval
  expect_equal(c(m$xmin[2], m$xmax[2]), c(0, 1))

  # Scientific extension
  expect_equal(m$doi, c("10.4121/abc", NA))
  expect_equal(m$citation[1], "Hazeu, G. (2026). LGN2024 [Data set]. 4TU.ResearchData.")
  expect_true(is.na(m$citation[2]))
  expect_equal(m$publications[[1]]$doi, c("10.1/xyz", NA))
  expect_equal(m$publications[[1]]$citation, c("Hazeu et al. (2026). A paper.", "A report without DOI."))
  expect_equal(nrow(m$publications[[2]]), 0)

  expect_equal(m$providers[[1]]$name, "NatureDataCube")
  expect_equal(m$providers[[1]]$roles, "host, processor")
  expect_equal(nrow(m$providers[[2]]), 0)
  expect_match(m$extensions[[1]], "scientific")

  # a field of another extension gets a list-column of its own, found in the data; the known ones do not
  expect_true("proj:epsg" %in% names(m))
  expect_equal(m[["proj:epsg"]][[1]], 28992)
  expect_null(m[["proj:epsg"]][[2]])
  expect_false(any(c("sci:doi", "sci:citation", "sci:publications") %in% names(m)))
})

test_that("ndc_metadata can be limited to some collections, and refuses unknown ones", {
  local_stac_api(collections = list(rich_collection("lgn"), rich_collection("ntot"), stac_collection("plain")))
  expect_equal(ndc_metadata(c("plain", "lgn"), token = "t")$id, c("plain", "lgn"))  # in the order asked
  expect_error(ndc_metadata(c("lgn", "nope"), token = "t"), "Unknown collection\\(s\\): nope")
  expect_error(ndc_metadata(token = ""), "token is missing")
})

test_that("ndc_metadata copes with a 3D bounding box, a missing extent and no collections", {
  doc <- rich_collection()
  doc$extent$spatial$bbox <- list(c(3, 50, 0, 7, 53, 100))
  bare <- list(type = "Collection", id = "bare", links = list())  # nothing but the required fields
  local_stac_api(collections = list(doc, bare))
  m <- ndc_metadata(token = "t")
  expect_equal(c(m$xmin[1], m$ymin[1], m$xmax[1], m$ymax[1]), c(3, 50, 7, 53))
  expect_true(all(is.na(c(m$xmin[2], m$ymax[2]))))
  expect_true(is.na(m$start[2]) && is.na(m$title[2]) && is.na(m$doi[2]))

  local_stac_api(collections = list())
  m <- ndc_metadata(token = "t")
  expect_equal(nrow(m), 0)
  expect_true(all(c("id", "doi", "citation", "publications", "providers") %in% names(m)))
})
