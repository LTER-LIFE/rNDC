test_that("numeric vectors become a 4326 bounding box", {
  for (x in list(c(5, 52, 6, 53), c(5L, 52L, 6L, 53L))) {
    r <- ndc_roi(x)
    expect_s3_class(r, "sfc")
    expect_equal(sf::st_crs(r)$epsg, 4326L)
    expect_equal(as.numeric(sf::st_bbox(r)), c(5, 52, 6, 53))
  }
})

test_that("invalid bounding boxes and inputs are rejected", {
  expect_error(ndc_roi(c(1, 2, 3)), "Invalid bounding box")
  expect_error(ndc_roi("does/not/exist.gpkg"), "not found")
  expect_error(ndc_roi(list(1)), "RoI must be")
})

test_that("NULL gives NULL", {
  expect_null(ndc_roi())
  expect_null(ndc_roi(NULL))
})

test_that("missing CRS is assumed to be 4326, other CRS are reprojected", {
  no_crs <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(5, 52))))
  expect_equal(sf::st_crs(ndc_roi(no_crs))$epsg, 4326L)
  expect_equal(sf::st_crs(ndc_roi(sf::st_sfc(sf::st_point(c(5, 52)))))$epsg, 4326L)

  rd <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(155000, 463000)), crs = 28992))
  xy <- sf::st_coordinates(ndc_roi(rd))
  expect_equal(unname(xy[1, ]), c(5.387, 52.155), tolerance = 1e-3)
})

test_that("multiple geometries are combined into one", {
  two <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(5, 52)), sf::st_point(c(6, 53)), crs = 4326))
  expect_length(ndc_roi(two), 1)
})

test_that("files are read", {
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(square_4326(), f, quiet = TRUE)
  expect_equal(as.numeric(sf::st_bbox(ndc_roi(f))), c(0.1, 0.1, 0.9, 0.9))
})
