# Live checks against the real APIs. They are skipped by default (and on CRAN) and run with
#   RNDC_LIVE_TESTS=true NDC_TOKEN=... ADC_TOKEN=... Rscript -e 'devtools::test(filter = "live")'
# They detect changes in the APIs that the offline tests cannot see, such as renamed collections.

skip_unless_live <- function() {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("RNDC_LIVE_TESTS"), "true"), "live API tests are opt-in (RNDC_LIVE_TESTS=true)")
  skip_if(!nzchar(Sys.getenv("NDC_TOKEN")) || !nzchar(Sys.getenv("ADC_TOKEN")), "NDC_TOKEN and ADC_TOKEN are required")
}

test_that("the collections used by the package exist", {
  skip_unless_live()
  expect_true(all(c("lgn", "ntot", "nox", "nh3", "ndvi") %in% ndc_datasets()))
})

test_that("land use and nitrogen items exist for the study sites", {
  skip_unless_live()
  site <- ndc_sites("loobos")
  lgn <- stac_collect_metadata(site, Sys.getenv("NDC_TOKEN"), ndc_endpoint(), "lgn")
  expect_true("2024" %in% lgn$year)
  for (layer in nitrogen_layer_choices) {
    meta <- stac_collect_metadata(site, Sys.getenv("NDC_TOKEN"), ndc_endpoint(), layer)
    expect_true(all(c("2024", "2025") %in% meta$year), info = layer)
  }
})

test_that("rasters can be downloaded and clipped", {
  skip_unless_live()
  site <- ndc_sites("loobos")
  out_dir <- withr::local_tempdir()
  landuse <- get_landuse_raster(site, year = 2024, out_dir = out_dir)
  expect_s4_class(landuse$stack, "SpatRaster")
  nitrogen <- get_nitrogen_raster(site, year = 2025, out_dir = out_dir)
  expect_equal(names(nitrogen$stack), c("ntot_2025", "nox_2025", "nh3_2025"))
})

test_that("AgroDataCube weather data is available", {
  skip_unless_live()
  poly <- "POLYGON((5.7 52.14, 5.79 52.14, 5.79 52.19, 5.7 52.19, 5.7 52.14))"
  station <- get_closest_meteostation(poly, token = Sys.getenv("ADC_TOKEN"))
  expect_true(nzchar(station$closest_id))
  day <- get_meteo_for_date(station$closest_id, "2025-07-15", token = Sys.getenv("ADC_TOKEN"))
  expect_s3_class(day, "sf")
})

test_that("GroenMonitor serves NDVI for a known date and rejects unknown ones", {
  skip_unless_live()
  p <- c(date = "20250715", xmin = 684613, xmax = 685907, ymin = 5763913, ymax = 5764822, format = "tiff")
  res <- gm_get(url = gm_url("NDVI", p), out_path = withr::local_tempfile(fileext = ".tif"))
  expect_equal(httr::status_code(res), 200L)
  p["date"] <- "20251001"
  expect_error(gm_get(url = gm_url("NDVI", p), out_path = withr::local_tempfile(fileext = ".tif")), "HTTP 404")
})

test_that("all pages of a search can be retrieved", {
  skip_unless_live()
  matched <- ndc_count(collection = "lter")
  skip_if(matched < 3, "too few items to page")
  first_page <- suppressWarnings(ndc_get("lter", mode = "sf", limit = 2))
  expect_equal(nrow(first_page), 2)
  all <- ndc_get("lter", mode = "sf", limit = 2, all_pages = TRUE)
  expect_equal(nrow(all), matched)
})

test_that("AgroDataCube results are retrieved page by page", {
  skip_unless_live()
  poly <- "POLYGON((5.70 52.00,5.85 52.00,5.85 52.10,5.70 52.10,5.70 52.00))"  # several hundred fields
  fields <- adc_get_all("Fields", c(geometry = poly, epsg = "4326", year = "2024", output_epsg = "4326"),
                        token = Sys.getenv("ADC_TOKEN"))
  expect_gt(length(fields$features), 100)
})

test_that("monthly NDVI statistics are available for a project polygon", {
  skip_unless_live()
  lter <- ndc_get("lter", mode = "sf", all_pages = TRUE)
  poly <- lter[lter$name == "Loobos", ][1, ]
  stats <- get_ndvi_stats(poly, "ndvi-lter", from = "2024-05-01", to = "2024-07-31")
  expect_gt(nrow(stats), 0)
  expect_setequal(unique(stats$ndc_id), as.character(poly$ndc_id))  # only the polygon itself
  expect_equal(anyDuplicated(stats$month), 0)
  expect_true(all(stats$ndvi_mean > -1 & stats$ndvi_mean < 1))
})

test_that("the years of the land use and nitrogen rasters are known", {
  skip_unless_live()
  expect_true(as.character(ndc_landuse_default_year()) %in% ndc_landuse_years())
  expect_true(all(c("2024", "2025") %in% ndc_nitrogen_years()))
})

