# rNDC 0.5

## New

* `ndc_get()` gains `all_pages`: the modes that convert or download only the first page (`tibble`, `sf`, `sfc`,
  `download`, `download_wcs`) can first retrieve all the pages of a search. Without it they warn when the first page
  is incomplete.
* `adc_get_all()` retrieves all the pages of a paged AgroDataCube request (e.g. `Fields`).
* `get_ndvi_stats()` summarises the `ndvi-*` collections per polygon and month.
* `ndc_landuse_years()`, `ndc_nitrogen_years()`, `ndc_landuse_collection()`, `ndc_landuse_default_year()` and
  `ndc_nitrogen_layers()` give the defaults of the land use and nitrogen rasters, and the years that exist.
* `ndc_with_progress()` reports the progress of the functions that make many requests (`get_meteo_for_long_period()`,
  `download_avg_ndvi_month()`, `download_avg_ndvi_stack()`), and lets the caller stop them.
* `ndc_roi()` gains `layer`, to choose a layer of a file with several layers (such as the bundled
  `study_sites.gpkg`). Without it the first layer is used, with a warning if there are more.
* `stac_year_trange()` builds a time range from years, and `stac_collect_metadata()` gains `trange`.

## Changes

* `ndc_landuse_years()` and `ndc_nitrogen_years()` read the years from the STAC items (they were fixed lists); they
  take a `token`, and stop if the API cannot be reached.
* `get_landuse_raster()` and `get_nitrogen_raster()` send the year to the STAC API as a time range, instead of
  retrieving every item of the area and filtering afterwards. The result is the same (checked against the live
  collections); the local year filter remains as a safeguard.
* `get_landuse_raster()` no longer filters the items by the `lgn` keyword (it only searches the `lgn` collection by
  default). The columns `layer_lower`, `title_lower`, `keyword_match`, `layer_match` and `title_match` are gone from
  its `metadata`.
* `ndc_get()`, `ndc_datasets()`, `ndc_count()`, `adc_get()` and the raster functions stop at once, naming the
  environment variable (`NDC_TOKEN` or `ADC_TOKEN`), when the token is missing or empty, instead of sending a
  request that fails with an opaque 401. The AgroDataCube health check needs no token.
* `download_avg_ndvi_month()` and `download_avg_ndvi_stack()` share their code, which now downloads through
  `gm_get()`. Only a day for which GroenMonitor has no coverage (HTTP 404) is skipped; server and connection errors
  are retried twice and then raised with the message of the server, where they used to be mistaken for a day
  without data and left out of the average. The messages for each day are replaced by the progress reports. A
  month without any valid NDVI is left out of the stack.
* Errors of the services are of class `rNDC_http_error`, with the HTTP `status`.
* `stac_keywords_to_vec()`, `stac_feature_meta()`, `stac_bbox_in_crs()`, `stac_make_file_prefix()` and
  `stac_build_stack()` are no longer exported; they are internal helpers of the raster functions.
* The documentation of `get_landuse_raster()` and `get_nitrogen_raster()` describes the area of interest and the
  value they return.

## Fixes

* `assets_download_wcs()` failed when it downloaded more than one asset (after downloading all of them), did not
  check the status of the responses, and left the body of an error as a `.tif` file. It now checks each response,
  removes the file of a failed download, creates the output folder recursively, and returns the response (one
  asset) or a list of responses.
* `adc_url()` did not encode `&`, `=` and `/` in the values of the parameters, so that a value with an `&` added a
  parameter. An unknown `option` is an error (it put `NA` in the URL).
* The NDVI functions named their temporary folder after the second of the call, so that two calls in the same second
  of one R process shared a folder, and the first to finish removed the tiles of the other. The folders are unique.

# rNDC 0.4

The first release of the fork maintained for LTER-LIFE. The package is now only the R interface: the Shiny app and
the Docker files moved to [rNDC-Shiny](https://github.com/LTER-LIFE/rNDC-Shiny), and `ndc_shiny()` is gone.

## New

* `ndc_endpoint()` gives the URL of the NatureDataCube STAC API (the test server by default); the option
  `rNDC.endpoint` overrides it. No function has the URL written in it any more.
* Manual pages for every exported function, generated with roxygen2; `NAMESPACE` is generated as well.
* Offline tests with `testthat` (HTTP is stubbed with `webmockr`), live tests that run only on request
  (`RNDC_LIVE_TESTS=true`), and the GitHub Actions `R-CMD-check` (offline, on every push) and `live-checks` (weekly,
  with the API tokens).
* Example notebooks reorganised in `examples/` (`01` to `04` for the high-level functions, `advanced/` for the raw
  STAC, REST and WCS interfaces), with `tools/run_notebooks.R` to run them as plain R.
* `CITATION.cff`, `aidecl.yaml` (declaration of the use of AI tools) and `CLAUDE.md`.

## Changes

* `get_landuse_raster()` and `get_nitrogen_raster()` are built on the shared `stac_*` helpers
  (`stac_collect_metadata()`, `stac_download_one()` with retries and a minimum file size, `stac_wcs_subset_suffix()`,
  `stac_clip_raster_to_aoi()`, `stac_make_headers()`, ...), and take the area of interest in any form that
  `ndc_roi()` accepts (`stac_normalize_aoi()` is gone).
* The package no longer calls `library()` or imports whole packages: the functions it uses are listed in
  `R/rNDC-package.R`, and `DESCRIPTION` lists the dependencies (R >= 4.1), the authors and the licence.
* The rasters are read from the current STAC layout: land use from the collection `lgn`, and nitrogen from one
  collection per layer (`ntot`, `nox`, `nh3`), instead of the single collection `ndc-geoserver-rasters`. The
  token of the raster functions is `NDC_TOKEN` (it was `NDC_NATURE_TOKEN`).
* `ndc_get()` has the modes `fetch` (all pages) and `download_wcs`, checks `mode` with `match.arg()`, converts
  `trange` with `ndc_trange()`, and only adds the area and period to the search when they are given.
* `ndc_roi()`: a numeric vector of four numbers is a bounding box in EPSG:4326 (any other length is an error), a
  path is read as a file (an error if it does not exist), a missing CRS is taken as EPSG:4326, other CRS are
  transformed, and several geometries are combined into one, since the STAC search takes one.
* `ndc_trange()` is exported, and `as_rcf3339()` is renamed `as_rfc3339()` (it was a typo). `ndc_trange()` accepts
  `Date`, `POSIXct` and character values, leaves a side open for `NA`, passes a value that is a STAC `datetime`
  already, and returns `NA` for an empty, all-`NA` or too long input.
* `ndc_sites()` finds the bundled `study_sites.gpkg` of this package (it looked in `NatureDataCubeR`), reads only the
  site that is asked for, and stops with the available sites for an unknown name.
* `adc_get()` returns the parsed content of the response, and stops with the message of the server when the request
  fails (also when the response is saved to a file, which is then removed). An unknown `server` is an error.
  `gm_get()` stops with the message of the server too, and removes the file it saved.
* `get_closest_meteostation()` measures the distances in EPSG:28992 (it used EPSG:3857, which distorts them) and
  asks for all the stations in one request. `get_meteo_for_date()` uses the parameter `stationid` only and no longer
  hides the errors of the API. The weather functions format the page parameters without scientific notation and
  no longer warn about the observations having no geometry.
* The weather functions, `split_date_range()`, `download_avg_ndvi_month()`, `download_avg_ndvi_stack()`,
  `gm_get()`, `gm_url()` and the other exported functions have manual pages.
* `ndc_datasets()` passes the token on when it counts the items (it used the `NDC_TOKEN` of the environment instead
  of the one it was given).
* `assets_download_wcs()` honours `overwrite` and creates the output folder.

## Removed

* The Shiny app (`inst/shiny`), the Dockerfile and compose files, and the tutorial notebook (they are in rNDC-Shiny).
* The export of the operator `%||%`, which is an internal helper.
* The nitrogen-specific copies of the header, AOI, download and clipping helpers, and the user-interface helper
  `nitrogen_controls_ui()`, which belonged to the app.

# rNDC 0.3 and earlier

The package before the fork (tags `v0.1` and `v0.2`, February to June 2026), when it was developed together with the
Shiny app.

* Functions for the three sources: `adc_get()` and `adc_url()` for the AgroDataCube REST API, `gm_get()` and
  `gm_url()` for the GroenMonitor WCS server, and `ndc_get()` for the NatureDataCube STAC API (February 2026).
* The `ndc_*` family (June 2026): `ndc_get()` with its output modes (items, tibble, sf, sfc, download; a separate
  `ndc_search()` was merged into it), `ndc_count()`, `ndc_datasets()`, `ndc_roi()`, `ndc_trange()` and `ndc_sites()`,
  with the bundled study sites.
* The weather functions (`get_closest_meteostation()`, `get_meteo_for_date()`, `get_meteo_for_period()`,
  `get_meteo_for_long_period()`), the monthly NDVI functions of GroenMonitor (`download_avg_ndvi_month()` and
  `download_avg_ndvi_stack()`, which aligns the monthly means that the WCS returns on different grids), and the land
  use and nitrogen raster functions.
* The Shiny app was merged into the repository (and `launch_app()` became `ndc_shiny()`); a Docker image ran it.
