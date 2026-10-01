# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`rNDC` is an R package (Apache-2.0, fork maintained for LTER-LIFE) with wrappers for retrieving data from LTER-LIFE's **NatureDataCube** (a STAC API) plus two related sources: **AgroDataCube** (REST) and **GroenMonitor** (WCS GeoServer). Unqualified calls to imported packages must be listed as `importFrom(pkg, fn)` in [NAMESPACE](NAMESPACE) (no blanket `import()`, to avoid masking warnings); otherwise use `pkg::fn`. `DESCRIPTION` `Imports` lists the packages.

## Commands

There is no test suite, linter or CI. `tests/` holds example Jupyter notebooks (R kernel), one per data source (`examples_ndc`, `examples_stac`, `examples_adc`, `examples_gm`); they are the only usage examples/smoke tests and need network access plus tokens.

- Load during development: `devtools::load_all()` (or `R CMD INSTALL .`)
- Docs/NAMESPACE: roxygen comments exist but **NAMESPACE is maintained by hand** (no `Roxygen` field in DESCRIPTION, no `man/`). When adding or removing an exported function, edit [NAMESPACE](NAMESPACE) manually.
- Credentials come from env vars: `NDC_TOKEN` (default for `ndc_get`, landuse and nitrogen functions). AgroDataCube functions take `token` explicitly.

## Architecture

Code lives in flat `R/*.R` files, grouped by data source:

1. **Generic NDC STAC client** (`ndc_*.R`, `assets_download_wcs.R`): `ndc_get()` builds an rstac search against the endpoint `https://ndc-test.containers.wur.nl/api/` (hard-coded), with a Bearer token. Its `mode` argument selects the output: raw items, `fetch`, `tibble`, `sf`, `sfc`, `download`, or `download_wcs`. `ndc_roi()` normalizes a RoI (bbox numeric / file path / sf) to EPSG:4326; `ndc_trange()`/`as_rfc3339()` format dates; `ndc_datasets()`/`ndc_count()` list collections and item counts. STAC assets backed by WCS servers need `assets_download_wcs()`.
2. **Thematic raster retrievers** (`landuse.R`, `nitrogen.R`): high-level `get_landuse_raster()` / `get_nitrogen_raster()` take an AOI, year (and layers), query the `ndc-geoserver-rasters` collection, filter items by keyword/layer/year, download the WCS asset with a subset in EPSG:32631, clip to the AOI and return a `terra` stack. Each file has its own config constants at the top (endpoint, collection, keyword, year choices).
3. **Shared STAC raster helpers** (`stac_raster_helpers.R`): the intended common layer (`stac_collect_metadata`, `stac_download_one` with retries/min-size check, `stac_wcs_subset_suffix`, `stac_clip_raster_to_aoi`, `stac_build_stack`, plus the `%||%` operator). `landuse.R` uses it; `nitrogen.R` still carries duplicated `nitrogen_*` copies of the same logic (header, metadata, download, clip); AOI normalisation in both goes through `ndc_roi()`. Prefer the `stac_*` helpers for new code and consider migrating nitrogen onto them.
4. **AgroDataCube REST** (`adc_url.R`, `adc_get.R`) plus meteo helpers built on top (`get_closest_meteostation`, `get_meteo_for_date/period/long_period`, `split_date_range`). `long_period` splits a range into chunks of `by_days` and `rbind`s the sf results.
5. **GroenMonitor WCS** (`gm_url.R`, `gm_get.R`, `monthly_ndvi.R`, `monthly_ndvi_period.R`): `download_avg_ndvi_month()` downloads one daily NDVI coverage per day (`groenmonitor__ndvi_YYYYMMDD`) from `data.groenmonitor.nl` and averages them; `download_avg_ndvi_stack()` loops over months.

Bundled data is in `inst/extdata/` (`study_sites.gpkg` with one layer per site, `nl.gpkg`, `veluwe.gpkg`).

## Gotchas

- Some functions are not exported in NAMESPACE (e.g. `get_nitrogen_stats`, `nitrogen_*` internals); check NAMESPACE before assuming a function is public.
- `aidecl.yaml` is an AI-usage declaration for the project; update it if AI tool usage changes.
