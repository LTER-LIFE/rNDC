# rNDC

[![R-CMD-check](https://github.com/LTER-LIFE/rNDC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LTER-LIFE/rNDC/actions/workflows/R-CMD-check.yaml)
[![live-checks](https://github.com/LTER-LIFE/rNDC/actions/workflows/live-checks.yaml/badge.svg)](https://github.com/LTER-LIFE/rNDC/actions/workflows/live-checks.yaml)

This package provides an R-based interface to *NatureDataCube*.

The idea of the *NatureDataCube* is to offer an accessible way for researchers/ecologists to retrieve relevant data.

*NatureDataCube* is a platform based on [*AgroDataCube*](https://agrodatacube.wur.nl/), holding and providing access to data used in the context of project [LTER-LIFE](https://lter-life.nl/en).

## Installation

```r
# install.packages("remotes")
remotes::install_github("LTER-LIFE/rNDC")
```

The package needs R >= 4.1 and the packages listed in `Imports` in [DESCRIPTION](DESCRIPTION).

## Authentication

API tokens are read from environment variables (or can be passed through the `token` argument):

| Variable | Used by |
|---|---|
| `NDC_TOKEN` | `ndc_get`, `ndc_count`, `ndc_datasets`, `ndc_metadata`, `ndc_citation`, `stac_availability`, `ndc_landuse_raster`, `ndc_nitrogen_raster`, `ndc_ahn_raster`, `ndc_ahn_stats`, `ndc_soil_raster`, `ndc_soil_legend`, `ndc_ndvi_stats`, `ndc_ndvi_month`, `ndc_ndvi_stack`, `ndc_landuse_years`, `ndc_nitrogen_years` |
| `ADC_TOKEN` | `adc_get`, `adc_get_all`, `adc_weather_station`, `adc_weather_date`, `adc_weather_period`, `adc_weather_long_period` |

For example, `Sys.setenv(NDC_TOKEN = "<your token>")`, or put it in your `.Renviron`. A missing or empty token stops the function at once, naming the variable to set.

The STAC endpoint defaults to the NatureDataCube test server. To use another one, set `options(rNDC.endpoint = "https://.../api/")` (see [`ndc_endpoint`](R/ndc_endpoint.R)).

## Main R functions

The function names start with the source of the data (`ndc_` for the NatureDataCube, `adc_` for the AgroDataCube, `gm_` for the GroenMonitor, `stac_` for the building blocks), and an argument has the same name everywhere: the region is `roi`, a period is `trange` (two dates, `NA` for an open side), a weather station is `station`, and the output goes to `output_dir` or `output_path`. Before version 0.5.2 some functions were called differently (`get_landuse_raster()`, `get_meteo_for_period()`, `download_avg_ndvi_month()`, ...): the old names still work, with a warning, see `?"rNDC-deprecated"` and [NEWS.md](NEWS.md).

### Via the *NatureDataCube* STAC API (see [`01_getting_started.ipynb`](examples/01_getting_started.ipynb) and [`stac_with_rstac.ipynb`](examples/advanced/stac_with_rstac.ipynb))

- [`ndc_get`](R/ndc_get.R): Search (and optionally download) data through a custom STAC query (with `all_pages = TRUE`, the modes that convert or download use all the pages of results, not only the first). The retrieval functions can record their parameters (`save_params = "file.json"`, never the token) and run again from the file (`params_file = "file.json"`); whatever you give overrides the file. [`ndc_repeat`](R/ndc_repeat.R) runs such a file without knowing its function.
- [`ndc_datasets`](R/ndc_datasets.R): List all datasets (optionally constrained by query parameters, or limited to some `collections`) in NatureDataCube.
- [`ndc_metadata`](R/ndc_metadata.R), [`ndc_citation`](R/ndc_citation.R): Read the metadata of the datasets (title, license, extent, providers, and the fields of STAC extensions such as the citation and DOI of the Scientific extension), and write the reference of a dataset from it.
- [`ndc_available`](R/ndc_available.R): List the datasets that have data for an area and/or a period: the STAC collections (checked against their items) and the datasets of other services ([`ndc_external_datasets`](R/ndc_external.R), with the coverage that is known for them; [`ndc_netherlands`](R/ndc_external.R) is the outline of the country).
- [`stac_availability`](R/stac_availability.R): Which collections have items for an area, and for which years, with one search over all of them (e.g. "is there land use or nitrogen data here?").
- [`ndc_count`](R/ndc_count.R): Obtain a number of items available in NatureDataCube datasets (optionally constrained by query parameters).
- [`ndc_roi`](R/ndc_roi.R): Import and transform spatial region of interest (for a file with several layers, such as the bundled study sites, choose one with `layer`).
- [`ndc_trange`](R/ndc_trange.R): Convert one or more dates to the RFC 3339 format.
- [`assets_download_wcs`](R/assets_download_wcs.R): Workaround for downloading STAC Assets coming from WCS servers.
- [`ndc_sites`](R/ndc_sites.R): List the bundled LTER-LIFE study sites, or load the boundaries of one of them.

### Thematic rasters via the *NatureDataCube* STAC API

- [`ndc_landuse_raster`](R/ndc_landuse.R): Download the Land Use raster for an area of interest and year, clipped to the area.
- [`ndc_nitrogen_raster`](R/ndc_nitrogen.R): Download the nitrogen rasters (`ntot`, `nox`, `nh3`) for an area of interest and year, clipped to the area.
- [`ndc_ahn_raster`](R/ndc_ahn.R), [`ndc_ahn_stats`](R/ndc_ahn.R): Download the AHN height rasters (terrain height `dtm` and object height `ohn`, in metres) for an area of interest, clipped to the area, or summarise the heights in the area (area, minimum, maximum, mean and standard deviation). An area with very many cells is requested at a lower resolution.
- [`ndc_soil_raster`](R/ndc_soil.R), [`ndc_soil_legend`](R/ndc_soil.R): Download the soil map (_Bodemkaart_) raster for an area of interest, and read the names and colours of its soil types.
- [`ndc_landuse_years`, `ndc_nitrogen_years`, `ndc_landuse_default_year`, `ndc_nitrogen_layers`, `ndc_landuse_collection`](R/ndc_defaults.R): The years for which these rasters exist (read from the STAC API), and their defaults.
- [`ndc_ndvi_stats`](R/ndc_ndvi_stats.R): Monthly NDVI statistics per polygon from the `ndvi-lter` / `ndvi-snl` collections.
- [`ndc_ndvi_month`](R/ndc_ndvi.R), [`ndc_ndvi_stack`](R/ndc_ndvi.R): Monthly average NDVI rasters, found through the `ndvi` collection: the items are the daily *GroenMonitor* coverages (the same rasters as `gm_ndvi_month` / `gm_ndvi_stack`), so only the days with data are downloaded.
- `stac_*` helpers ([`stac_metadata.R`](R/stac_metadata.R), [`stac_wcs.R`](R/stac_wcs.R), [`stac_raster.R`](R/stac_raster.R)): Shared building blocks for the functions above (`stac_collect_metadata`, `stac_year_trange`, `stac_wcs_subset_suffix`, `stac_download_one`, `stac_clip_raster_to_aoi`, `stac_make_headers`).

### Via the *AgroDataCube* REST API (see [`agrodatacube_rest.ipynb`](examples/advanced/agrodatacube_rest.ipynb))

- [`adc_url`](R/adc_url.R): Compose URL text string for submitting data requests through the REST API.
- [`adc_get`](R/adc_get.R): Submit requests via REST API.
- [`adc_get_all`](R/adc_get_all.R): Submit a request whose results are paged (e.g. `Fields`) and combine all the pages.
- [`adc_ndvi`](R/adc_ndvi.R): NDVI statistics (mean and standard deviation) of agricultural fields on each date with an observation, for the fields in an area (an `sf` polygon) or for field ids.
- [`adc_weather_station`](R/adc_weather.R): Find the meteorological station closest to a study area.
- [`adc_weather_date`](R/adc_weather.R), [`adc_weather_period`](R/adc_weather.R), [`adc_weather_long_period`](R/adc_weather.R): Get weather data for a station for one day, a period, or a long period split into several requests.

### Via the *GroenMonitor* WCS GeoServer (see [`groenmonitor_wcs.ipynb`](examples/advanced/groenmonitor_wcs.ipynb))

- [`gm_url`](R/gm_url.R): Compose URL text string for submitting data requests through the *GroenMonitor* WCS GeoServer.
- [`gm_get`](R/gm_get.R): Submit requests to the *GroenMonitor* WCS GeoServer.
- [`gm_ndvi_month`](R/gm_ndvi.R), [`gm_ndvi_stack`](R/gm_ndvi.R): Compute the average NDVI raster for a month, or a stack of monthly averages over a period.

### Long downloads

- [`ndc_with_progress`](R/ndc_progress.R): Follow, or stop, functions that make many requests (`adc_weather_long_period`, `gm_ndvi_month`, `gm_ndvi_stack`): they report the request that starts to a `report` function, and stop with an error of class `rNDC_interrupted` when an `interrupt` function says so. Used by [rNDC.Shiny](https://github.com/LTER-LIFE/rNDC-Shiny) for its progress bar.

### Examples

The Jupyter notebooks in [`examples/`](examples) show each interface in use, starting with [`01_getting_started`](examples/01_getting_started.ipynb); see the [examples README](examples/README.md) for an overview. They need network access and the `NDC_TOKEN` and `ADC_TOKEN` environment variables.

### ToDo

- Migrate to `httr2`;
- Generate a vignette or pkgdown site;
- Harmonize functionality across the different data sources, and also towards using the returned data within Digital Twins platforms (e.g. *NaaVRE*);
- Generally improve all functions.

## Citation

See [CITATION.cff](CITATION.cff)
