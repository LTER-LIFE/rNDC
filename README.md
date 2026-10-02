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
| `NDC_TOKEN` | `ndc_get`, `ndc_count`, `ndc_datasets`, `get_landuse_raster`, `get_nitrogen_raster` |
| `ADC_TOKEN` | `adc_get` (and the meteo functions built on it, which take `token` explicitly) |

For example, `Sys.setenv(NDC_TOKEN = "<your token>")`, or put it in your `.Renviron`.

The STAC endpoint defaults to the NatureDataCube test server. To use another one, set `options(rNDC.endpoint = "https://.../api/")` (see [`ndc_endpoint`](R/ndc_endpoint.R)).

## Main R functions

### Via the *NatureDataCube* STAC API (see [`01_getting_started.ipynb`](examples/01_getting_started.ipynb) and [`stac_with_rstac.ipynb`](examples/advanced/stac_with_rstac.ipynb))

- [`ndc_get`](R/ndc_get.R): Search (and optionally download) data through a custom STAC query
- [`ndc_datasets`](R/ndc_datasets.R): List all datasets (optionally constrained by query parameters) in NatureDataCube.
- [`ndc_count`](R/ndc_count.R): Obtain a number of items available in NatureDataCube datasets (optionally constrained by query parameters).
- [`ndc_roi`](R/ndc_roi.R): Import and transform spatial region of interest.
- [`ndc_trange`](R/ndc_trange.R): Convert one or more dates to the RFC 3339 format.
- [`assets_download_wcs`](R/assets_download_wcs.R): Workaround for downloading STAC Assets coming from WCS servers.
- [`ndc_sites`](R/ndc_sites.R): List the bundled LTER-LIFE study sites, or load the boundaries of one of them.

### Thematic rasters via the *NatureDataCube* STAC API

- [`get_landuse_raster`](R/landuse.R): Download the Land Use raster for an area of interest and year, clipped to the area.
- [`get_nitrogen_raster`](R/nitrogen.R): Download the nitrogen rasters (`ntot`, `nox`, `nh3`) for an area of interest and year, clipped to the area.
- `stac_*` helpers ([`stac_raster_helpers.R`](R/stac_raster_helpers.R)): Shared building blocks for the functions above.

### Via the *AgroDataCube* REST API (see [`agrodatacube_rest.ipynb`](examples/advanced/agrodatacube_rest.ipynb))

- [`adc_url`](R/adc_url.R): Compose URL text string for submitting data requests through the REST API.
- [`adc_get`](R/adc_get.R): Submit requests via REST API.
- [`get_closest_meteostation`](R/get_closest_meteostation.R): Find the meteorological station closest to a study area.
- [`get_meteo_for_date`](R/get_meteo_for_date.R), [`get_meteo_for_period`](R/get_meteo_for_period.R), [`get_meteo_for_long_period`](R/get_meteo_for_long_period.R): Get weather data for a station for one day, a period, or a long period split into several requests.

### Via the *GroenMonitor* WCS GeoServer (see [`groenmonitor_wcs.ipynb`](examples/advanced/groenmonitor_wcs.ipynb))

- [`gm_url`](R/gm_url.R): Compose URL text string for submitting data requests through the *GroenMonitor* WCS GeoServer.
- [`gm_get`](R/gm_get.R): Submit requests to the *GroenMonitor* WCS GeoServer.
- [`download_avg_ndvi_month`](R/monthly_ndvi.R), [`download_avg_ndvi_stack`](R/monthly_ndvi_period.R): Compute the average NDVI raster for a month, or a stack of monthly averages over a period.

### Examples

The Jupyter notebooks in [`examples/`](examples) show each interface in use, starting with [`01_getting_started`](examples/01_getting_started.ipynb); see the [examples README](examples/README.md) for an overview. They need network access and the `NDC_TOKEN` and `ADC_TOKEN` environment variables.

### To be implemented

- Add (advanced) STAC filtering (e.g. post-fetching filtering, CQL2);
- Add a way to easily list available date ranges within items matched with search parameters;
- Add functions for post-processing (e.g. cropping acquired gridded data to RoI);
- Harmonize functionality across the different data sources, and also towards using the returned data within Digital Twins platforms (e.g. *NaaVRE*);
- Generally improve all functions.
