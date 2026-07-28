# rNDC

This package provides an R-based interface to *NatureDataCube*.

The idea of the *NatureDataCube* is to offer an accessible way for researchers/ecologists to retrieve relevant data.

*NatureDataCube* is a platform based on [*AgroDataCube*](https://agrodatacube.wur.nl/), holding and providing access to data used in the context of project [LTER-LIFE](https://lter-life.nl/en).

## Main R functions

### Via the *NatureDataCube* STAC API (see [`examples_ndc.ipynb`]("tests/examples_ndc.ipynb") and [`examples_stac.ipynb`]("tests/examples_stac.ipynb"))

- [`ndc_get`]("R/ndc_get.R"): Search (and optionally download) data through a custom STAC query
- [`ndc_datasets`]("R/ndc_datasets.R"): List all datasets (optionally constrained by query parameters) in NatureDataCube.
- [`ndc_count`]("R/ndc_count.R"): Obtain a number of items available in NatureDataCube datasets (optionally constrained by query parameters).
- [`ndc_roi`]("R/ndc_roi.R"): Import and transform spatial region of interest.
- [`ndc_trange`]("R/ndc_trange.R"): Convert one or more dates to the RCF3339 format.
- [`assets_download_wcs`]("R/assets_download_wcs.R"): Workaround for downloading STAC Assets coming from WCS servers.

### Via the *AgroDataCube* REST API (see [`examples_adc.ipynb`]("tests/examples_adc.ipynb"))

- [`adc_url`]("R/adc_url.R"): Compose URL text string for submitting data requests through the REST API.
- [`adc_get`]("R/adc_get.R"): Submit requests via REST API.

### Via the *GroenMonitor* WCS GeoServer (see [`examples_gm.ipynb`]("tests/examples_gm.ipynb"))

- [`gm_url`]("R/gm_url.R"): Compose URL text string for submitting data requests through the *GroenMonitor* WCS GeoServer.
- [`gm_get`]("R/gm_get.R"): Submit requests to the *GroenMonitor* WCS GeoServer.

### To be implemented

- Add (advanced) STAC filtering (e.g. post-fetching filtering, CQL2);
- Add a way to easily list available date ranges within items matched with search parameters;
- Add function(s) to deal with weather (point) data;
- Add functions for post-processing (e.g. cropping acquired gridded data to RoI);
- Harmonize functionality across the different data sources, ando also towards using the returned data within Digital Twins platforms (e.g. *NaaVRE*);
- Generally improve all functions.
