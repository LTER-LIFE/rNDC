# Examples

Jupyter notebooks (R kernel) showing how to use rNDC. They need network access and API tokens in environment variables (see the main [README](../README.md#authentication)). The saved outputs were generated against the live APIs; they are refreshed when the code or the APIs change.

## Start here

| Notebook | What it shows | Tokens | Runtime |
|---|---|---|---|
| [`01_getting_started`](01_getting_started.ipynb) | Find datasets, define regions and dates of interest, search items and get metadata from *NatureDataCube*: `ndc_roi`, `ndc_trange`, `ndc_datasets`, `ndc_count`, `ndc_get` | `NDC_TOKEN` | ~1 min |
| [`02_rasters`](02_rasters.ipynb) | Study sites, land use and nitrogen rasters for an area of interest: `ndc_sites`, `get_landuse_raster`, `get_nitrogen_raster` | `NDC_TOKEN` | ~1 min |
| [`03_weather`](03_weather.ipynb) | KNMI weather data from *AgroDataCube*: `get_closest_meteostation`, `get_meteo_for_date`, `get_meteo_for_period`, `get_meteo_for_long_period` | `ADC_TOKEN` | ~30 s |
| [`04_ndvi`](04_ndvi.ipynb) | Monthly average NDVI from *GroenMonitor*: `download_avg_ndvi_month`, `download_avg_ndvi_stack` | `NDC_TOKEN` (last section) | ~2 min |

## Advanced: the raw interfaces

The functions above are built on these interfaces. Use the notebooks below when you need functionality that the wrapper functions do not offer.

| Notebook | What it shows | Tokens | Runtime |
|---|---|---|---|
| [`advanced/stac_with_rstac`](advanced/stac_with_rstac.ipynb) | The *NatureDataCube* STAC API through `rstac` | `NDC_TOKEN` | ~1 min |
| [`advanced/agrodatacube_rest`](advanced/agrodatacube_rest.ipynb) | The *AgroDataCube* REST API: `adc_url`, `adc_get` | `ADC_TOKEN` | ~1 min |
| [`advanced/groenmonitor_wcs`](advanced/groenmonitor_wcs.ipynb) | The *GroenMonitor* WCS server: `gm_url`, `gm_get` | none | seconds |

## Running them

`library(rNDC)` in the notebooks loads the *installed* package: reinstall it after changing the code. To run all notebooks (or some of them) as plain R scripts against the working tree, stopping at the first error:

```sh
Rscript tools/run_notebooks.R                      # all notebooks
Rscript tools/run_notebooks.R 02_rasters 04_ndvi   # selected notebooks
```

The same script runs weekly in GitHub Actions (see [`.github/workflows/live-checks.yaml`](../.github/workflows/live-checks.yaml)), to catch changes in the APIs.
