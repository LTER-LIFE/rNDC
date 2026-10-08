#' Compose _AgroDataCube_ URL
#'
#' Compose URL text string to submit requests through _AgroDataCube_ REST APIs.
#'
#' @param option character. Determines the type of request.
#' @param params vector or list. List of named parameters.
#' @param base_url character. Base URL for the REST API.
#' @returns A request URL text string.
#' @export

adc_url <- function(option, params, base_url = "https://agrodatacube.wur.nl/api/v2/rest/") {

  # Map options
  data_options <- c("Health_check"      = "lifeprobe",
                    "Fields"            = "fields",
                    "AHN"               = "ahn",
                    "AHN_image"         = "ahn_image",
                    "Meteo_stations"    = "meteostations",
                    "Meteo_data"        = "meteodata",
                    "Soilparams"        = "soilparams",
                    "Soiltypes"         = "soiltypes",
                    "NDVI"              = "ndvi",
                    "NDVI_image"        = "ndvi_image",
                    "Cropcodes"         = "codes/cropcodes",
                    "Soilcodes"         = "codes/soilcodes",
                    "Cropcategory"      = "codes/category",
                    "Municipalities"    = "regions/municipalities",
                    "Postalcodes"       = "regions/postalcodes",
                    "Provinces"         = "regions/provinces",
                    "KPI_Greenness"     = "datapackage/kpi/greenness",
                    "KPI_Croprotation"  = "datapackage/kpi/croprotation")

  if (length(option) != 1 || !option %in% names(data_options)) {
    stop("Unknown `option`: use one of ", paste(names(data_options), collapse = ", "), ".", call. = FALSE)
  }

  # Compose root URL
  if ((option == "NDVI") && ("fieldid" %in% names(params))) {
    root_url <- paste0(base_url, "fields/", params["fieldid"], "/", data_options[option])
    params <- params[-which(names(params) == "fieldid")]
  } else {
    root_url <- paste0(base_url, data_options[option])
  }
  
  # Format parameters
  if (!missing(params)) {

    # Remove NAs
    params_nona <- lapply(na.omit(params),
                          FUN = function(x) URLencode(x, reserved = TRUE))

    # Format parameters for weather data
    if ((option == "Meteo_stations") && ("meteostation" %in% names(params_nona))) {
      params_ms <- params_nona["meteostation"]
      params_nona <- params_nona[-which(names(params_nona) == "meteostation")]
      ms_url <- paste0("/", as.character(params_ms))
    } else {
      ms_url <- ""
    }
    
    # Generate URL with parameters
    params_url <- paste(names(params_nona), params_nona, sep = "=", collapse = "&")
  
  } else {
    
    # In case no parameters are provided
    params_url <- ""
  }
  
  # Compose final URL
  if (option == "Health_check") {
    url <- root_url
  } else {
    url <- paste0(root_url, ms_url, "?", params_url)
  }
  return(url)
}
