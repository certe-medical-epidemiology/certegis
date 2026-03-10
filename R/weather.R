# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Get Hourly/Daily Weather from KNMI Through Open-Meteo
#'
#' Retrieves hourly weather data from the [Open-Meteo API](https://open-meteo.com/)
#' using the KNMI HARMONIE AROME weather model via the `/v1/forecast` endpoint.
#' The KNMI model provides high-resolution forecasts (2 km for the Netherlands,
#' 5.5 km for Europe) updated hourly. The `"knmi_seamless"` model (default)
#' blends KNMI HARMONIE AROME with ECMWF IFS beyond the 2.5-day KNMI horizon.
#'
#' @param latitude Latitude in decimal degrees (WGS84). Default: `53.22` (Groningen). Use negative values for the Southern Hemisphere.
#' @param longitude Longitude in decimal degrees (WGS84). Default: `6.57` (Groningen). Use negative values for locations west of Greenwich.
#' @param metrics Character vector of one or more hourly weather metrics to retrieve (default: `"temperature_2m"`). Multiple metrics can be specified, e.g. `c("temperature_2m", "wind_speed_10m")`. Common metrics include:
#'   - `"temperature_2m"`: air temperature at 2 m above ground (degrees Celsius)
#'   - `"relative_humidity_2m"`: relative humidity at 2 m (%)
#'   - `"precipitation"`: rain + snow total (mm)
#'   - `"wind_speed_10m"`: wind speed at 10 m (km/h)
#'   - `"cloud_cover"`: total cloud cover (%)
#'
#'   See the [KNMI API documentation](https://open-meteo.com/en/docs/knmi-api) for the full list of available variables.
#' @param past_days Integer number of past days to include (default: `30`, range: 0--92).
#' @param forecast_days Integer number of forecast days to include (default: `0`, range: 0--16). The native KNMI model provides up to 2.5 days; beyond that, the seamless model falls back to ECMWF IFS.
#' @param model Character string specifying the weather model. One of
#'   `"knmi_seamless"` (default, KNMI + ECMWF blend),
#'   `"knmi_harmonie_arome_europe"` (5.5 km, Central & Northern Europe), or
#'   `"knmi_harmonie_arome_netherlands"` (2 km, Netherlands & Belgium).
#' @param timezone Character string for the timezone of returned timestamps (default: `"Europe/Amsterdam"`).
#' @param api_url Base URL of the Open-Meteo API endpoint.
#' @return A [data.frame] with columns:
#' - `time`: `POSIXct` timestamp in the requested timezone
#' - One column per requested weather variable, named as given in `metrics`
#' @name weather
#' @rdname weather
#' @export
#' @examples
#' \dontrun{
#' # temperature in Groningen over the past 30 days
#' get_weather_hourly()
#' get_weather_daily() # default: between 08:00-17:59
#' 
#' # wind speed with 3-day forecast
#' get_weather_hourly(metrics = "wind_speed_10m", past_days = 7, forecast_days = 3)
#'
#' # multiple variables at once
#' get_weather_hourly(metrics = c("temperature_2m", "relative_humidity_2m"))
#'
#' # high-resolution Netherlands-only model
#' get_weather_hourly(model = "knmi_harmonie_arome_netherlands",
#'                    past_days = 0,
#'                    forecast_days = 2)
#' }
get_weather_hourly <- function(latitude = 53.22,
                        longitude = 6.57,
                        metrics = "temperature_2m",
                        past_days = 30,
                        forecast_days = 0,
                        model = "knmi_seamless",
                        timezone = "Europe/Amsterdam",
                        api_url = "https://api.open-meteo.com/v1/forecast") {
  
  check_is_installed("httr")
  check_is_installed("jsonlite")
  
  model <- match.arg(model,
                     choices = c("knmi_seamless",
                                 "knmi_harmonie_arome_europe",
                                 "knmi_harmonie_arome_netherlands"))
  stopifnot(is.numeric(latitude),
            is.numeric(longitude),
            is.numeric(past_days),
            is.numeric(forecast_days),
            past_days >= 0, past_days <= 92,
            forecast_days >= 0, forecast_days <= 16,
            is.character(metrics), length(metrics) >= 1)
  
  metrics_collapsed <- paste(metrics, collapse = ",")
  
  url <- paste0(api_url, "?",
                "latitude=", latitude, "&",
                "longitude=", longitude, "&",
                "hourly=", metrics_collapsed, "&",
                "models=", model, "&",
                "timezone=", timezone, "&",
                "past_days=", past_days, "&",
                "forecast_days=", forecast_days)
  
  response <- httr::GET(url)
  if (httr::http_error(response)) {
    body <- httr::content(response, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE),
                       error = function(e) NULL)
    reason <- parsed[["reason"]] %||% httr::http_status(response)[["message"]]
    stop("Open-Meteo API request failed: ", reason, call. = FALSE)
  }
  
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(content, simplifyVector = TRUE)
  
  hourly <- parsed[["hourly"]]
  
  out <- data.frame(
    time = as.POSIXct(hourly[["time"]],
                      format = "%Y-%m-%dT%H:%M",
                      tz = timezone),
    stringsAsFactors = FALSE
  )
  
  for (var in metrics) {
    if (is.null(hourly[[var]])) {
      warning("Variable '", var, "' not found in API response, skipping.",
              call. = FALSE)
      next
    }
    out[[var]] <- as.double(hourly[[var]])
  }
  
  out
}

#' @param min_hour First hour of the day to include on a `0-23` scale. Default: `8`.
#' @param max_hour Last hour of the day to include on a `0-23` scale. Default: `18`.
#' @param fun Function to use for calculation, defaults to [mean()].
#' @param ... Argument passed on to `get_weather_hourly()`.
#' @importFrom dplyr mutate filter group_by summarise across all_of everything
#' @rdname weather
#' @export
get_weather_daily <- function(min_hour = 8,
                              max_hour = 18,
                              fun = base::mean,
                              ...) {
  
  stopifnot(is.numeric(min_hour), is.numeric(max_hour),
            min_hour >= 0, min_hour <= 23,
            max_hour >= 0, max_hour <= 23,
            min_hour <= max_hour)
  
  hourly <- get_weather_hourly(...)
  
  hourly |>
    mutate(date = as.Date(time),
           hour = as.integer(format(time, "%H"))) |>
    filter(hour >= min_hour, hour <= max_hour) |>
    select(-hour, -time) |>
    group_by(date) |>
    summarise(across(everything(), function(x) fun(x, na.rm = TRUE)),
              .groups = "drop") |>
    as.data.frame()
}
