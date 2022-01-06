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

#' Geocoding to Translate Address <-> Coordinates
#'
#' @param search a search string, such as an address. Preference is given to places in the Northern Netherlands using [[crop_certe()].
#' @param api URL of the application programming interface, defaults to OpenStreetMap
#' @details
#' [get_coordinates()] returns a [list] with the latitude and longitude. These can be retrieved with that names (e.g. `output$latitude`) or with the [latitude()]/[longitude()] functions.
#' @name geocoding
#' @rdname geocoding
#' @export
#' @examples
#' get_coordinates("Van Swietenlaan 2, Groningen")
#' coord <- get_coordinates(c("Van Swietenlaan 2, Groningen"),
#'                            "Jelsumerstraat 6, Leeuwarden"))
#' coord
#' 
#' if (require("certeplot2")) {
#'   geo_gemeenten %>%
#'     crop_certe() %>%
#'     plot2() +
#'     geom_sf(aes(x = coord$longitude,
#'                 y = coord$latitude))
#'
#'   geo_gemeenten %>%
#'     crop_certe() %>%
#'     plot2() %>%
#'     add_coordinates(coord, colour = "certeroze")
#' }
get_coordinates <- function(search, api = "https://nominatim.openstreetmap.org/search?q={search}&format=json") {
  check_is_installed("jsonlite")

  out <- list(latitude = numeric(length(search)),
              longitude = numeric(length(search)))
  for (i in seq_len(length(search))) {
    url <- gsub("{search}", search[i], api, fixed = TRUE)
    osm <- jsonlite::fromJSON(url)
    osm <- tryCatch(osm[1, c("lat", "lon", "display_name"), drop = FALSE],
                    error = function(e) NULL)
    if (is.null(osm)) {
      message("No coordinates found for '", search[i], "'")
      out$latitude[i] <- NA_real_
      out$longitude[i] <- NA_real_
    } else {
      message("Found address: ", osm$display_name)
      out$latitude[i] <- as.double(osm$lat)
      out$longitude[i] <- as.double(osm$lon)
    }
  }
  return(out)
}

#' @rdname geocoding
#' @param st_point a valid geometric `st_point` 
#' @param latitude latitude of a GPS location
#' @param longitude longitude of a GPS location
#' @export
get_address <- function(st_point, latitude, longitud, api = "https://nominatim.openstreetmap.org/reverse?lat={latitude}&lon={longitude}") {
  check_is_installed("jsonlite")
  
}
