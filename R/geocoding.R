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

#' Geocoding to Translate Addresses <-> Coordinates
#'
#' @param search a (vector of) search string(s), such as an address or name
#' @param sf_data an 'sf' object or an 'sfc' object (i.e., a vector with geometric `sfc_POINT`s). Can also be a character vector, in which case [get_coordinates()] will be called first.
#' @param api URL of the application programming interface, defaults to OpenStreetMap
#' @details
#' These functions use [OpenStreetMap (OSM)](https://openstreetmap.org).
#'
#' [get_coordinates()] provides geocoding (looking up the coordinates of a place) and returns an 'sfc' geometry object. For the results, preference is given to places in the Northern Netherlands using [[crop_certe()].
#'
#' [get_addresses()] provides reversed-geocoding (looking up the place of coordinates) and returns a [data.frame] with the columns "address", "zipcode" and "city".
#'
#' For both functions, the OSM API will only be called on unique input values, to increase speed. 
#' @name geocoding
#' @rdname geocoding
#' @export
#' @examples
#' # geocoding: retrieve sfc points of addresses
#' sfc <- get_coordinates("Van Swietenlaan 2, Groningen")
#' sfc
#' 
#' # reversed-geocoding: get the addresses
#' get_addresses(sfc)
#' 
#' # or use search parameters:
#' get_addresses(c("Certe NL", "IKEA Groningen"))
#' 
#' coord <- get_coordinates(c("Van Swietenlaan 2, Groningen"),
#'                            "Jelsumerstraat 6, Leeuwarden",
#'                            "Medisch Centrum Leeuwarden"))
#' coord
#' 
#' if (require("certeplot2")) {
#'   geo_gemeenten %>%
#'     crop_certe() %>%
#'     plot2() # %>%
#'     # add_coordinates(coord, colour = "certeroze")
#' }
get_coordinates <- function(search, api = "https://nominatim.openstreetmap.org/search?q={search}&format=json") {
  check_is_installed("jsonlite")
  check_is_installed("sf")

  search_long <- search
  search <- unique(search)

  out <- list(search = search,
              latitude = numeric(length(search)),
              longitude = numeric(length(search)))
  
  for (i in seq_len(length(search))) {
    url <- gsub("{search}", search[i], api, fixed = TRUE)
    osm <- tryCatch({
      result <- jsonlite::fromJSON(url)
      # fair use is 1 per second
      Sys.sleep(0.25)
      # later: prefer Certe region instead of just first row
      result[1, c("lat", "lon", "display_name"), drop = FALSE]
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(osm) || length(osm) == 0 || is.na(osm)) {
      message("No coordinates found for '", search[i], "'")
      out$latitude[i] <- 0 # NA_real_
      out$longitude[i] <- 0 # NA_real_
    } else {
      out$latitude[i] <- as.double(osm$lat)
      out$longitude[i] <- as.double(osm$lon)
    }
  }
  
  out <- as.data.frame(out)
  # 'de-uniquify' the data set
  out <- out[match(search_long, search), , drop = FALSE]
  rownames(out) <- NULL

  # as sf with geometry column and same CRS as included data sets
  out <- sf::st_as_sf(out,
                      coords = c("longitude", "latitude"),
                      crs = "EPSG:4326")$geometry
  # replace lat=0,lon=0 coordinates with valid empty ones
  out[which(vapply(FUN.VALUE = logical(1), out, function(sfc) identical(unclass(sfc), c(0, 0))))] <- sf::st_point()
  out
}

#' @rdname geocoding
#' @importFrom dplyr `%>%` mutate select
#' @export
get_addresses <- function(sf_data, api = "https://nominatim.openstreetmap.org/reverse?lat={latitude}&lon={longitude}&format=json") {
  check_is_installed("jsonlite")
  check_is_installed("sf")
  
  if (NCOL(sf_data) == 1 && is.character(sf_data)) {
    # no coordinates but text instead, do regular geocoding first
    sf_data <- get_coordinates(sf_data)
  }
  
  # get the lat/lon
  if (inherits(sf_data, c("sf", "sfc"))) {
    lat <- latitude(sf_data)
    lon <- longitude(sf_data)
  } else {
    stop("unable to retrieve latitude/longitude")
  }
  coord_long <- paste(lat, lon)
  coord <- unique(coord_long)
  lat <- lat[match(coord, coord_long)]
  lon <- lon[match(coord, coord_long)]
  urls <- rep(api, length(coord))
  out <- data.frame(coord = coord,
                    address = rep(NA_character_, length(coord)),
                    zipcode = rep(NA_character_, length(coord)),
                    city = rep(NA_character_, length(coord)))
  for (i in seq_len(length(coord))) {
    if (is.nan(lat[i])) {
      # missing coordinates
      next
    }
    url <- api
    url <- gsub("{latitude}", lat[i], url, fixed = TRUE)
    url <- gsub("{longitude}", lon[i], url, fixed = TRUE)
    osm <- tryCatch({
      result <- jsonlite::fromJSON(url)$address %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        mutate(address = trimws(paste(road, house_number)))
      # fair use is 1 per second
      Sys.sleep(0.25)
      result$zipcode <- result$postcode
      result[, colnames(result)[which(colnames(result) %in% c("address", "zipcode", "city", "town"))], drop = FALSE]
    }, error = function(e) {
      message("unable to retrieve address: ", e$message)
      return(NULL)
    })
    if (!is.null(osm)) {
      out$address[i] <- osm$address
      out$zipcode[i] <- osm$zipcode
      if ("city" %in% colnames(osm)) {
        out$city[i] <- osm$city
      } else if ("town" %in% colnames(osm)) {
        out$city[i] <- osm$town      
      }
    }
  }
  out <- out[match(coord_long, coord), c("address", "zipcode", "city"), drop = FALSE]
  rownames(out) <- NULL
  out
}
