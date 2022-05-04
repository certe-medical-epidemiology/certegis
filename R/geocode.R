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

#' Geocoding to Find Coordinates and Addresses
#'
#' **Geocoding** is the process of retrieving geographic coordinates based on text, such as an address or the name of a place ([Wikipedia page](https://en.wikipedia.org/wiki/Address_geocoding)). On the other hand, **reverse geocoding** is the process of retrieving the name and address from geographic coordinates ([Wikipedia page](https://en.wikipedia.org/wiki/Reverse_geocoding)).
#' @param place a (vector of) names or addresses of places
#' @param sf_data an 'sf' object or an 'sfc' object (i.e., a vector with geometric `sfc_POINT`s). Can also be a character vector, in which case [geocode()] will be called first.
#' @param as_coordinates a [logical] to indicate whether the result should be returned as coordinates (i.e., class `sfc_POINT`)
#' @param only_netherlands a [logical] to indicate whether only Dutch places should be searched
#' @details
#' These functions use [OpenStreetMap (OSM)](https://openstreetmap.org).
#'
#' [geocode()] provides geocoding and returns an 'sf' [data.frame] at default. In case of multiple results, the distance from the main Certe building in Groningen is leading.
#'
#' [reverse_geocode()] provides reversed geocoding and returns a [data.frame] with the columns "name", "address", "zipcode" and "city".
#'
#' For both functions, the OSM API will only be called on unique input values, to increase speed. 
#' @source Data © OpenStreetMap contributors, ODbL 1.0. <https://osm.org/copyright>
#' @name geocoding
#' @rdname geocoding
#' @export
#' @examples
#' # geocoding: retrieve 'sf' data.frame based on place names
#' coord <- geocode("Van Swietenlaan 2, Groningen")
#' coord
#' 
#' # reverse geocoding: get the name and address
#' reverse_geocode(coord)
#' 
#' # places can be any text, and the results are prioritised based on
#' # the distance from the main Certe building, so:
#' reverse_geocode(c("Certe", "IKEA"))
#' 
#' hospitals <- geocode(c("Martini ziekenhuis",
#'                        "Medisch Centrum Leeuwarden",
#'                        "Tjongerschans Heerenveen",
#'                        "Scheper Emmen"))
#' hospitals
#' 
#' if (require("certeplot2")) {
#'   geo_gemeenten |>
#'     crop_certe() |>
#'     plot2(datalabels = FALSE) |>
#'     add_sf(hospitals, colour = "certeroze", datalabels = place)
#' }
geocode <- function(place, as_coordinates = FALSE, only_netherlands = TRUE) {
  check_is_installed("jsonlite")
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain
  
  api <- paste("https://nominatim.openstreetmap.org/search?format=json",
               "q={place}",
               ifelse(isTRUE(only_netherlands), "countrycodes=nl", ""),
               "limit=50",
               "namedetails=1",
               sep = "&")
  
  place_long <- place
  place <- unique(place)
  
  out <- data.frame(place = place,
                    name = character(length(place)),
                    latitude = numeric(length(place)),
                    longitude = numeric(length(place)),
                    stringsAsFactors = FALSE)
  
  # HQ of Certe
  van_swietenlaan_2 <- sf::st_sfc(sf::st_point(c(6.5504128, 53.1931877)),
                                  # take the CRS from included datasets
                                  crs = sf::st_crs(certegis::geo_postcodes4))
  
  for (i in seq_len(length(place))) {
    url <- gsub("{place}", place[i], api, fixed = TRUE)
    osm <- tryCatch({
      result <- jsonlite::fromJSON(url)
      # fair use is 1 per second
      Sys.sleep(0.25)
      if (NROW(result) > 1) {
        # keep track of how the OSM algorithm sorts:
        result$osm_sorting <- seq_len(nrow(result))
        # multiple results, determine distance from the HQ of Certe
        result$metres_from_certe <- round(
          vapply(FUN.VALUE = double(1),
                 X = get_bbox(result$boundingbox,
                              # take the CRS from included datasets
                              crs = sf::st_crs(certegis::geo_postcodes4)),
                 FUN = function(x)
                   as.double(sf::st_distance(sf::st_as_sfc(x),
                                             van_swietenlaan_2))),
          # round on 1000 metres:
          digits = -3)
        # return first hit based on distance from Certe HQ and OSM sorting
        result[order(result$metres_from_certe, result$osm_sorting)[1], , drop = FALSE]
      } else {
        result[1, , drop = FALSE]
      }
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(osm) || length(osm) == 0 || is.na(osm[[1]])) {
      message("No coordinates found for '", place[i], "'")
      out$latitude[i] <- 0
      out$longitude[i] <- 0
    } else {
      out$name[i] <- ifelse("short_name" %in% colnames(osm$namedetails),
                            osm$namedetails$short_name,
                            ifelse("name" %in% colnames(osm$namedetails),
                                   osm$namedetails$name,
                                   NA_character_))
      out$latitude[i] <- as.double(osm$lat)
      out$longitude[i] <- as.double(osm$lon)
    }
  }
  
  # 'de-uniquify' the data set
  out <- out[match(place_long, place), , drop = FALSE]
  rownames(out) <- NULL
  
  # as sf with geometry column and same CRS as included data sets
  out <- sf::st_as_sf(out,
                      coords = c("longitude", "latitude"),
                      # take the CRS from included datasets
                      crs = sf::st_crs(certegis::geo_postcodes4))
  # replace lat=0,lon=0 coordinates with valid empty ones
  out$geometry[which(vapply(FUN.VALUE = logical(1),
                            out$geometry,
                            function(sfc) identical(unclass(sfc), c(0, 0))))] <- sf::st_point()
  if (isTRUE(as_coordinates)) {
    out$geometry
  } else {
    out
  }
}

#' @rdname geocoding
#' @export
reverse_geocode <- function(sf_data) {
  check_is_installed("jsonlite")
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain
  
  api <- paste("https://nominatim.openstreetmap.org/reverse?format=json",
               "lat={latitude}",
               "lon={longitude}",
               "limit=50",
               "namedetails=1",
               sep = "&")
  
  if (NCOL(sf_data) == 1 && is.character(sf_data)) {
    # no coordinates but text instead, do regular geocoding first
    sf_data <- geocode(sf_data)
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
                    name = rep(NA_character_, length(coord)),
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
      result <- as.data.frame(jsonlite::fromJSON(url)$address, stringsAsFactors = FALSE)
      if (all(c("road", "house_number") %in% colnames(result))) {
        result$address <- trimws(paste(result$road, result$house_number))
      } else {
        result$address <- trimws(result$road)
      }
      if (any(c("amenity", "shop", "building") %in% colnames(result)) && !"place" %in% colnames(result)) {
        result$place <- c(result$amenity, result$shop, result$building)[1]
      }
      # fair use is 1 per second
      Sys.sleep(0.25)
      result$zipcode <- result$postcode
      result[, colnames(result)[which(colnames(result) %in% c("place", "address", "zipcode", "city", "town"))], drop = FALSE]
    }, error = function(e) {
      message("unable to retrieve address: ", e$message)
      return(NULL)
    })
    if (!is.null(osm)) {
      if ("place" %in% colnames(osm)) {
        out$name[i] <- osm$place
      }
      out$address[i] <- osm$address
      out$zipcode[i] <- osm$zipcode
      if ("city" %in% colnames(osm)) {
        out$city[i] <- osm$city
      } else if ("town" %in% colnames(osm)) {
        out$city[i] <- osm$town      
      }
    }
  }
  out <- out[match(coord_long, coord), c("name", "address", "zipcode", "city"), drop = FALSE]
  rownames(out) <- NULL
  out
}
