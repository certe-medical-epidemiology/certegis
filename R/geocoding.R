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

#' Geocoding to Translate Address to Coordinates
#'
#' @param search a search string, such as an address. Preference is given to places in the Northern Netherlands using [[crop_certe()].
#' @param api URL of the application programming interface, defaults to OpenStreetMap
#' @name geocoding
#' @rdname geocoding
#' @export
get_coordinates <- function(search, api = "") {
  
}

#' @rdname geocoding
#' @param st_point a valid geometric `st_point` 
#' @param latitude latitude of a GPS location
#' @param longitude longitude of a GPS location
#' @export
get_address <- function(st_point, latitude, longitud, api = "https://nominatim.openstreetmap.org/reverse?lat={latitude}&lon={longitude}") {
  
}
