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

CBS_VERSION <- c("'Kerncijfers per postcode'" = "ZIP 2020 v1",
                 "'Gebiedsindelingen'" = "GPKG 2022 v1",
                 "'Bevolking per geslacht per postcode' (data set 83503NED)" = "1 januari 2021",
                 "'Bevolking en leeftijd per postcode' (data set 83502NED)" = "1 januari 2021")

included_datasets <- function() {
  DATASETS <- utils::data(package = "certegis")$results[, "Item", drop = TRUE]
  DATASETS[DATASETS %like% "^geo"]
}

#' Data Set with Dutch Zip Codes, Cities, Municipalities and Province
#' @format A [data.frame] with `r format(nrow(postcodes), big.mark = ",")` observations and `r ncol(postcodes)` variables:
#' - `postcode`\cr zip code, contains PC2, PC3 and PC4
#' - `inwoners`\cr total number of inhabitants
#' - `inwoners_man`\cr total number of male inhabitants
#' - `inwoners_vrouw`\cr total number of female inhabitants
#' - `plaats`\cr formal Dutch city name
#' - `gemeente`\cr formal Dutch municipality name
#' - `provincie`\cr formal Dutch province name
#' - `nuts3`\cr Nomenclature of Territorial Units for Statistics, level 3 (in Dutch: COROP region, *Coordinatie Commissie Regionaal OnderzoeksProgramma*)
#' - `ggdregio`\cr name of the regional GGD service (public healthcare service)
# - `huishoudens`\cr number of households in the zip code area
# - `huishouden_grootte`\cr mean size of the household sizes in the zip code area
#' @details See [the repository file](https://github.com/certe-medical-epidemiology/certegis/blob/main/data-raw/update_gis.R) to update this data set.
#' @source The data in this [data.frame] are retrieved from, and publicly available at, Statistics Netherlands: *StatLine*, Centraal Bureau voor de Statistiek (CBS), `r names(CBS_VERSION[names(CBS_VERSION) %like% "83503NED"])`, `r CBS_VERSION[names(CBS_VERSION) %like% "83503NED"]`, <https://opendata.cbs.nl>.
#' @examples 
#' head(postcodes)
#' str(postcodes)
"postcodes"

#' Number of Inhabitants per Zip Code and Age
#' @format A [data.frame] with `r format(nrow(inwoners_per_postcode_leeftijd), big.mark = ",")` observations and `r ncol(inwoners_per_postcode_leeftijd)` variables:
#' - `postcode`\cr zip code, contains PC2, PC3 and PC4
#' - `leeftijd`\cr age group per 5 years: 0-4, 5-9, ..., 90-94, 95+
#' - `inwoners`\cr total number of inhabitants
#' - `inwoners_man`\cr total number of male inhabitants
#' - `inwoners_vrouw`\cr total number of female inhabitants
#' @details See [the repository file](https://github.com/certe-medical-epidemiology/certegis/blob/main/data-raw/update_gis.R) to update this data set.
#' @source The data in this [data.frame] are retrieved from, and publicly available at, Statistics Netherlands: *StatLine*, Centraal Bureau voor de Statistiek (CBS), `r names(CBS_VERSION[names(CBS_VERSION) %like% "83502NED"])`, `r CBS_VERSION[names(CBS_VERSION) %like% "83502NED"]`, <https://opendata.cbs.nl>.
#' @examples 
#' head(inwoners_per_postcode_leeftijd)
#' str(inwoners_per_postcode_leeftijd)
"inwoners_per_postcode_leeftijd"

#' Data Sets with Geometries of Dutch Provinces, Municipalities and Zip Codes
#' @details These [data.frame]s are of additional class `sf` and contain 3 variables:
#' - `...`\cr name of the area, these are: `r paste0("\u0096", vapply(FUN.VALUE = character(1), get_geo_datasets(), function(d) paste0(d, "$", colnames(get(d, envir = asNamespace("certegis")))[1]), USE.NAMES = FALSE), "\u0096", collapse = ", ")`
#' - `inwoners`\cr number of inhabitants in the area
#' - `oppervlakte_km2`\cr area in square kilometres
#' - `geometry`\cr multipolygonal object of the area
#' 
#' All data sets have the coordinate reference system (CRS) set to [EPSG:28992](https://epsg.io/28992) ('RD New'), following the sphere of Earth. They can be flattened to e.g. [EPSG:4326](https://epsg.io/4326) ('WGS 84') using [`st_transform()`][sf::st_transform()].
#' 
#' See [the repository file](https://github.com/certe-medical-epidemiology/certegis/blob/main/data-raw/update_gis.R) to update these data sets.
#' 
#' **NOTE**: all data sets contains all areas of the whole country of the Netherlands, except for `geo_postcodes6` which was cropped to only cover the Certe region (using [crop_certe()]).
#' @source The data in these [data.frame]s are retrieved from, and publicly available at, Statistics Netherlands: 
#' 
#' * Centraal Bureau voor de Statistiek (CBS), `r names(CBS_VERSION[names(CBS_VERSION) %like% "gebiedsindeling"])`, `r CBS_VERSION[names(CBS_VERSION) %like% "gebiedsindeling"]`, <https://www.cbs.nl>
#' * Centraal Bureau voor de Statistiek (CBS), `r names(CBS_VERSION[names(CBS_VERSION) %like% "kerncijfers"])`, `r CBS_VERSION[names(CBS_VERSION) %like% "kerncijfers"]`, <https://www.cbs.nl>
#' @name cbs_geodata
#' @rdname cbs_geodata
#' @examples 
#' if (require("certeplot2")) {
#' 
#'   geo_postcodes6 |>
#'     filter_geolocation(plaats == "Groningen") |>
#'     plot2(category = inwoners / oppervlakte_km2,
#'           datalabels = FALSE,
#'           title = "City of Groningen")
#'   
#' }
#' 
#' if (require("certeplot2")) {
#' 
#'   geo_postcodes4 |>
#'     filter_geolocation(plaats == "Groningen") |>
#'     plot2(category = inwoners / oppervlakte_km2,
#'           datalabels = FALSE,
#'           title = "City of Groningen")
#'   
#' }
#' 
#' if (require("sf")) {
#' 
#'   head(geo_gemeenten)
#' 
#' }
"geo_gemeenten"

#' @rdname cbs_geodata
"geo_ggdregios"

#' @rdname cbs_geodata
"geo_nuts3"

#' @rdname cbs_geodata
"geo_postcodes2"

#' @rdname cbs_geodata
"geo_postcodes3"

#' @rdname cbs_geodata
"geo_postcodes4"

#' @rdname cbs_geodata
"geo_postcodes6"

#' @rdname cbs_geodata
"geo_provincies"

#' Distance from Zip Code to Zip Code
#' 
#' This data set was obtained by calculating the difference from the middle point of a zip code geometry to another zip code geometry (using the [geo_postcodes4] data set and the `sf` package).
#' @format A [data.frame] with `r format(nrow(postcodes4_afstanden), big.mark = ",")` observations and `r ncol(postcodes4_afstanden)` variables:
#' - `postcode.x`\cr zip code (PC4)
#' - `postcode.y`\cr zip code (PC4)
#' - `afstand_km`\cr distance in kilometres
#' @source The data in this [data.frame] are retrieved from, and publicly available at, Statistics Netherlands: 
#' 
#' * Centraal Bureau voor de Statistiek (CBS), `r names(CBS_VERSION[names(CBS_VERSION) %like% "gebiedsindeling"])`, `r CBS_VERSION[names(CBS_VERSION) %like% "gebiedsindeling"]`, <https://www.cbs.nl>
#' @examples 
#' head(postcodes4_afstanden)
"postcodes4_afstanden"
