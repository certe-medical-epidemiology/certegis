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

CBS_VERSION <- c(postcodes = "2021",
                 gebiedsindelingen = "2022 v1")

included_datasets <- function() {
  DATASETS <- utils::data(package = "certegis")$results[, "Item", drop = TRUE]
  DATASETS[DATASETS %like% "^geo"]
}

#' Data Set with Dutch Zipcodes, Cities, Municipalities and Province
#' @format A [data.frame] with `r format(nrow(postcodes), big.mark = ",")` observations and `r ncol(postcodes)` variables:
#' - `postcode`\cr zipcode, contains PC2, PC3 and PC4
#' - `inwoners`\cr total number of inhabitants
#' - `inwoners_man`\cr total number of male inhabitants
#' - `inwoners_vrouw`\cr total number of female inhabitants
#' - `plaats`\cr formal Dutch city name
#' - `gemeente`\cr formal Dutch municipality name
#' - `provincie`\cr formal Dutch province name
#' - `nuts3`\cr Nomenclature of Territorial Units for Statistics, level 3 (in Dutch: COROP region, *Coordinatie Commissie Regionaal OnderzoeksProgramma*)
#' - `ggdregio`\cr name of the regional GGD service (public healthcare service)
#' - `jeugdregio`\cr name of the regional youth service
#' - `veiligheidsregio`\cr name of the regional safety service (public healthcare service)
# - `huishoudens`\cr number of households in the zipcode area
# - `huishouden_grootte`\cr mean size of the household sizes in the zipcode area
#' @details See [the repository file](https://github.com/certe-medical-epidemiology/certedata/blob/main/data-raw/update_gis.R) to update this data set.
"postcodes"

#' Number of Inhabitants per Zipcode and Age
#' @format A [data.frame] with `r format(nrow(inwoners_per_postcode_leeftijd), big.mark = ",")` observations and `r ncol(inwoners_per_postcode_leeftijd)` variables:
#' - `postcode`\cr zipcode, contains PC2, PC3 and PC4
#' - `leeftijd`\cr age group per 5 years: 0-4, 5-9, ..., 90-94, 95+
#' - `inwoners`\cr total number of inhabitants
#' - `inwoners_man`\cr total number of male inhabitants
#' - `inwoners_vrouw`\cr total number of female inhabitants
#' @details See [the repository file](https://github.com/certe-medical-epidemiology/certedata/blob/main/data-raw/update_gis.R) to update this data set.
"inwoners_per_postcode_leeftijd"

#' Data Sets with Geometries of Dutch Provinces, Municipalities and Zipcodes
#' @format A `sf`/[data.frame] with 3 variables:
#' - `...`\cr column name of the identifier, which is the singular form of the name of the data set (i.e., `geo_gemeenten$gemeente`)
#' - `inwoners`\cr number of inhabitants
#' - `oppervlakte_km2`\cr area in square kilometres
#' - `geometry`\cr the polygonal form of the area
#' @details All data sets (maps) contain all of the Netherlands, except for the `geo_postcodes*` maps which only contains the Northern Netherlands.
#' @name cbs_data
#' @rdname cbs_data
#' @details See [the repository file](https://github.com/certe-medical-epidemiology/certegis/blob/main/data-raw/update_gis.R) to update these data sets.
"geo_postcodes4"

#' @rdname cbs_data
"geo_postcodes3"

#' @rdname cbs_data
"geo_postcodes2"

#' @rdname cbs_data
"geo_ggdregios"

#' @rdname cbs_data
"geo_nuts3"

#' @rdname cbs_data
"geo_gemeenten"

#' @rdname cbs_data
"geo_provincies"


#' Distance from Zipcode to Zipcode
#' 
#' This data set was obtained by calculating the difference from the middle point of a zipcode geometry to another zipcode geometry (using the [postcodes] data set and the `sf` package).
#' @format A [data.frame] with `r format(nrow(postcodes4_afstanden), big.mark = ",")` observations and `r ncol(postcodes4_afstanden)` variables:
#' - `postcode.x`\cr zipcode (PC4)
#' - `postcode.y`\cr zipcode (PC4)
#' - `afstand_km`\cr distance in kilometres
"postcodes4_afstanden"
