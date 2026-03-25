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

#' Determine Hospital Adherence Area
#'
#' Looks up the nearest hospital for a given zipcode (4 digits) based on
#' the shortest distance in the [postcodes4_afstanden] dataset.
#'
#' @param zipcode A character or numeric vector of zipcodes. Six-character
#'   zipcodes are automatically truncated to the first 4 digits using
#'   `substr()`.
#'
#' @details
#' The function looks up the distance from each input zipcode to all
#' hospital zipcodes in [postcodes4_afstanden]. The hospital with the
#' shortest distance is returned. In case of equal distances, the first
#' hospital in definition order is chosen.
#'
#' The following hospitals and their zipcodes are hard-coded:
#'
#' | Hospital            | Zip Code |
#' |---------------------|----------|
#' | Antonius            | 8601     |
#' | Frisius Heerenveen  | 8441     |
#' | Frisius Leeuwarden  | 8934     |
#' | Martini             | 9728     |
#' | Nij Smellinghe      | 9202     |
#' | Ommelander          | 9679     |
#' | Treant              | 7824     |
#' | Wilhelmina          | 9401     |
#'
#' zipcodes not found in [postcodes4_afstanden] return `NA`.
#'
#' @return A character vector of hospital names, the same length as `zipcode`.
#' @export
#' @importFrom dplyr filter inner_join arrange distinct select tibble
#' @examples
#' adherence_area("9700")
#' adherence_area(c("9700", "7702", "8600"))
#'
#' # six-character zipcodes are automatically truncated
#' adherence_area("9700AB")
adherence_area <- function(zipcode) {
  
  hosp_zipcodes <- c(
    "Antonius"           = "8601",
    "Frisius Heerenveen" = "8441",
    "Frisius Leeuwarden" = "8934",
    "Martini"            = "9728",
    "Nij Smellinghe"     = "9202",
    "Ommelander"         = "9679",
    "Treant"             = "7824",
    "Wilhelmina"         = "9401"
  )
  
  zipcode <- as.character(zipcode)
  zipcode <- ifelse(nchar(zipcode) == 6, substr(zipcode, 1, 4), zipcode)
  
  # build full lookup: one row per postcode, nearest hospital
  lookup <- certegis::postcodes4_afstanden |>
    filter(postcode.y %in% hosp_zipcodes) |>
    inner_join(
      tibble(
        hosp_name = names(hosp_zipcodes),
        hosp_zipcode = unname(hosp_zipcodes)
      ),
      by = c("postcode.y" = "hosp_zipcode")
    ) |>
    arrange(postcode.x, afstand_km) |>
    distinct(postcode.x, .keep_all = TRUE) |>
    select(postcode.x, hosp_name)
  
  # named vector for O(1) lookup per postcode
  lookup_vec <- setNames(lookup$hosp_name, lookup$postcode.x)
  
  unname(lookup_vec[zipcode])
}
