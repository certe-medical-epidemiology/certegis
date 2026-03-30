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
#' Zipcodes not found in [postcodes4_afstanden] return `NA`.
#'
#' @return A character vector of hospital names, the same length as `zipcode`.
#' @export
#' @importFrom dplyr filter inner_join arrange distinct select tibble
#' @examples
#' adherence_area("9700")
#' adherence_area(c("9713", "7702", "8896"))
#'
#' # six-character zipcodes are automatically truncated
#' adherence_area("9251AB")
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
  zipcode <- substr(trimws(zipcode), 1, 4)
  
  dist <- certegis::postcodes4_afstanden |>
    filter(postcode.y %in% hosp_zipcodes) |>
    arrange(afstand_km)
  out <- character(length(zipcode))
  for (i in seq_along(out)) {
    mtch <- match(zipcode[i], dist$postcode.x)[1]
    if (is.na(mtch)) {
      out[i] <- NA_character_
    } else {
      hosp_zip <- dist$postcode.y[mtch]
      out[i] <- names(hosp_zipcodes[hosp_zipcodes == hosp_zip])
    }
  }
  
  out
}
