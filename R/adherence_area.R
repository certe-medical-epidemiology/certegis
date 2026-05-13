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
#' @importFrom dplyr filter arrange
#' @examples
#' adherence_area("9700")
#' adherence_area(c("9713", "7702", "8896"))
#'
#' # six-character zipcodes are automatically truncated
#' adherence_area("9251AB")
#' 
#' if (requireNamespace("plot2", quietly = TRUE)) {
#'   library(certeplot2)
#'   geo_postcodes4 |>
#'      crop_certe() |>
#'      plot2(category = suppressMessages(adherence_area(postcode)),
#'            datalabels = FALSE) |>
#'      add_sf(geo_provincies |> crop_certe(),
#'             colour_fill = NA, colour = "black", linewidth = 0.5)
#' }
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
  
  zipcode <- substr(trimws(as.character(zipcode)), 1, 4)
  
  # Work on unique codes only
  uzip <- unique(zipcode)
  known <- certegis::postcodes4_afstanden$postcode.x
  
  # Substitute missing codes with nearest higher, per unique missing code
  missing <- uzip[!uzip %in% known]
  substitutes <- setNames(uzip, uzip)  # identity map for all, overwrite missing below
  
  for (z in missing) {
    zip2 <- substr(z, 1, 2)
    candidates <- known[substr(known, 1, 2) == zip2 & as.integer(known) > as.integer(z)]
    if (length(candidates) == 0L) {
      substitutes[z] <- NA_character_
    } else {
      substitutes[z] <- candidates[which.min(as.integer(candidates) - as.integer(z))]
    }
    message("Interpreting missing ", z, " as closest higher ", substitutes[z])
  }
  
  # Map full input to substituted unique codes
  zip_sub <- substitutes[zipcode]
  
  # Pre-filter distance table once, then find nearest hospital per unique substituted code
  dist <- certegis::postcodes4_afstanden |>
    filter(postcode.y %in% hosp_zipcodes) |>
    arrange(afstand_km)
  
  u_sub <- unique(zip_sub[!is.na(zip_sub)])
  hosp_lookup <- setNames(
    sapply(u_sub, function(z) {
      mtch <- match(z, dist$postcode.x)
      if (is.na(mtch)) NA_character_
      else names(hosp_zipcodes[hosp_zipcodes == dist$postcode.y[mtch]])
    }),
    u_sub
  )
  
  out <- hosp_lookup[zip_sub]
  out[is.na(zip_sub)] <- NA_character_
  
  # Overwrite Noordoostpolder / Urk as Antonius
  out[zipcode %in% postcodes$postcode[postcodes$gemeente %in% c("Noordoostpolder", "Urk")]] <- "Antonius"
  
  unname(out)
}
