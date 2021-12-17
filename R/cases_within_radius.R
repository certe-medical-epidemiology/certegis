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

#' Check Cases Within Radius
#'
#' Based on the [postcodes4_afstanden] data set, this function determines the specified minimum number of cases within a certain radius.
#' @param data data set containing a column 'postcode'
#' @param radius_km radius in kilometres from each zip code. The search `*diameter*` is twice this number (since zip codes e.g. to the west and to the east are searched).
#' @param minimal_cases minimum number of cases to search for
#' @param column_count column name in `data` with the number of case counts
#' @param ... ignored, allows for future extensions
#' @importFrom dplyr `%>%` filter select pull bind_rows
#' @export
#' @examples
#' noro <- data.frame(postcode = c(9000:9100),
#'                    n = floor(runif(101, 0, 3)))
#'                    
#' radial_check <- cases_within_radius(noro, radius_km = 10, minimal_cases = 10)
#' radial_check
#' 
#' if (require("certeplot2")) {
#' 
#'   radial_check %>%
#'     add_map() %>%
#'     filter_geolocation(provincie == "Friesland") %>%
#'     plot2(category = cases_within_range,
#'           datalabels = FALSE,
#'           colour_fill = "viridis")
#' 
#' }
cases_within_radius <- function(data, radius_km = 10, minimal_cases = 10, column_count = NULL, ...) {
  if (!"postcode" %in% colnames(data)) {
    stop("`data` must contain a column 'postcode'")
  }
  if (is.null(column_count)) {
    column_count <- colnames(data)[vapply(data, is.numeric, FUN.VALUE = logical(1))]
    column_count <- column_count[column_count != "postcode"][1]
    if (is.na(column_count)) {
      stop("No numeric column found in `data`")
    } else {
      message("Using column '", column_count, "' for cases_within_radius()")
    }
  }
  unique_pc <- unique(data$postcode)
  warns <- data.frame(postcode = character(0),
                      cases_within_range = integer(0))
  for (i in seq_len(length(unique_pc))) {
    pcs_within_radius <- certegis::postcodes4_afstanden %>% 
      filter((postcode.x == as.character(unique_pc[i]) & postcode.y %in% as.character(data$postcode)) |
               (postcode.y == as.character(unique_pc[i]) & postcode.x %in% as.character(data$postcode))) %>% 
      filter(afstand_km <= radius_km) %>% 
      select(postcode.x, postcode.y) %>% 
      unlist() %>% 
      unique()
    n_sum <- data %>% 
      filter(postcode %in% pcs_within_radius) %>% 
      pull(column_count) %>% 
      sum()
    if (n_sum >= minimal_cases) {
      message("Found ", n_sum, " cases within ", radius_km, " km of ", unique_pc[i])
      warns <- warns %>% 
        bind_rows(data.frame(postcode = as.character(unique_pc[i]),
                             cases_within_range = as.integer(n_sum)))
    }
  }
  warns
}
