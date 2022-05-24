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
#' @param radius_km radius in kilometres from each zip code. The search *diameter* is twice this number (since zip codes e.g. to the west and to the east are searched).
#' @param minimum_cases minimum number of cases to search for
#' @param column_count column name in `data` with the number of case counts, defaults to the first column with numeric values
#' @param ... ignored, allows for future extensions
#' @return This function adds two columns (`"cases_within_radius"` `<dbl>` and `"minimum_met"` `<lgl>`) to the input data.
#' @importFrom dplyr filter select pull bind_rows group_rows
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' postcodes_friesland <- geo_postcodes4 |> 
#'   filter_geolocation(provincie == "Friesland") |> 
#'   pull(postcode)
#' 
#' # example with Norovirus cases:
#' noro <- data.frame(postcode = postcodes_friesland,
#'                    n = floor(runif(length(postcodes_friesland),
#'                                    min = 0, max = 3)))
#' head(noro)
#' 
#' radial_check <- cases_within_radius(noro, radius_km = 10, minimum_cases = 10)
#' head(radial_check)
#' 
#' 
#' # dplyr group support:
#' mdro <- data.frame(type = rep(c("ESBL", "MRSA", "VRE"), 20),
#'                    pc4 = postcodes_friesland[1:20],
#'                    n = floor(runif(60, min = 0, max = 3)))
#' mdro |> 
#'   group_by(type) |> 
#'   cases_within_radius()
#'   
#'   
#' # plotting support:
#' if (require("certeplot2")) {
#' 
#'   radial_check |>
#'     add_map() |>
#'     filter_geolocation(provincie == "Friesland") |>
#'     plot2(category = cases_within_radius,
#'           category.title = "Cases",
#'           datalabels = FALSE,
#'           colour_fill = "viridis")
#' 
#' }
cases_within_radius <- function(data, radius_km = 10, minimum_cases = 10, column_count = NULL, ...) {
  if (!any(colnames(data) %like% "^(pc|postcode)")) {
    stop("`data` must contain column that starts with 'pc' or 'postcode' for cases_within_radius()")
  }
  column_zip <- colnames(data)[colnames(data) %like% "^(pc|postcode)"][1]
  
  if (is.null(column_count)) {
    column_count <- colnames(data)[vapply(data, is.numeric, FUN.VALUE = logical(1))]
    column_count <- column_count[column_count %unlike% "^(pc|postcode)"][1]
    if (is.na(column_count)) {
      stop("No numeric column found in `data`")
    } else {
      message("Using column '", column_count, "' for cases_within_radius()")
    }
  }
  
  data$cases_within_radius <- 0
  data$minimum_met <- FALSE
  data_group_rows <- group_rows(data)
  current_group <- 0
  for (rows in data_group_rows) {
    
    current_group <- current_group + 1
    if (length(data_group_rows) > 1) {
      message("Analysing cases for group ", current_group, " out of ", length(data_group_rows),
              " (n = ", length(rows), ")... ",
              appendLF = FALSE)
    }
    
    data_in_group <- data[rows, , drop = FALSE]
    data_in_group$postcode <- data_in_group[, column_zip, drop = TRUE]
    
    unique_pc <- unique(data_in_group$postcode)
    warns <- data.frame(postcode = as.character(unique_pc),
                        cases_within_radius = NA_integer_,
                        minimum_met = FALSE)
    for (i in seq_len(length(unique_pc))) {
      pcs_within_radius <- certegis::postcodes4_afstanden |> 
        filter((postcode.x == as.character(unique_pc[i]) & postcode.y %in% as.character(data_in_group$postcode)) |
                 (postcode.y == as.character(unique_pc[i]) & postcode.x %in% as.character(data_in_group$postcode))) |> 
        filter(afstand_km <= radius_km) |> 
        select(postcode.x, postcode.y) |> 
        unlist() |> 
        unique()
      # add own zip code
      pcs_within_radius <- c(pcs_within_radius, unique_pc[i])
      
      n_sum <- data_in_group |> 
        filter(postcode %in% pcs_within_radius) |> 
        pull(column_count) |> 
        sum(na.rm = TRUE)
      if (n_sum >= minimum_cases) {
        warns$minimum_met[i] <- TRUE
      }
      warns$cases_within_radius[i] <- as.integer(n_sum)
    }
    
    data[rows, "cases_within_radius"] <- warns$cases_within_radius
    data[rows, "minimum_met"] <- warns$minimum_met
    if (length(data_group_rows) > 1) {
      message("OK.")
    }
  }
  data
}
