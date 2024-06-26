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

globalVariables(c("afstand_km",
                  "gemeente",
                  "ggdregio",
                  "nuts3",
                  "postcode",
                  "postcode.x",
                  "postcode.y",
                  "provincie"))

check_is_installed <- function(pkgs) {
  to_install <- pkgs[which(!pkgs %in% rownames(utils::installed.packages()))]
  if (length(to_install) > 0) {
    # ask to install
    choice <- utils::askYesNo(paste0("Package(s) required but not installed: ",
                                     paste0("'", to_install, "'", collapse = ", "), ". ",
                                     "Install now?"))
    if (isTRUE(choice)) {
      utils::install.packages(to_install)
      if ("sf" %in% pkgs) {
        try(loadNamespace("sf"), silent = TRUE)
      }
      # try again:
      check_is_installed(pkgs)
    } else {
      stop("Required package(s) ",
           paste0("'", to_install, "'", collapse = ", "), 
           " not installed", call. = FALSE)
    }
  } else {
    return(TRUE)
  }
}

get_geo_datasets <- function() {
  ds <- utils::data(package = "certegis")$results[, "Item"]
  ds[ds %like% "^geo_"]
}

get_bbox <- function(lst, crs) {
  lapply(lst, function(bb) {
    bb <- as.double(bb)
    sf::st_bbox(c(ymin = bb[1],
                  ymax = bb[2],
                  xmin = bb[3],
                  xmax = bb[4]),
                crs = crs)
  })
}

get_df_from_remote_json <- function(x) {
  check_is_installed(c("httr", "jsonlite"))
  result <- httr::GET(utils::URLencode(x))
  httr::stop_for_status(result)
  result |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)
}
