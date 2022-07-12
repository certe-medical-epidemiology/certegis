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

.onAttach <- function(...) {
  # edit in R/data.R
  packageStartupMessage(paste0("Linking to open data from Statistics Netherlands:\n",
                               paste0(" * ", names(CBS_VERSION), ": ", CBS_VERSION, collapse = "\n")))
}

.onLoad <- function(...) {
  if ("sf" %in% utils::installed.packages()) {
    # load sf namespace on load, so that:
    # - `certegis` geographic data sets will print correctly
    # - `certegis` GIS functions can be used correctly
    # - `certegis` GIS functions can be used in other packages (`vctrs` pkg will otherwise complain)
    loadNamespace("sf")
  }
}
