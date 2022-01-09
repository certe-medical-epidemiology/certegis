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

library(sf)

test_that("geocoding works", {
  expect_s3_class(geocode("Van Swietenlaan 2, Groningen"), "sf")
  expect_s3_class(geocode("Van Swietenlaan 2, Groningen", as_coordinates = TRUE), "sfc_POINT")
  expect_equal(nrow(reverse_geocode(c("Certe", "Certe", "IKEA Groningen"))), 3)
  # places should be found nearest to the Certe main building
  expect_equal(reverse_geocode("IKEA")$city, "Groningen")
})
