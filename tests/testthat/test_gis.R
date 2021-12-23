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

test_that("gis works", {
  mp <- get_map()
  expect_true(is.sf(mp))
  
  expect_error(get_map(maptype = "fake"))
  expect_error(add_map(data.frame(pc = c(9001:9010))))
  expect_s3_class(add_map(data.frame(postcode = c(9001:9010))), "data.frame")
  expect_s3_class(add_map(data.frame(gemeente = "Groningen")), "data.frame")
  expect_s3_class(add_map(data.frame(provincie = "Groningen")), "data.frame")
  expect_s3_class(add_map(data.frame(ggdregio = "Groningen")), "data.frame")
  expect_s3_class(add_map(data.frame(nuts3 = "Overig Groningen")), "data.frame")
  expect_warning(add_map(data.frame(postcode = c(9001, 9001), n = c(1, 2))))
  
  expect_lt(nrow(crop_certe(geo_provincies)), nrow(geo_provincies))
  expect_lt(nrow(crop_certe(geo_gemeenten)), nrow(geo_gemeenten))
  expect_lt(nrow(crop_certe(geo_ggdregios)), nrow(geo_ggdregios))
  expect_lt(nrow(crop_certe(geo_nuts3)), nrow(geo_nuts3))
  expect_lt(nrow(crop_certe(geo_postcodes2)), nrow(geo_postcodes2))
  expect_lt(nrow(crop_certe(geo_postcodes3)), nrow(geo_postcodes3))
  expect_lt(nrow(crop_certe(geo_postcodes4)), nrow(geo_postcodes4))
  
  expect_true(is.numeric(latitude(geo_provincies)))
  expect_true(is.numeric(longitude(geo_provincies)))
})
