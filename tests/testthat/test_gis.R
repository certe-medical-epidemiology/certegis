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

  mp <- get_map()
  expect_true(is.sf(mp))
  expect_true(is.sf(as.sf(mp)))
  expect_true(is.sf(as.sf(as.data.frame(mp))))
  
  expect_error(latitude(as.data.frame(mp)))
  expect_error(longitude(as.data.frame(mp)))
  
  expect_error(get_map(maptype = "fake"))
  expect_error(add_map(data.frame(pc = c(9001:9010))))
  expect_s3_class(add_map(data.frame(postcode = c(9001:9010))), "sf") # PC4
  expect_s3_class(add_map(data.frame(postcode = c(900:910))), "sf")   # PC3
  expect_s3_class(add_map(data.frame(postcode = c(90:91))), "sf")     # PC2
  expect_s3_class(add_map(data.frame(gemeente = "Groningen")), "sf")
  expect_s3_class(add_map(data.frame(provincie = "Groningen")), "sf")
  expect_s3_class(add_map(data.frame(ggdregio = "Groningen")), "sf")
  expect_s3_class(add_map(data.frame(nuts3 = "Overig Groningen")), "sf")
  expect_warning(add_map(data.frame(postcode = c(9001, 9001), n = c(1, 2))))
  
  expect_lt(nrow(crop_certe(geo_provincies)), nrow(geo_provincies))
  expect_lt(nrow(crop_certe(geo_gemeenten)), nrow(geo_gemeenten))
  expect_lt(nrow(crop_certe(geo_ggdregios)), nrow(geo_ggdregios))
  expect_lt(nrow(crop_certe(geo_nuts3)), nrow(geo_nuts3))
  expect_lt(nrow(crop_certe(geo_postcodes2)), nrow(geo_postcodes2))
  expect_lt(nrow(crop_certe(geo_postcodes3)), nrow(geo_postcodes3))
  expect_lt(nrow(crop_certe(geo_postcodes4)), nrow(geo_postcodes4))
  
  expect_lt(nrow(filter_sf(geo_provincies, ymin = 52.5)), nrow(geo_provincies))
  expect_lt(nrow(filter_sf(geo_provincies, ymin = 52.5, ymax = 60, xmin = 4, xmax = 8)), nrow(geo_provincies))
  
  expect_true(is.numeric(latitude(geo_provincies)))
  expect_true(is.numeric(longitude(geo_provincies)))
  
  expect_lt(nrow(geo_postcodes4 %>% filter_geolocation(gemeente == "Tytsjerksteradiel")),
            nrow(geo_postcodes4))
  expect_warning(geo_postcodes3 %>% filter_geolocation(gemeente == "Tytsjerksteradiel"))
})

test_that("geocoding works", {
  expect_s3_class(get_coordinates("Van Swietenlaan 2, Groningen"), "sfc")
  expect_equal(nrow(get_addresses(c("Certe NL", "Certe NL", "IKEA Groningen"))), 3)
})
