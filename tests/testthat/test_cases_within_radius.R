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

test_that("cases_within_radius works", {
  df <- data.frame(postcode = c(9001:9010),
                   n = floor(runif(10, min = 0, max = 30)))
  expect_s3_class(cases_within_radius(df), "data.frame")
  expect_s3_class(df |> 
                    dplyr::mutate(group = rep(c("A", "B"), 5)) |> 
                    dplyr::group_by(group) |> 
                    cases_within_radius(), "data.frame")
})
