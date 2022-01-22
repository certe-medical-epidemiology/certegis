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

#' Geodata Functions
#' 
#' These are functions to work with geographical data. To determine coordinates based on a location (or vice versa), use [geocode()] / [reverse_geocode()].
#' @param data [data.frame]
#' @param maptype type of geometric data, must be one of: `r paste0("``\"", gsub("geo_", "", included_datasets()), "\"``", collapse = ", ")`. For [add_map()], this is determined automatically if left blank.
#' @param crop_certe [logical] to keep only the Certe region
#' @details All of these functions will check if the `sf` package is installed, and will load its namespace (but not attach the package).
#' @rdname GIS
#' @name GIS
#' @return An `sf` model. The column with geodata is always called `"geometry"`.
#' @export
#' @examples 
#' get_map() # defaults to the geo_postcodes4 data set
get_map <- function(maptype = "postcodes4") {
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain
  
  maptype <- gsub("pc", "postcodes", tolower(maptype[1L]))
  if (maptype %unlike% "^geo_") {
    maptype <- paste0("geo_", maptype)
  }
  if (!maptype %in% included_datasets()) {
    stop("invalid 'maptype', invalid types are ", paste0("'", gsub("geo_", "", included_datasets()), "'", collapse = ", "))
  }
  getExportedValue(name = maptype, ns = asNamespace("certegis"))
}

#' @rdname GIS
#' @param data data set to join left to the geodata
#' @param by column to join by
#' @importFrom dplyr left_join group_by summarise across everything
#' @export
#' @examples 
#' 
#' data.frame(postcode = 7753, number_of_cases = 3) %>% 
#'   add_map()
add_map <- function(data, maptype = NULL, by = NULL, crop_certe = TRUE) {
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain
  
  if (is.null(maptype)) {
    # determine automatically
    common_cols <- intersect(colnames(data), gsub("[ns]$", "", gsub("^geo_", "", included_datasets())))
    if (length(common_cols) == 0) {
      # try zip codes
      if ("postcode" %in% colnames(data)) {
        maptype <- paste0("postcodes", max(nchar(as.character(data$postcode))))
      } else {
        stop("No common column found to join.")
      }
    } else {
      maptype <- included_datasets()[included_datasets() %like% common_cols][1]
    }
  }
  
  geo_data <- get_map(maptype = maptype)
  if (isTRUE(crop_certe)) {
    geo_data <- crop_certe(geo_data)
  }

  if (is.null(by)) {
    # search for the 'by'
    by <- intersect(colnames(data), colnames(geo_data))
    if (length(by) == 0) {
      stop("No common column found to join.")
    } else {
      message("Joining, by ", by)
    }
  }
  
  if (length(data[, by, drop = TRUE]) > length(unique(data[, by, drop = TRUE]))) {
    warning("Column '", by, "'in `data` should have unique values for `add_map()`, summarising all numeric columns of `data`", call. = FALSE)
    data <- data %>% 
      group_by(across(by)) %>% 
      summarise(across(everything(),
                       function(x) {
                         if (is.numeric(x)) {
                           sum(x, na.rm = TRUE)
                         } else {
                           x[1]
                         }
                       }))
  }
  
  geo_data[, by] <- as.character(geo_data[, by, drop = TRUE])
  data[, by] <- as.character(data[, by, drop = TRUE])
  sf::st_as_sf(as.data.frame(suppressMessages(left_join(geo_data, data, by = by)),
                             stringsAsFactors = FALSE))
}

#' @rdname GIS
#' @export
is.sf <- function(sf_data) {
  inherits(sf_data, "sf")
}

#' @rdname GIS
#' @export
as.sf <- function(data) {
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain
  
  if (is.sf(data)) {
    data
  } else {
    sf::st_as_sf(data)
  }
}

#' @rdname GIS
#' @importFrom dplyr `%>%` mutate filter 
#' @details [crop_certe()] cuts any geometry on the Certe region (more or less the Northern three provinces of the Netherlands).
#' @export
#' @examples 
#' 
#' geo_provincies %>% crop_certe()
crop_certe <- function(sf_data) {
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain

  postcode_filter <- certegis::postcodes %>%
    filter(provincie %in% c("Friesland", "Groningen", "Drenthe"))
  
  if ("provincie" %in% colnames(sf_data)) {
    sf_data <- sf_data %>%
      filter(provincie %in% postcode_filter$provincie)
    return(sf_data)
  } else if ("gemeente" %in% colnames(sf_data)) {
    sf_data <- sf_data %>%
      filter(gemeente %in% postcode_filter$gemeente)
  } else if ("nuts3" %in% colnames(sf_data)) {
    sf_data <- sf_data %>%
      filter(nuts3 %in% postcode_filter$nuts3)
  } else if ("ggdregio" %in% colnames(sf_data)) {
    sf_data <- sf_data %>%
      filter(ggdregio %in% postcode_filter$ggdregio)
  } else if ("postcode" %in% colnames(sf_data)) {
    max_nchar <- max(nchar(as.character(sf_data$postcode)), na.rm = TRUE)
    # PC4
    if (max_nchar == 4) {
      sf_data <- sf_data %>%
        filter(!as.integer(gsub("[^0-9]|", "", as.character(postcode))) %in% c(0:7749, 7770:7799, 8000:8299))
    } else if (max_nchar == 3) { 
      sf_data <- sf_data %>%
        filter(!as.integer(gsub("[^0-9]|", "", as.character(postcode))) %in% c(0:774, 777:779, 800:829))
    } else {
      sf_data <- sf_data %>%
        filter(!as.integer(gsub("[^0-9]|", "", as.character(postcode))) %in% c(0:77, 80:82))
    }
  } else {
    # try a bounding box based on PC4 level
    bbox <- sf::st_bbox(crop_certe(certegis::geo_postcodes4))
    # special case for SFC points
    if (inherits(sf_data, c("sfg", "XY", "sfc", "sfc_POINT"))) {
      if (inherits(sf_data, c("sfg", "XY"))) {
        sf_data <- sf::st_sfc(sf_data)
      }
      sf_data <- sf::st_as_sf(sf_data, crs = "EPSG:4326")
      sf_data <- suppressMessages(sf::st_filter(sf_data, sf::st_as_sfc(bbox), .predicate = sf::st_within))
      return(sf_data[, 1, drop = TRUE])
    }
    sf_data <- suppressMessages(sf::st_filter(sf_data, sf::st_as_sfc(bbox), .predicate = sf::st_within))
  }
  sf_data
}

#' @rdname GIS
#' @param sf_data a data set of class 'sf'
#' @param xmin,xmax,ymin,ymax coordination filters for `sf_data`
#' @details [filter_sf()] filters an sf object on coordinates, and is internally used by [crop_certe()].
#' @export
#' @examples 
#' 
#' # filter on a latitude of 52.5 degrees and higher
#' geo_provincies %>% filter_sf(ymin = 52.5)
filter_sf <- function(sf_data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain
  
  if(!is.sf(sf_data)) {
    sf_data <- sf::st_as_sf(sf_data)
  }
  bb <- sf::st_bbox(sf_data)
  if (!is.null(xmin)) bb["xmin"] <- xmin
  if (!is.null(xmax)) bb["xmax"] <- xmax
  if (!is.null(ymin)) bb["ymin"] <- ymin
  if (!is.null(ymax)) bb["ymax"] <- ymax
  suppressMessages(sf::st_filter(sf_data, sf::st_as_sfc(bb), .predicate = sf::st_within))
}

#' @rdname GIS
#' @param ... filters to set
#' @param col_zipcode column with zip codes
#' @importFrom dplyr `%>%` mutate filter 
#' @export
#' @examples 
#' 
#' if (require("certeplot2")) {
#' 
#'   geo_postcodes4 %>% 
#'     filter_geolocation(gemeente == "Tytsjerksteradiel") %>% 
#'     plot2(category = inwoners,
#'           datalabels = postcode)
#' 
#' }
filter_geolocation <- function(sf_data, ..., col_zipcode = NULL) {
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain
  
  if (is.null(col_zipcode)) {
    col_zipcode <- rev(sort(colnames(sf_data)[which(colnames(sf_data) %in% c("postcode", paste0("pc", 2:4)))]))[1]
    if (is.na(col_zipcode)) {
      stop("set column for zipcodes with 'col_zipcode'")
    }
  }
  if (!col_zipcode %in% colnames(sf_data)) {
    stop("'sf_data' must contain the column '", col_zipcode, "'")
  }
  min_char <- min(nchar(as.character(gsub("[^0-9]|", "", sf_data[, col_zipcode, drop = TRUE]))))
  if (min_char < 4) {
    warning("filter may not be accurate since (some) zip codes only contain ", min_char, " numbers", call. = FALSE)
  }
  filtered <- certegis::postcodes %>% filter(...)
  filtered_sf <- sf_data %>%
    mutate(postcode = as.double(gsub("[^0-9]", "", sf_data[, col_zipcode, drop = TRUE]))) %>%
    filter(postcode %in% filtered$postcode)
  filtered_sf[, colnames(sf_data), drop = FALSE]
}

#' @rdname GIS
#' @param sf_data a data set of class 'sf'
#' @details [latitude()] specifies the north-south position ('y axis') and [longitude()] specifies the east-west position ('x axis'). They return the numeric coordinate of the centre of a simple feature.
#' @export
#' @examples 
#' 
#' latitude(geo_provincies)
#' longitude(geo_provincies)
latitude <- function(sf_data) {
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain
  
  if (!isTRUE(sf::st_is_longlat(sf_data))) {
    stop("`sf_data` does not contain geographic coordinates", call. = FALSE)
  }
  as.data.frame(
    sf::st_coordinates(
      suppressWarnings(sf::st_centroid(sf_data))
    )
  )$Y
}

#' @rdname GIS
#' @param sf_data a data set of class 'sf'
#' @export
longitude <- function(sf_data) {
  check_is_installed("sf")
  loadNamespace("sf") # for use in other packages, otherwise the `vctrs` pkg will complain
  
  if (!isTRUE(sf::st_is_longlat(sf_data))) {
    stop("`sf_data` does not contain geographic coordinates", call. = FALSE)
  }
  as.data.frame(
    sf::st_coordinates(
      suppressWarnings(sf::st_centroid(sf_data))
    )
  )$X
}
