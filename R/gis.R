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
#' These are function to work with geographical data.
#' @param data data to join left to the geodata
#' @param maptype type of geometric data, must be one of: `r paste0("``\"", gsub("geo_", "", included_datasets()), "\"``", collapse = ", ")`. For [add_map()], this is determined automatically if left blank.
#' @param crop_certe [logical] to keep only the Certe region
#' @rdname GIS
#' @return An `sf` model. The column with geodata is always called `"geometry"`.
#' @export
#' @examples 
#' library(sf)
#' 
#' get_map() # defaults to the geo_postcodes4 data set
get_map <- function(maptype = "postcodes4") {
  
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
#' data.frame(postcode = 7702, number_of_cases = 3) %>% 
#'   add_map()
add_map <- function(data, maptype = NULL, by = NULL, crop_certe = TRUE) {
  check_is_installed("sf")
  
  if (is.null(maptype)) {
    # determine automatically
    common_cols <- intersect(colnames(data), gsub("[ns]$", "", gsub("^geo_", "", included_datasets())))
    if (length(common_cols) == 0) {
      # try zip codes
      if ("postcode" %in% colnames(data)) {
        maptype <- paste0("postcodes", max(nchar(data$postcode)))
      } else {
        stop("No common column found to join.")
      }
    } else {
      maptype <- included_datasets()[included_datasets() %like% common_cols][1]
    }
  }
  
  geo_data <- get_map(maptype = maptype)
  if (isTRUE(crop_certe) && maptype %unlike% "postcodes") {
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
  is.data.frame(sf_data) &&
    (inherits(sf_data, "sf") |
       (any(unlist(lapply(sf_data, class)) == "sfc") & "geometry" %in% colnames(sf_data)))
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
  
  postcode_filter <- certegis::postcodes %>%
    filter(provincie %in% c("Friesland", "Fryslân", "Groningen", "Drenthe"))
  
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
    sf_data <- sf_data %>%
      filter(!as.integer(gsub("[^0-9]|", "", postcode)) %in% c(0:7000, 8255, 8263))
  }
  
  crs <- sf::st_crs(sf_data)
  crs_changed <- FALSE
  
  if (sf::st_crs(sf_data)$input %unlike% "4326") {
    sf_data <- sf::st_transform(sf_data, 4326)
    crs_changed <- TRUE
  }
  sf_data <- sf_data %>%
    filter_sf(xmin = 4.75,
              xmax = 7.25,
              ymin = ifelse("postcode" %in% colnames(sf_data), 52.58, 52.40),
              ymax = 53.60)
  
  if (isTRUE(crs_changed)) {
    sf_data <- sf::st_transform(sf_data, crs)
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
  if (is.null(col_zipcode)) {
    col_zipcode <- rev(sort(colnames(sf_data)[which(colnames(sf_data) %in% c("postcode", paste0("pc", 2:4)))]))[1]
    if (is.na(col_zipcode)) {
      stop("set column for zipcodes with 'col_zipcode'")
    }
  }
  if (!col_zipcode %in% colnames(sf_data)) {
    stop("'sf_data' must contain the column '", col_zipcode, "'")
  }
  min_char <- min(nchar(as.integer(gsub("[^0-9]|", "", sf_data[, col_zipcode, drop = TRUE]))))
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
#' @details [latitude()] and [longitude()] return these specific geographic properties of `sf_data`.
#' @export
#' @examples 
#' 
#' latitude(geo_provincies)
#' longitude(geo_provincies)
latitude <- function(sf_data) {
  check_is_installed("sf")
  
  if (!sf::st_is_longlat(sf_data)) {
    stop("sf_data does not contain geographic coordinates")
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
  
  if (!sf::st_is_longlat(sf_data)) {
    stop("sf_data does not contain geographic coordinates")
  }
  as.data.frame(
    sf::st_coordinates(
      suppressWarnings(sf::st_centroid(sf_data))
    )
  )$X
}
