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
#' @param data data to join left to the geodata
#' @param maptype type of geometric data, must be one of: `r paste0("``\"", data(package = "certegis")$results[, "Item"], "\"``", collapse = ", ")`
#' @rdname GIS
#' @importFrom dplyr left_join
#' @return An `sf` model. The column with geodata is always called `"geometry"`.
#' @export
add_map <- function(data, maptype = "postcodes4", by = NULL, cut_northern_nl = TRUE, ...) {
  check_is_installed("sf")
  data_ <- data
  map_ <- getExportedValue(name = maptype, ns = asNamespace("certegis"))
  if (isTRUE(cut_northern_nl)) {
    map_ <- cut_northern_nl(map_)
  }
  if (is.null(by)) {
    # search for the 'by'
    by <- colnames(data_)[colnames(data_) %in% colnames(map_)][1]
    if (is.na(by)) {
      stop("No common column found to join.")
    } else {
      message("Joining, by ", by)
    }
  }
  map_[, by] <- as.character(map_[, by, drop = TRUE])
  data_[, by] <- as.character(data_[, by, drop = TRUE])
  joined <- suppressMessages(left_join(map_, data_, by = by))
  sf::st_as_sf(joined)
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
#' @details [cut_northern_nl()] cuts any geometry on the northern three provinces of the Netherlands.
#' @export
cut_northern_nl <- function(sf_data) {
  check_is_installed("sf")
  
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
  } else if ("jeugdregio" %in% colnames(sf_data)) {
    sf_data <- sf_data %>%
      filter(jeugdregio %in% postcode_filter$jeugdregio)
  } else if ("veiligheidsregio" %in% colnames(sf_data)) {
    sf_data <- sf_data %>%
      filter(veiligheidsregio %in% postcode_filter$veiligheidsregio)
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
#' @details [filter_sf()] filters an sf object on coordinates, and is internally used by [cut_northern_nl()].
#' @export
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
#' @importFrom dplyr `%>%` mutate filter 
#' @export
#' @examples 
#' if (require("certeplot2")) {
#' 
#'   postcodes4 %>% 
#'     filter_geolocation(gemeente == "Tytsjerksteradiel") %>% 
#'     plot2()
#' 
#' }
filter_geolocation <- function(sf_data, ..., col_zipcode = NULL) {
  if (is.null(col_zipcode)) {
    col_zipcode <- rev(sort(colnames(sf_data)[which(colnames(sf_data) %in% c("postcode", paste0("pc", 2:6)))]))[1]
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
