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
#' @param maptype type of geometric data, must be one of: `r paste0("\u0060\"", gsub("geo_", "", included_datasets()), "\"\u0060", collapse = ", ")`. For [add_map()], this is determined automatically if left blank.
#' @param crop_certe [logical] to keep only the Certe region
#' @details All of these functions will check if the `sf` package is installed, and will load its namespace (but not attach the package).
#' @rdname GIS
#' @name GIS
#' @return An `sf` model. The column with geodata is always called `"geometry"`.
#' @export
#' @examples 
#' # Retrieving and joining maps ------------------------------------------
#' 
#' get_map() # defaults to the geo_postcodes4 data set
get_map <- function(maptype = "postcodes4") {
  check_is_installed("sf")
  loadNamespace("sf")
  
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
#' # adding a map applies a RIGHT JOIN to get all relevant geometric data
#' data.frame(postcode = 7753, number_of_cases = 3) |> 
#'   add_map()
add_map <- function(data, maptype = NULL, by = NULL, crop_certe = TRUE) {
  check_is_installed("sf")
  loadNamespace("sf")
  
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
    data <- data |> 
      group_by(across(by)) |> 
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
  check_is_installed("sf")
  loadNamespace("sf")
  inherits(sf_data, "sf")
}

#' @rdname GIS
#' @export
as.sf <- function(data) {
  check_is_installed("sf")
  loadNamespace("sf")
  if (is.sf(data)) {
    data
  } else {
    sf::st_as_sf(data)
  }
}


#' @rdname GIS
#' @importFrom dplyr mutate filter 
#' @details [crop_certe()] cuts any geometry to the Certe region (more of less): the Northern three provinces of the Netherlands and municipalities of Noordoostpolder, Urk, and Steenwijkerland. This will be based on [certegis::postcodes].
#' @export
#' @examples
#' 
#' 
#' # Cropping to Certe region ---------------------------------------------
#' 
#' # Note: provinces do not include Flevoland
#' geo_provincies |> crop_certe()
#' 
#' # but other geometries do, such as geo_gemeenten
#' if (require("certeplot2")) {
#'   geo_gemeenten |> crop_certe() |>    # cropped municipalities
#'     plot2(title = "Certe Region") |>
#'     add_sf(
#'       geo_provincies |> crop_certe(), # cropped provinces
#'       colour_fill = NA,
#'       colour = "black",
#'       linewidth = 0.5)
#' }
crop_certe <- function(sf_data) {
  check_is_installed("sf")
  loadNamespace("sf")
  
  postcode_filter <- certegis::postcodes |>
    filter(provincie %in% c("Friesland", "Groningen", "Drenthe") | gemeente %in% c("Noordoostpolder", "Urk", "Steenwijkerland"))
  
  if ("provincie" %in% colnames(sf_data)) {
    sf_data <- sf_data |>
      filter(provincie %in% c("Friesland", "Groningen", "Drenthe"))
    return(sf_data)
  } else if ("gemeente" %in% colnames(sf_data)) {
    sf_data <- sf_data |>
      filter(gemeente %in% postcode_filter$gemeente)
  } else if ("nuts3" %in% colnames(sf_data)) {
    sf_data <- sf_data |>
      filter(nuts3 %in% postcode_filter$nuts3)
  } else if ("ggdregio" %in% colnames(sf_data)) {
    sf_data <- sf_data |>
      filter(ggdregio %in% postcode_filter$ggdregio)
  } else if ("postcode" %in% colnames(sf_data)) {
    sf_data <- sf_data |>
      filter(as.numeric(substr(as.character(postcode), 1, 4)) %in% postcode_filter$postcode)
  } else {
    # try a bounding box based on PC4 level
    bbox <- sf::st_bbox(crop_certe(certegis::geo_postcodes4))
    # special case for SFC points
    if (inherits(sf_data, c("sfg", "XY", "sfc", "sfc_POINT"))) {
      if (inherits(sf_data, c("sfg", "XY"))) {
        sf_data <- sf::st_sfc(sf_data)
      }
      sf_data <- sf::st_as_sf(sf_data, crs = "EPSG:28992")
      sf_data <- suppressMessages(sf::st_filter(sf_data, sf::st_as_sfc(bbox), .predicate = sf::st_within))
      return(sf_data[, 1, drop = TRUE])
    }
    sf_data <- suppressMessages(sf::st_filter(sf_data, sf::st_as_sfc(bbox), .predicate = sf::st_within))
  }
  sf_data
}

#' @rdname GIS
#' @param ... filters to set
#' @details [filter_geolocation()] filters an sf object on qualitative values such as 'gemeente' and 'provincie'. The input data `sf_data` will be joined with [certegis::postcodes] and filtering can thus be done on any of these columns: `r toString(colnames(certegis::postcodes))`.
#' @importFrom dplyr filter group_by across summarise everything select all_of mutate left_join
#' @export
#' @examples 
#' 
#' 
#' # Filtering geometries -------------------------------------------------
#' 
#' geo_gemeenten |>
#'   crop_certe() |>
#'   # notice that the `provincie` column is not even in `geo_gemeenten`
#'   filter_geolocation(provincie == "Flevoland")
#'   
#' geo_gemeenten |>
#'   crop_certe() |>
#'   filter_geolocation(inwoners_vrouw >= 50000)
#' 
#' if (require("certeplot2")) {
#'   geo_postcodes4 |> 
#'     filter_geolocation(gemeente == "Tytsjerksteradiel") |> 
#'     plot2(category = inwoners,
#'           datalabels = postcode)
#' 
#' }
filter_geolocation <- function(sf_data, ...) {
  check_is_installed("sf")
  loadNamespace("sf")
  
  current_cols <- colnames(sf_data)
  if (!any(current_cols %in% colnames(certegis::postcodes))) {
    stop("The input data set should contain one of these columns: ", toString(colnames(certegis::postcodes)))
  }
  
  # only keep unique numbers in the postcodes data set
  # this allows to use a filter such as `inwoners > 150000` for `gemeente`
  join_col <- current_cols[which(current_cols %in% colnames(certegis::postcodes))][1]
  if (join_col != "postcode") {
    pcdata_to_join <- certegis::postcodes |> filter(postcode >= 1000)
  } else {
    # the join col is postcode, determine which group we need: pc2, pc3 or pc4
    max_pc_length <- max(nchar(sf_data$postcode))
    if (max_pc_length == 2) {
      pcdata_to_join <- certegis::postcodes |> filter(postcode < 100)
    } else if (max_pc_length == 3) {
      pcdata_to_join <- certegis::postcodes |> filter(postcode >= 100 & postcode < 1000)
    } else {
      pcdata_to_join <- certegis::postcodes |> filter(postcode >= 1000)
    }
  }
  pcdata_to_join <- pcdata_to_join |>
    mutate(postcode = as.character(postcode)) |> 
    group_by(across(all_of(join_col))) |>
    summarise(across(everything(), function(x) if (is.numeric(x)) sum(x, na.rm = TRUE) else x[1]))
  if ("postcode" %in% colnames(sf_data) && is.numeric(sf_data$postcode)) {
    pcdata_to_join <- pcdata_to_join |>
      mutate(postcode = as.numeric(postcode))
  }
  
  sf_data |>
    left_join(pcdata_to_join, by = join_col, suffix = c("", "_pcdata")) |>
    filter(...) |> 
    select(all_of(current_cols))
}

#' @rdname GIS
#' @param sf_data a data set of class 'sf'
#' @param xmin,xmax,ymin,ymax coordination filters for `sf_data`, given in degrees following [EPSG:4326](https://epsg.io/4326) ('WGS 84')
#' @details [filter_sf()] filters an sf object on coordinates, and is internally used by [crop_certe()].
#' @export
#' @examples 
#' 
#' # filter on a latitude of 52.5 degrees and higher
#' geo_provincies |> filter_sf(ymin = 52.5)
filter_sf <- function(sf_data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
  check_is_installed("sf")
  loadNamespace("sf")
  
  if(!is.sf(sf_data)) {
    sf_data <- sf::st_as_sf(sf_data)
  }
  
  # set CRS to degrees
  old_crs <- sf::st_crs(sf_data)
  sf_data <- sf::st_transform(sf_data, crs = 4326)
  
  bb <- sf::st_bbox(sf_data)
  if (!is.null(xmin)) bb["xmin"] <- xmin
  if (!is.null(xmax)) bb["xmax"] <- xmax
  if (!is.null(ymin)) bb["ymin"] <- ymin
  if (!is.null(ymax)) bb["ymax"] <- ymax
  out <- suppressMessages(sf::st_filter(sf_data, sf::st_as_sfc(bb), .predicate = sf::st_within))
  
  # set CRS back to input data
  sf::st_transform(out, crs = old_crs)
}

#' @rdname GIS
#' @export
#' @details [convert_to_degrees_CRS4326()] will transform SF data to [WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS](https://epsg.io/4326), CRS 4326.
#' @examples
#' 
#' 
#' # Transforming Coordinate Reference System (CRS) -----------------------
#' 
#' geo_provincies |> convert_to_degrees_CRS4326()
#' 
#' geo_provincies |> convert_to_metre_CRS28992()
convert_to_degrees_CRS4326 <- function(sf_data) {
  check_is_installed("sf")
  loadNamespace("sf")
  sf::st_transform(sf_data, crs = 4326)
}

#' @rdname GIS
#' @export
#' @details [convert_to_metre_CRS28992()] will transform SF data to [Amersfoort / RD New -- Netherlands - Holland - Dutch](https://epsg.io/28992), CRS 28992.
convert_to_metre_CRS28992 <- function(sf_data) {
  check_is_installed("sf")
  loadNamespace("sf")
  sf::st_transform(sf_data, crs = 28992)
}

#' @rdname GIS
#' @param longitudes vector of longitudes
#' @param latitudes vector of latitudes
#' @param crs the coordinate reference system (CRS) to use as output
#' @export
#' @examples
#' 
#' 
#' # Other functions ------------------------------------------------------
#' 
#' degrees_to_sf(4.5, 54)
#' 
#' if (require("certeplot2")) {
#'   geo_provincies |>
#'       crop_certe() |> 
#'       plot2(category = NULL, colour_fill = NA) |> 
#'       add_sf(degrees_to_sf(6.5, 53),
#'              datalabels = "Some Point!")
#' }
degrees_to_sf <- function(longitudes, latitudes, crs = 28992) {
  check_is_installed("sf")
  loadNamespace("sf")
  sf::st_as_sf(data.frame(long = longitudes, lat = latitudes),
               coords = c("long", "lat"),
               crs = 4326) |> 
    sf::st_transform(crs = crs)
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
  loadNamespace("sf")
  
  if (!isTRUE(sf::st_is_longlat(sf_data))) {
    sf_data <- sf::st_transform(sf_data, crs = 4326)
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
  loadNamespace("sf")
  
  if (!isTRUE(sf::st_is_longlat(sf_data))) {
    sf_data <- sf::st_transform(sf_data, crs = 4326)
  }
  as.data.frame(
    sf::st_coordinates(
      suppressWarnings(sf::st_centroid(sf_data))
    )
  )$X
}
