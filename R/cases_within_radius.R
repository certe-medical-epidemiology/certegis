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

#' Aantal casussen binnen kilometerbereik bepalen
#'
#' Met deze functie kan bepaald worden of er binnen een geografisch bereik van een postcode een minimaal aantal casussen (van bijv. een virus, isolaat of aandoening) voorgekomen is.
#' @param data Een data set met postcodes, gefilterd op positieve gevallen. Deze dataset moet een kolom 'postcode' bevatten met PC4-waarden. Zie Examples.
#' @param data_sf Een sf-object met in ieder geval deze kolommen: de postcode (instellen met \code{column_zipcodes}) en het aantal casussen (instellen met \code{column_count}).
#' @param radius_km Standaard is 10. De straal in kilometers waarbinnen het totaal \code{minimal_cases} voor moet komen, vanaf elke postcode. Dit is een thresholdparameter. Het kan voorkomen dat bijv. een postcode 10 km links van de postcode en een postcode 10 km rechts van postcode meegeteld worden. Dus de totale zoekdiameter rond elke postcode is 2x \code{radius_km}.
#' @param minimal_cases Standaard is 10. Het minimaal aantal casussen dat voor moet komen binnen het geografisch gebied. Dit is een thresholdparameter.
#' @param column_zipcodes Standaard is \code{"postcode"}. De kolomnaam van de postcodes.
#' @param column_count Standaard is eerste kolom die numeriek is en waarbij alle waarden binnen [0, 999] vallen.
#' @param info Standaard is \code{TRUE}. Printen van geo-analyse en voortgang.
#' @param slack Resultaat naar Slack versturen, kanaal #early-warning.
#' @param topic Standaard is leeg. Onderwerp om in geformatteerde tekst op te nemen. Dit kan de naam van een virus, bacterie of aandoening zijn.
#' @return Een \code{list} met de postcodes als namen, waarbij elk element een \code{data.frame} bevat met postcodes, aantallen en afstand in kilometers wanneer er van die postcode minimaal \code{minimal_cases} gevonden zijn binnen een straal van \code{radius_km} kilometer. Als dit niet het geval is, bevat het element \code{NA}.
#'
#' \code{cases_within_radius()} retourneert onzichtbaar de output van \code{cases_within_radius.sf()}. Gebruik \code{format()} of \code{format2()} om een printbaar formaat te maken, dat verzonden kan worden per mail of Slack.
#'
#' Met \code{rbind()} worden alle 'positieve' postcodes samengevoegd tot 1 \code{data.frame}, dat een geldig sf-object is.
#' @rdname cases_within_radius
#' @exportMethod cases_within_radius
#' @export
#' @examples
#' \dontrun{
#' noro <- certedb_getmmb(where = db$t.testcode == "PRNORO",
#'                        zipcodes = TRUE)
#'
#' # als je cases_within_radius() op 'normale' data gebruikt, worden de
#' # postcodes van de dataset 'postcodes4' genomen, wordt de inputdata
#' # gefilterd op positieven, zo nodig gegroepeerd op postcode en geteld,
#' # en wordt dan cases_within_radius.sf() gedraaid.
#' noro %>%
#'    cases_within_radius(radius_km = 5,
#'                        minimal_cases = 10,
#'                        topic = "norovirus",
#'                        slack = TRUE)
#'
#'
#'
#' data.frame(ordernr = c("abc", "def"),
#'            postcode = c(8900, 8901),
#'            n = c(10, 20)) %>%
#'   add_map("pc4") %>%
#'   cases_within_radius(radius_km = 5, # <- dit is dus cases_within_radius.sf()
#'                       minimal_cases = 10) %>%
#'   format(topic = "SARS-CoV-2") %>%
#'   teams("early-warning")
#' }
cases_within_radius <- function(...) {
  UseMethod("cases_within_radius")
}

#' @rdname cases_within_radius
#' @method cases_within_radius default
#' @export
cases_within_radius.default <- function(data,
                                        radius_km = 10,
                                        minimal_cases = 10,
                                        topic = NULL,
                                        slack = TRUE) {
  if (is.null(topic)) {
    topic_txt <- "(undefined bug)"
  } else {
    topic_txt <- topic
  }
  if (nrow(data) == 0) {
    message("Geen cassusen met ", topic_txt)
    return(invisible(NULL))
  }
  if ("postcode" %in% colnames(data) | !any(data$postcode %unlike% "^[0-9]{4}$")) {
    stop("`data` must contain a column 'postcode' with only PC4 values.")
  }
  if ("uitslag_int" %in% colnames(data)) {
    data <- data %>% filter(uitslag_int == "Positief")
  }
  if ("ontvangstdatum" %in% colnames(data)) {
    dates <- paste0(" (", format2(min(data$ontvangstdatum), "d mmm yyyy"), " t/m ", format2(max(data$ontvangstdatum), "d mmm yyyy"), ")")
  } else {
    dates <- ""
  }
  if (!"n" %in% colnames(data)) {
    data <- data %>%
      mutate(postcode = substr(postcode, 1, 4)) %>%
      count(postcode)
  }
  dataset <- postcodes4 %>%
    left_join(data, by = "postcode")
  if (nrow(dataset) > 0) {
    cases <- dataset %>%
      sf::st_as_sf() %>%
      cases_within_radius.sf(radius_km = radius_km,
                             minimal_cases = minimal_cases,
                             column_count = "n")
    if (all(is.na(cases))) {
      message("Minder dan ", minimal_cases, " cassusen met ", topic_txt)
      return(invisible(NULL))
    }
    cases_formatted <- format(cases)
    if (length(cases_formatted) > 0 & slack == TRUE) {
      if (topic %in% AMR::microorganisms$fullname) {
        topic <- paste0("_", topic, "_")
      }
      topic <- paste0("*", topic, dates, "*")
      teams(c(topic, cases_formatted), "early-warning")
    } else {
      message(length(cases_formatted), " rijen voor ", topic_txt)
    }
    return(invisible(cases))
  } else {
    message("Geen cassusen met ", topic_txt)
    return(invisible(NULL))
  }
}

#' @rdname cases_within_radius
#' @method cases_within_radius sf
#' @export
cases_within_radius.sf <- function(data_sf,
                                   radius_km = 10,
                                   minimal_cases = 10,
                                   column_zipcodes = "postcode",
                                   column_count = NULL,
                                   info = TRUE) {
  
  if (!column_zipcodes %in% colnames(data_sf)) {
    stop("Column for zip codes not found in data: ", column_zipcodes, call. = FALSE)
  }
  data_sf <- data_sf %>% arrange_at(column_zipcodes)
  
  column_count <- deparse(column_count)
  column_count <- gsub('(^"|"$)', "", column_count)
  if (column_count != "NULL" & !column_count %in% colnames(data_sf)) {
    stop("Column not found in data: ", column_count)
  } else if (column_count == "NULL") {
    # als kolom "n" of "aantal.*" voorkomt, die nemen
    if (any(colnames(data_sf) %like% "(^n$|aantal)" & colnames(data_sf) %unlike% "(_mul|_add?r)")) {
      column_count <- colnames(data_sf)[colnames(data_sf) %like% "(^n$|aantal)" & colnames(data_sf) %unlike% "(_mul|_add?r)"][1]
    } else {
      # eerste kolom die numeriek is en waar alle waarden binnen 0:999 vallen
      column_count <- colnames(data_sf)[sapply(data_sf,
                                               function(x) is.numeric(x) & all(x %in% c(0:999), na.rm = TRUE))][1]
    }
    if (is.na(column_count)) {
      stop("`column_count` not set.", call. = FALSE)
    }
    message("Taking column '", column_count, "' for `column_count`")
  }
  
  if (!"n" %in% colnames(data_sf)) {
    colnames(data_sf)[colnames(data_sf) == column_count] <- "n"
  }
  
  afstand_lst <- as.list(rep(NA, nrow(data_sf)))
  names(afstand_lst) <- paste0("pc_", data_sf[, column_zipcodes, drop = TRUE])
  
  if (sum(data_sf$n, na.rm = TRUE) < minimal_cases) {
    return(afstand_lst)
  }
  
  lookup_pc <- data.frame(pc1 = character(0),
                          pc2 = character(0),
                          afstand = double(0),
                          stringsAsFactors = FALSE)
  
  check_previous <- function(pc_1, pc_2, lookup_pc) {
    chck <- postcodes4_afstanden %>%
      filter((postcode.x == pc_1 & postcode.y == pc_2) | (postcode.x == pc_2 & postcode.y == pc_1))
    if (nrow(chck) > 0) {
      chck[1, ]$afstand_km
    } else {
      if (info == TRUE) {
        cat(".") # laat voortgang zien
      }
      NULL
    }
  }
  counter_total <- data_sf %>% filter(!is.na(n)) %>% nrow()
  counter <- 0
  for (i in seq_len(nrow(data_sf))) {
    if (is.na(data_sf[i, ]$n)) {
      next
    }
    counter <- counter + 1
    
    if (info == TRUE) {
      cat(paste0("Calculating distances from zip code ", data_sf[i, column_zipcodes, drop = TRUE], "..."))
    }
    ref_geometry <- data_sf[i, ]$geometry
    ref_pc <- data_sf[i, column_zipcodes, drop = TRUE]
    x <- data_sf %>%
      filter(.$n > 0 | row_number() == i) %>%
      mutate(afstand = NA_real_)
    for (j in seq_len(nrow(x))) {
      if (radius_km > 0) {
        x[j, "afstand"] <- ifelse(ref_pc == x[j, "postcode", drop = TRUE],
                                  as.double(0),
                                  ifelse(!is.null(check_previous(ref_pc, x[j, "postcode", drop = TRUE])),
                                         check_previous(ref_pc, x[j, "postcode", drop = TRUE]),
                                         as.double(sf::st_distance(x[j, ]$geometry, ref_geometry)) / 1000))
      } else {
        x[j, "afstand"] <- 1 # iets dat hoger is dan 0, hierna wordt gefilterd op afstand <= 0
      }
    }
    x <- x %>% filter(afstand <= radius_km)
    cases <- sum(x$n, na.rm = TRUE)
    lookup_pc <- bind_rows(lookup_pc,
                           data.frame(pc1 = rep(ref_pc, nrow(x)),
                                      pc2 = x$postcode,
                                      afstand = x$afstand,
                                      stringsAsFactors = FALSE))
    if (info == TRUE) {
      mult <- ifelse(cases == 1, "", "s")
      if (cases >= minimal_cases) {
        cat(crayon::red(crayon::bold(paste0(" *", cases, " case", mult, "* within ", radius_km, " km"))))
      } else {
        cat(paste0(" ", cases, " case", mult, " within ", radius_km, " km"))
      }
      cat(paste0(" [", percentage(counter / counter_total, digits = 1), "]\n"))
    }
    
    if (cases >= minimal_cases) {
      afstand_lst[[i]] <- x
    }
    cases <- 0
  }
  
  structure(afstand_lst,
            class = c("cases_within_radius", "list"),
            column_count = column_count)
}

#' @rdname cases_within_radius
#' @method format cases_within_radius
#' @export
format.cases_within_radius <- function(x, ...) {
  flagged <- x[sapply(x, function(x) !all(is.na(x)))]
  msg <- character(0)
  
  column_count <- attributes(x)$column_count
  
  for (i in seq_len(length(flagged))) {
    df <- flagged[[i]]
    if (!"n" %in% colnames(df)) {
      colnames(df)[colnames(df) == column_count] <- "n"
    }
    df <- df %>%
      filter(n > 0)
    distances <- df %>%
      pull(afstand) %>%
      sort() %>%
      format2(round = 1, format.NL = TRUE)
    pc <- gsub("pc_", "", names(flagged)[i])
    pc_plaats <- postcodes %>%
      filter(postcode == pc) %>%
      select(plaats, provincie) %>%
      as.character() %>%
      concat(", ")
    pcs <- df %>%
      filter(postcode != pc) %>%
      pull(postcode) %>%
      concat(", ")
    if (pcs != "") {
      pcs <- paste0("; andere postcodes: ", pcs)
      scope <- paste0("binnen een straal van ",
                      paste0(distances[1], "-", distances[length(distances)]),
                      " km rondom")
    } else {
      scope <- "in"
    }
    msg <- c(msg,
             paste0(sum(flagged[[i]]$n), "x gevonden ", scope, " postcode *", gsub("pc_", "", names(flagged)[i]),
                    "* (", pc_plaats, pcs, ")."))
  }
  msg
}

#' @method format2 cases_within_radius
#' @importFrom certestyle format2
#' @rdname cases_within_radius
#' @export
format2.cases_within_radius <- format.cases_within_radius

#' @method rbind cases_within_radius
#' @rdname cases_within_radius
#' @export
rbind.cases_within_radius <- function(x, ...) {
  flagged <- x[sapply(x, function(x) !all(is.na(x)))]
  do.call(rbind, flagged)
}
