library(dplyr)
library(tidyr)
library(readr)
library(AMR) # voor age_groups()
library(sf) # let op: minimaal v1.0!
library(cleaner)

## HIERNA R/data.R UPDATEN MET VERSIENUMMER

# MB/ 2021-12-31 ik kon vandaag dit hele script doorlopen zonder fouten.
# MB/ 2022-05-02 PC6 toegevoegd

# Bronnen -----------------------------------------------------------------

downloadmap <- "/Users/msberends/Downloads/"

# download inwoners (gescheiden op geslacht) per postcode hier als 'CSV volgens tabelindeling':
# https://opendata.cbs.nl/#/CBS/nl/dataset/83503NED/table?dl=5F8EB
# verwijder dan de eerste rijen en de laatste rij ("Bron: CBS")
postcodes_bestand <- paste0(downloadmap, "Bevolking__geslacht__migratieachtergrond__viercijferige_postcode__1_januari_30122021_210450.csv")

# download inwoners per 5 jaar leeftijd en geslacht hier voor het huidige jaar als 'CSV zonder statistische symbolen:
# https://opendata.cbs.nl/#/CBS/nl/dataset/83502NED/table?dl=42FE0
inwoners_bestand <- paste0(downloadmap, "Bevolking__leeftijd__postcode_30122021_210658.csv")

# download gebiedsindelingen hier:
# https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/cbs-gebiedsindelingen
gebiedsindelingen_bestand <- paste0(downloadmap, "cbsgebiedsindelingen_2022_v1.gpkg")

# download postcodes 4 onder 'Downloads' ('Numeriek deel van de postcode (PC4)') hier:
# https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/gegevens-per-postcode
# NB. eind 2021 stond de link voor 2020 niet op de site. De link van 2017 wel:
# https://download.cbs.nl/postcode/CBS-PC4-2017-v3.zip
# daarvan gemaakt:
# https://download.cbs.nl/postcode/CBS-PC4-2020-v1.zip (en die bestond gewoon)
postcodes4_bestand <- paste0(downloadmap, "CBS-PC4-2020-v1/CBS_PC4_2020_v1.shp")
# voor PC6: https://download.cbs.nl/postcode/CBS-PC6-2020-v1.zip
postcodes6_bestand <- paste0(downloadmap, "CBS-PC6-2020-v1/CBS_PC6_2020_v1.shp")


# Helpfuncties ------------------------------------------------------------

kaart_fixen <- function(kaart) {
  # bij CBS hebben ze als CRS "Amersfoort / RD New"
  # deze functie maakt daar "WGS 84" van, zodat we graden hebben
  
  # CRS opnieuw instellen, bij nieuwe GDAL geeft dit anders problemen:
  st_crs(kaart) <- clean_numeric(st_crs(kaart)$input)
  if (is.na(st_crs(kaart))) {
    # 'reserve' CRS
    st_crs(kaart) <- 28992
  }
  if (!"geometry" %in% colnames(kaart)) {
    geom_naam <- c("shape", "geom")
    geom_naam <- colnames(kaart)[colnames(kaart) %in% geom_naam][1]
    geometry <- kaart[, colnames(kaart)[tolower(colnames(kaart)) == geom_naam], drop = TRUE]
    kaart <- st_drop_geometry(kaart)
    kaart <- st_set_geometry(kaart, geometry)
  }
  # geometrie als breedte- en lengtegraad, CRS transformeren van 28992 naar 4326
  kaart <- st_transform(kaart, crs = 4326)

  # alle ongeldige vormen geldig maken
  if (any(!st_is_valid(kaart$geometry))) {
    kaart$geometry <- st_make_valid(kaart$geometry)
  }

  # CRS opnieuw instellen, bij nieuwe GDAL geeft dit anders problemen:
  st_crs(kaart) <- clean_numeric(st_crs(kaart)$input)
  if (is.na(st_crs(kaart))) {
    # 'reserve' CRS
    st_crs(kaart) <- 28992
  }
  # geometrie als breedte- en lengtegraad, CRS transformeren van 28992 naar 4326
  kaart <- st_transform(kaart, crs = 4326)
  # oppervlakte toevoegen
  kaart$area_km2 <- as.double(st_area(st_geometry(kaart)) / 1000 ^ 2)
  kaart
}

lagen_beschikbaar <- sort(st_layers(gebiedsindelingen_bestand)$name)
downloaden_transformeren <- function(laag) {
  zoeklaag <- sort(lagen_beschikbaar[lagen_beschikbaar %like% laag &
                                       lagen_beschikbaar %like% "gegeneraliseerd" &
                                       !lagen_beschikbaar %like% "niet"])
  if (length(zoeklaag) == 0) {
    stop("Geen laag gevonden")
  }
  zoeklaag <- zoeklaag[length(zoeklaag)]
  message("Laag '", zoeklaag, "' gevonden", appendLF = FALSE)

  kaart <- st_read(gebiedsindelingen_bestand, layer = zoeklaag, quiet = TRUE)
  kaart <- kaart_fixen(kaart)

  message(", met ", nrow(kaart), " geometrieën")
  colnames(kaart)[colnames(kaart) %like% "naam"] <- laag
  # geen factor
  kaart[, laag] <- as.character(kaart[, laag, drop = TRUE])

  # alleen het type, oppervlakte en de geometrie behouden
  kaart <- kaart[, c(laag, "area_km2", "geometry"), drop = FALSE]
  kaart
}


# Inwoners per postcode/leeftijd/geslacht ---------------------------------

inwoners_per_postcode_leeftijd <- read_csv2(inwoners_bestand)
inwoners_per_postcode_leeftijd <- inwoners_per_postcode_leeftijd %>%
  rename(leeftijd = Leeftijd, postcode = Postcode, geslacht = Geslacht, inwoners = `Bevolking (aantal)`) %>% 
  filter(!leeftijd %like% "totaal", postcode %like% "[0-9]") %>%
  mutate(postcode = as.double(postcode),
         geslacht = case_when(geslacht %like% "totaal" ~ "inwoners",
                              geslacht %like% "man"    ~ "inwoners_man",
                              geslacht %like% "vrouw"  ~ "inwoners_vrouw"),
         # bij CBS vinden ze 0-5 en 5-10 handig. Waarin zit iemand dan die 5 is?! 
         # ze noemen het "0 tot 5", dus we nemen aan "tot en met 4" en maken er 0-4 en 5-9 van:
         leeftijd_min = as.numeric(gsub("([0-9]+).*", "\\1", leeftijd)),
         leeftijd_max = as.numeric(gsub("([0-9]+)[^0-9]+([0-9]+)[^0-9]+", "\\2", leeftijd)),
         leeftijd_nieuw = paste0(leeftijd_min, "-", leeftijd_max - 1),
         leeftijd_nieuw = gsub("95-NA", "95+", leeftijd_nieuw),
         leeftijd = factor(leeftijd_nieuw, levels = levels(age_groups(0, 5 * c(1:19))), ordered = TRUE)) %>%
  select(postcode, geslacht, leeftijd, inwoners) %>%
  pivot_wider(names_from = geslacht, values_from = inwoners)
# alle PC2 en PC3 toevoegen
inwoners_per_postcode_leeftijd <- inwoners_per_postcode_leeftijd %>%
  bind_rows(inwoners_per_postcode_leeftijd %>%
              group_by(postcode = clean_numeric(substr(postcode, 1, 2)), leeftijd) %>%
              summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop")) %>%
  bind_rows(inwoners_per_postcode_leeftijd %>%
              group_by(postcode = clean_numeric(substr(postcode, 1, 3)), leeftijd) %>%
              summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop")) %>%
  arrange(postcode, leeftijd)
# korte check, moet allemaal gelijk zijn:
inwoners_per_postcode_leeftijd %>% filter(postcode > 999) %>% pull(inwoners) %>% sum(na.rm = TRUE) # pc4
inwoners_per_postcode_leeftijd %>% filter(!postcode < 100, !postcode > 999) %>% pull(inwoners) %>% sum(na.rm = TRUE) # pc3
inwoners_per_postcode_leeftijd %>% filter(postcode < 100) %>% pull(inwoners) %>% sum(na.rm = TRUE) # pc2

# Postcodes (wordt later alle referentiedata aan toegevoegd) --------------

# `postcodes` is hier de vorige versie die we als `postcodes` gebruikten, deze wordt vernieuwd
postcodes_plaats_gemeente <- certegis::postcodes %>%
  filter(postcode > 999) %>% # alleen PC4 houden, wordt later weer aangevuld met PC2 en PC3
  select(postcode, plaats, gemeente)

postcodes <- read_csv2(postcodes_bestand)
colnames(postcodes) <- c("postcode", "inwoners", "inwoners_man", "inwoners_vrouw")
postcodes <- postcodes %>% 
  filter(postcode %like% "[0-9]") %>% 
  mutate(postcode = as.double(postcode))

# Postcode-4 kaart --------------------------------------------------------

# we gebruiken deze postcodekaart om te bepalen welke postcodes in welk gebied liggen met sf::st_intersects().
# dus de geometrie van postcode 9251 valt in het snijvlak van de de geometrie van de gemeente Tytsjerksteradiel
# en dus is Tytsjerksteradiel de gemeente van postcode 9251 (en zo verder voor NUTS-3, GGD-regio, ...)
geo_postcodes4 <- st_read(postcodes4_bestand)
geo_postcodes4 <- kaart_fixen(geo_postcodes4) # duurt ca. 20 sec.
# alleen relevante kolommen houden
geo_postcodes4 <- geo_postcodes4 %>%
  transmute(postcode = as.double(as.character(PC4)),
            huishoudens = ifelse(AANTAL_HH < 0, NA_real_, AANTAL_HH),
            huishouden_grootte = ifelse(GEM_HH_GR < 0, NA_real_, GEM_HH_GR),
            area_km2 = as.double(st_area(geometry) / 1000 ^ 2),
            geometry)

# Postcode-6 kaart --------------------------------------------------------

geo_postcodes6 <- st_read(postcodes6_bestand)
geo_postcodes6 <- kaart_fixen(geo_postcodes6) # duurt ca. 2 min.
# alleen relevante kolommen houden
geo_postcodes6 <- geo_postcodes6 %>%
  transmute(postcode = as.character(PC6),
            inwoners = as.double(INWONER),
            oppervlakte_km2 = as.double(st_area(geometry) / 1000 ^ 2),
            geometry)
geo_postcodes6$inwoners[geo_postcodes6$inwoners < 0] <- 0

# Referentiewaarden aan `postcodes` toevoegen en kaarten opslaan ----------

sf_use_s2.old <- sf_use_s2()
sf_use_s2(FALSE) # uitzetten

relevante_lagen <- c("gemeente",
                     "provincie",
                     "nuts3",
                     "ggdregio")
for (i in 1:length(relevante_lagen)) {
  message("\n>> zoeken naar ", relevante_lagen[i])
  kaart <- downloaden_transformeren(relevante_lagen[i])
  x_kaart <<- kaart
  if (!relevante_lagen[i] %in% c("plaats", "gemeente")) {
  # referentiedata toevoegen aan 'postcodes'
    p <- dplyr::progress_estimated(length(geo_postcodes4$geometry))
    newvar <- character(length = nrow(geo_postcodes4))
    for (pc in 1:nrow(geo_postcodes4)) {
      x_pc <<- pc
      p$tick()$print()
      suppressMessages(
        verschillen <- as.double(st_area(st_difference(geo_postcodes4 %>% slice(pc),
                                                       kaart)) /
                                   st_area(geo_postcodes4 %>% slice(pc)))
      )
      if (any(verschillen < 1)) {
        newvar[pc] <- as.character(kaart[, 1, drop = TRUE])[verschillen == min(verschillen)]
      } else {
        kaart_ind <- as.double(suppressMessages(unlist(st_intersects(geo_postcodes4 %>% slice(pc), kaart))))[1]
        newvar[pc] <- as.character(kaart[, 1, drop = TRUE])[kaart_ind]
      }
    }
    newdf <- data.frame(postcode = as.double(geo_postcodes4$postcode),
                        newvar = as.character(newvar),
                        stringsAsFactors = FALSE)
    postcodes <- postcodes %>%
      left_join(newdf, by = "postcode")
    colnames(postcodes)[colnames(postcodes) == "newvar"] <- relevante_lagen[i]
  }

  object_naam <- case_when(relevante_lagen[i] == "gemeente" ~ "geo_gemeenten",
                           relevante_lagen[i] == "provincie" ~ "geo_provincies",
                           relevante_lagen[i] == "nuts3" ~ "geo_nuts3",
                           relevante_lagen[i] == "ggdregio" ~ "geo_ggdregios",
                           TRUE ~ paste0(relevante_lagen[i], "_UNDEFINED"))

  # kaart opslaan in Global Environment
  assign(x = object_naam,
         value = kaart,
         envir = globalenv())
}

# uit PC4-kaart van CBS ook nog wat kolommen halen, en die hoeven niet in dat kaartobject
postcodes <- postcodes %>%
  left_join(postcodes_plaats_gemeente, by = "postcode") %>%
  select(postcode, matches("inwoner"), "plaats", "gemeente", "provincie", everything()) %>%
  left_join(geo_postcodes4 %>%
              as.data.frame(stringsAsFactors = FALSE) %>%
              select(-area_km2, -geometry),
            by = "postcode")

# alles van PC2 en PC3 toevoegen
postcodes <- postcodes %>%
  bind_rows(postcodes %>%
              group_by(postcode = clean_numeric(substr(postcode, 1, 2))) %>%
              summarise(across(matches("inwoner"), sum, na.rm = TRUE),
                        across(matches("huishoudens"), sum, na.rm = TRUE),
                        across(matches("huishouden_grootte"), mean, na.rm = TRUE),
                        across(where(is.character), function(x) x[1])) %>% 
              select(colnames(postcodes))) %>%
  bind_rows(postcodes %>%
              group_by(postcode = clean_numeric(substr(postcode, 1, 3))) %>%
              summarise(across(matches("inwoner"), sum, na.rm = TRUE),
                        across(matches("huishoudens"), sum, na.rm = TRUE),
                        across(matches("huishouden_grootte"), mean, na.rm = TRUE),
                        across(where(is.character), function(x) x[1])) %>% 
              select(colnames(postcodes))) %>%
  arrange(postcode)

# huishoudens verwijderen
geo_postcodes4 <- geo_postcodes4 %>% 
  select(postcode, area_km2, geometry)

# inwoners toevoegen aan de kaarten
inwoners_toevoegen <- function(kaart) {
  out <- kaart %>%
    left_join(postcodes %>%
                filter(postcode > 999) %>%
                group_by(across(colnames(kaart)[1])) %>%
                summarise(inwoners = sum(inwoners, na.rm = TRUE))) %>%
    relocate(inwoners, .before = area_km2) %>%
    rename(oppervlakte_km2 = area_km2) %>% 
    arrange(across(colnames(.)[1]))
  message("Inwoners: ", sum(out$inwoners, na.rm = TRUE), " (", sum(is.na(out$inwoners)), " NA's)")
  out
}

geo_gemeenten <- inwoners_toevoegen(geo_gemeenten)
geo_ggdregios <- inwoners_toevoegen(geo_ggdregios)
geo_nuts3 <- inwoners_toevoegen(geo_nuts3)
geo_postcodes4 <- inwoners_toevoegen(geo_postcodes4)
geo_provincies <- inwoners_toevoegen(geo_provincies)

# geo_postcodes4 is veel te groot, we maken hem simpeler
geo_postcodes4 <- st_simplify(geo_postcodes4, dTolerance = 0.0001)
geo_postcodes4$geometry <- st_cast(geo_postcodes4$geometry, , "MULTIPOLYGON")

# geo_postcodes6 is ook veel te groot en bevat veel te veel detail
geo_postcodes6 <- st_simplify(geo_postcodes6, dTolerance = 5)
geo_postcodes6$geometry <- st_cast(geo_postcodes6$geometry, , "MULTIPOLYGON")
geo_postcodes6 <- crop_certe(geo_postcodes6)

# "Fryslân" vervangen door "Friesland"
geo_provincies$provincie <- gsub("Fryslân", "Friesland", geo_provincies$provincie, fixed = TRUE)
postcodes$provincie <- gsub("Fryslân", "Friesland", postcodes$provincie, fixed = TRUE)

# nu kan alles opgeslagen worden in het certegis pakket:
usethis::use_data(postcodes, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)
usethis::use_data(inwoners_per_postcode_leeftijd, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)
usethis::use_data(geo_gemeenten, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)
usethis::use_data(geo_ggdregios, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)
usethis::use_data(geo_nuts3, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)
usethis::use_data(geo_postcodes4, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)
usethis::use_data(geo_provincies, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)
usethis::use_data(geo_postcodes6, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)

# vanuit geo_postcodes4 ook geo_postcodes2 en geo_postcodes3 maken
geo_postcodes2 <- geo_postcodes4 %>%
  as.data.frame() %>%
  mutate(postcode = as.double(substr(postcode, 1, 2))) %>%
  group_by(postcode) %>%
  summarise(inwoners = sum(inwoners, na.rm = TRUE),
            oppervlakte_km2 = sum(oppervlakte_km2, na.rm = TRUE),
            geometry = st_union(geometry)) %>% 
  as.data.frame() %>%
  st_as_sf()

geo_postcodes3 <- geo_postcodes4 %>%
  as.data.frame() %>%
  mutate(postcode = as.double(substr(postcode, 1, 3))) %>%
  group_by(postcode) %>%
  summarise(inwoners = sum(inwoners, na.rm = TRUE),
            oppervlakte_km2 = sum(oppervlakte_km2, na.rm = TRUE),
            geometry = st_union(geometry)) %>% 
  as.data.frame() %>%
  st_as_sf()

# instelling terugzetten
sf_use_s2(sf_use_s2.old)

usethis::use_data(geo_postcodes2, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)
usethis::use_data(geo_postcodes3, overwrite = TRUE, internal = FALSE, compress = "xz", version = 2)

