library(dplyr)
library(sf)
library(cleaner)

## HIER NA R/data.R UPDATEN MET VERSIENUMMER



# Bronnen -----------------------------------------------------------------

downloadmap <- "/Users/msberends/Downloads/"

# download inwoners (gescheiden op geslacht) per postcode hier als 'CSV volgens tabelindeling':
# https://opendata.cbs.nl/#/CBS/nl/dataset/83503NED/table?dl=42FC6
# verwijder dan de eerste rijen en de laatste rij ("Bron: CBS")
postcodes_bestand <- paste0(downloadmap, "Bevolking__geslacht__migratieachtergrond__viercijferige_postcode__1_januari_15102020_095747.csv")
# download inwoners per 5 jaar leeftijd en geslacht hier voor het huidige jaar:
# https://opendata.cbs.nl/#/CBS/nl/dataset/83502NED/table?dl=42FE0
inwoners_bestand <- paste0(downloadmap, "Bevolking__leeftijd__postcode_15102020_102334.csv")
# gebiedsindelingen hier downloaden:
# https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/cbs-gebiedsindelingen
gebiedsindelingen_bestand <- paste0(downloadmap, "cbsgebiedsindelingen_2021_v1.gpkg")
# download postcodes 4 onder 'Downloads' ('Numeriek deel van de postcode (PC4)') hier:
# https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/gegevens-per-postcode
postcodes4_bestand <- paste0(downloadmap, "2019-CBS_PC4_2018_v1/CBS_PC4_2018_v1.shp")


# Helpfuncties ------------------------------------------------------------

kaart_fixen <- function(kaart) {
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
  filter(!leeftijd %like% "totaal", postcode %like% "[0-9]") %>%
  mutate(postcode = as.double(postcode),
         geslacht = case_when(geslacht %like% "totaal" ~ "inwoners",
                              geslacht %like% "man"    ~ "inwoners_man",
                              geslacht %like% "vrouw"  ~ "inwoners_vrouw"),
         # bij CBS vinden ze 0-5 en 5-10 handig. Waarin zit iemand dan die 5 is?! We maken er 0-4 en 5-9 van:
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
              summarise_all(sum)) %>%
  bind_rows(inwoners_per_postcode_leeftijd %>%
              group_by(postcode = clean_numeric(substr(postcode, 1, 3)), leeftijd) %>%
              summarise_all(sum)) %>%
  arrange(postcode, leeftijd)


# Postcodes (wordt later alle referentiedata aan toegevoegd) --------------

# `postcodes` is hier de vorige versie die we als `postcodes` gebruikten, deze wordt vernieuwd
postcodes_plaats_gemeente <- postcodes %>%
  filter(postcode > 999) %>% # alleen PC4 houden, wordt later weer aangevuld met PC2 en PC3
  select(postcode, plaats, gemeente)

postcodes <- read_csv2(postcodes_bestand)
colnames(postcodes) <- gsub("(man|vrouw)n?en", "\\1", colnames(postcodes))


# Postcode-4 kaart --------------------------------------------------------

# we gebruiken deze postcodekaart om te bepalen welke postcodes in welk gebied liggen met sf::st_intersects().
# dus de geometrie van postcode 9251 valt in het snijvlak van de de geometrie van de gemeente Tytsjerksteradiel
# en dus is Tytsjerksteradiel de gemeente van postcode 9251 (en zo verder voor NUTS-3, GGD-regio, ...)
postcodes4 <- st_read(postcodes4_bestand)
postcodes4 <- kaart_fixen(postcodes4)
# alleen relevante kolommen houden
postcodes4 <- postcodes4 %>%
  transmute(postcode = as.double(as.character(PC4)),
            huishoudens = ifelse(AANTAL_HH < 0, NA_real_, AANTAL_HH),
            huishouden_grootte = ifelse(GEM_HH_GR < 0, NA_real_, GEM_HH_GR),
            area_km2 = as.double(st_area(geometry) / 1000 ^ 2),
            geometry)


# Referentiewaarden aan `postcodes` toevoegen en kaarten opslaan ----------

relevante_lagen <- c("gemeente",
                     "provincie",
                     "nuts3",
                     "ggdregio")
for (i in 3:length(relevante_lagen)) {
  message(">> zoeken naar ", relevante_lagen[i])
  kaart <- downloaden_transformeren(relevante_lagen[i])

  if (!relevante_lagen[i] %in% c("plaats", "gemeente")) {
  # referentiedata toevoegen aan 'postcodes'
    p <- dplyr::progress_estimated(length(postcodes4$geometry))
    newvar <- character(length = nrow(postcodes4))
    for (pc in 1:nrow(postcodes4)) {
      p$tick()$print()
      suppressMessages(
        verschillen <- as.double(st_area(st_difference(postcodes4 %>% slice(pc),
                                                       kaart)) /
                                   st_area(postcodes4 %>% slice(pc)))
      )
      if (any(verschillen < 1)) {
        newvar[pc] <- as.character(kaart[, 1, drop = TRUE])[verschillen == min(verschillen)]
      } else {
        kaart_ind <- as.double(suppressMessages(st_intersects(postcodes4 %>% slice(pc), kaart)))[1]
        newvar[pc] <- as.character(kaart[, 1, drop = TRUE])[kaart_ind]
      }
    }
    newdf <- data.frame(postcode = as.double(postcodes4$postcode),
                         newvar = as.character(newvar), stringsAsFactors = FALSE)
    postcodes <- postcodes %>%
      left_join(newdf, by = "postcode")
    colnames(postcodes)[colnames(postcodes) == "newvar"] <- relevante_lagen[i]
  }

  object_naam <- case_when(relevante_lagen[i] == "gemeente" ~ "gemeenten",
                           relevante_lagen[i] == "nuts3" ~ "nuts3regios",
                           TRUE ~ paste0(relevante_lagen[i], "s"))

  # kaart opslaan in Global Environment
  assign(x = object_naam,
         value = kaart,
         envir = globalenv())
  # kaart opslaan op Z-schijf van Certe
  saveRDS(object = kaart,
          file = .R_REFMAP(paste0("GIS/", object_naam, ".rds")),
          version = 2,
          compress = "xz")
}

# uit PC4-kaart van CBS ook nog wat kolommen halen, en die hoeven niet in dat kaartobject
postcodes <- postcodes %>%
  left_join(postcodes_plaats_gemeente, by = "postcode") %>%
  select(postcode, matches("inwoner"), "plaats", "gemeente", "provincie", everything()) %>%
  left_join(postcodes4 %>%
              as.data.frame(stringsAsFactors = FALSE) %>%
              select(-area_km2, -geometry),
            by = "postcode")

# alles van PC2 en PC3 toevoegen
postcodes <- postcodes %>%
  bind_rows(postcodes %>%
              group_by(postcode = clean_numeric(substr(postcode, 1, 2))) %>%
              summarise_all(function(x, ...) {
                if (is.numeric(x) & any(x > 20)) {
                  # inwoners en aantal huishoudens
                  sum(x, na.rm = TRUE)
                } else if (is.numeric(x)) {
                  # gemiddelde grootte van huishoudens
                  mean(x, na.rm = TRUE)
                } else {
                  x[1]
                }
              })) %>%
  bind_rows(postcodes %>%
              group_by(postcode = clean_numeric(substr(postcode, 1, 3))) %>%
              summarise_all(function(x, ...) {
                if (is.numeric(x) & any(x > 20)) {
                  # inwoners en aantal huishoudens
                  sum(x, na.rm = TRUE)
                } else if (is.numeric(x)) {
                  # gemiddelde grootte van huishoudens
                  mean(x, na.rm = TRUE)
                } else {
                  x[1]
                }
              })) %>%
  arrange(postcode)

# nu kan alles opgeslagen worden in het certetools pakket:
usethis::use_data(gemeenten, overwrite = TRUE, internal = FALSE)
usethis::use_data(ggdregios, overwrite = TRUE, internal = FALSE)
usethis::use_data(inwoners_per_postcode_leeftijd, overwrite = TRUE, internal = FALSE)
usethis::use_data(jeugdregios, overwrite = TRUE, internal = FALSE)
usethis::use_data(nuts3regios, overwrite = TRUE, internal = FALSE)
usethis::use_data(postcodes, overwrite = TRUE, internal = FALSE)
usethis::use_data(provincies, overwrite = TRUE, internal = FALSE)
usethis::use_data(veiligheidsregios, overwrite = TRUE, internal = FALSE)
