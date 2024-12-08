% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cbs_geodata}
\alias{cbs_geodata}
\alias{geo_gemeenten}
\alias{geo_ggdregios}
\alias{geo_nuts3}
\alias{geo_postcodes2}
\alias{geo_postcodes3}
\alias{geo_postcodes4}
\alias{geo_postcodes6}
\alias{geo_provincies}
\title{Data Sets with Geometries of Dutch Provinces, Municipalities and Zip Codes}
\format{
An object of class \code{sf} (inherits from \code{data.frame}) with 345 rows and 4 columns.

An object of class \code{sf} (inherits from \code{data.frame}) with 25 rows and 4 columns.

An object of class \code{sf} (inherits from \code{data.frame}) with 40 rows and 4 columns.

An object of class \code{sf} (inherits from \code{data.frame}) with 90 rows and 4 columns.

An object of class \code{sf} (inherits from \code{data.frame}) with 798 rows and 4 columns.

An object of class \code{sf} (inherits from \code{data.frame}) with 4068 rows and 4 columns.

An object of class \code{sf} (inherits from \code{data.frame}) with 58481 rows and 4 columns.

An object of class \code{sf} (inherits from \code{data.frame}) with 12 rows and 4 columns.
}
\source{
The data in these \link{data.frame}s are retrieved from, and publicly available at, Statistics Netherlands:
\itemize{
\item Centraal Bureau voor de Statistiek (CBS), 'Gebiedsindelingen', GPKG 2022 v1, \url{https://www.cbs.nl}
\item Centraal Bureau voor de Statistiek (CBS), 'Kerncijfers per postcode', ZIP 2020 v1, \url{https://www.cbs.nl}
}
}
\usage{
geo_gemeenten

geo_ggdregios

geo_nuts3

geo_postcodes2

geo_postcodes3

geo_postcodes4

geo_postcodes6

geo_provincies
}
\description{
Data Sets with Geometries of Dutch Provinces, Municipalities and Zip Codes
}
\details{
These \link{data.frame}s are of additional class \code{sf} and contain 3 variables:
\itemize{
\item \code{...}\cr name of the area, these are: geo_gemeenten$gemeente, geo_ggdregios$ggdregio, geo_nuts3$nuts3, geo_postcodes2$postcode, geo_postcodes3$postcode, geo_postcodes4$postcode, geo_postcodes6$postcode, geo_provincies$provincie
\item \code{inwoners}\cr number of inhabitants in the area
\item \code{oppervlakte_km2}\cr area in square kilometres
\item \code{geometry}\cr multipolygonal object of the area
}

All data sets have the coordinate reference system (CRS) set to \href{https://epsg.io/28992}{EPSG:28992} ('RD New'), following the sphere of Earth. They can be flattened to e.g. \href{https://epsg.io/4326}{EPSG:4326} ('WGS 84') using \code{\link[sf:st_transform]{st_transform()}}.

See \href{https://github.com/certe-medical-epidemiology/certegis/blob/main/data-raw/update_gis.R}{the repository file} to update these data sets.

\strong{NOTE}: all data sets contains all areas of the whole country of the Netherlands, except for \code{geo_postcodes6} which was cropped to only cover the Certe region (using \code{\link[=crop_certe]{crop_certe()}}).
}
\examples{
if (require("certeplot2")) {

  geo_postcodes6 |>
    filter_geolocation(plaats == "Groningen") |>
    plot2(category = inwoners / oppervlakte_km2,
          datalabels = FALSE,
          title = "City of Groningen (PC6 level)")
  
}

if (require("certeplot2")) {

  geo_postcodes4 |>
    filter_geolocation(plaats == "Groningen") |>
    plot2(category = inwoners / oppervlakte_km2,
          datalabels = FALSE,
          title = "City of Groningen (PC4 level)")
  
}

if (require("sf")) {

  head(geo_gemeenten)

}
}
\keyword{datasets}
