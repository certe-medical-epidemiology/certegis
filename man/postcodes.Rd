% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{postcodes}
\alias{postcodes}
\title{Data Set with Dutch Zip Codes, Cities, Municipalities and Province}
\format{
A \link{data.frame} with 4,963 observations and 9 variables:
\itemize{
\item \code{postcode}\cr zip code, contains PC2, PC3 and PC4
\item \code{inwoners}\cr total number of inhabitants
\item \code{inwoners_man}\cr total number of male inhabitants
\item \code{inwoners_vrouw}\cr total number of female inhabitants
\item \code{plaats}\cr formal Dutch city name
\item \code{gemeente}\cr formal Dutch municipality name
\item \code{provincie}\cr formal Dutch province name
\item \code{nuts3}\cr Nomenclature of Territorial Units for Statistics, level 3 (in Dutch: COROP region, \emph{Coordinatie Commissie Regionaal OnderzoeksProgramma})
\item \code{ggdregio}\cr name of the regional GGD service (public healthcare service)
}
}
\source{
The data in this \link{data.frame} are retrieved from, and publicly available at, Statistics Netherlands: \emph{StatLine}, Centraal Bureau voor de Statistiek (CBS), 'Bevolking per geslacht per postcode' (data set 83503NED), 1 januari 2021, \url{https://opendata.cbs.nl}.
}
\usage{
postcodes
}
\description{
Data Set with Dutch Zip Codes, Cities, Municipalities and Province
}
\details{
See \href{https://github.com/certe-medical-epidemiology/certegis/blob/main/data-raw/update_gis.R}{the repository file} to update this data set.
}
\examples{
head(postcodes)
str(postcodes)
}
\keyword{datasets}
