% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{inwoners_per_postcode_leeftijd}
\alias{inwoners_per_postcode_leeftijd}
\title{Number of Inhabitants per Zip Code and Age}
\format{
A \link{data.frame} with 99,260 observations and 5 variables:
\itemize{
\item \code{postcode}\cr zip code, contains PC2, PC3 and PC4
\item \code{leeftijd}\cr age group per 5 years: 0-4, 5-9, ..., 90-94, 95+
\item \code{inwoners}\cr total number of inhabitants
\item \code{inwoners_man}\cr total number of male inhabitants
\item \code{inwoners_vrouw}\cr total number of female inhabitants
}
}
\source{
The data in this \link{data.frame} are retrieved from, and publicly available at, Statistics Netherlands: \emph{StatLine}, Centraal Bureau voor de Statistiek (CBS), 'Bevolking en leeftijd per postcode' (data set 83502NED), 1 januari 2021, \url{https://opendata.cbs.nl}.
}
\usage{
inwoners_per_postcode_leeftijd
}
\description{
Number of Inhabitants per Zip Code and Age
}
\details{
See \href{https://github.com/certe-medical-epidemiology/certegis/blob/main/data-raw/update_gis.R}{the repository file} to update this data set.
}
\examples{
head(inwoners_per_postcode_leeftijd)
str(inwoners_per_postcode_leeftijd)
}
\keyword{datasets}
