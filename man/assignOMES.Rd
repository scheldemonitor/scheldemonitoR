% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AssignRegion.R
\name{assignOMES}
\alias{assignOMES}
\title{Assign OMES-segments}
\usage{
assignOMES(data, map, external)
}
\arguments{
\item{data}{A dataframe with 'latitude' and 'longitude' columns. For example, the result
of \code{\link[=importAbioticData]{importAbioticData()}} or \code{\link[=importBioticData]{importBioticData()}}}

\item{map}{Two options:
\itemize{
\item "T2015" to use the Scheldt Estuary shapefile of the T2015 report
\item "T2021" to use the Scheldt Estuary shapefile of the T2021 report
}}

\item{external}{Logical. Select \code{TRUE} to conditionally check stations outside standard geometry (like boundary stations)}
}
\value{
Input dataframe with extra column \code{omes} added for omes segment.
}
\description{
\code{assignOMES()} assigns Scheldt OMES segments (= omes) as an extra column of the input dataframe. The function uses
the overarching function \code{\link[=assignGeometry]{assignGeometry()}}
}
\examples{
# import data from high- and lowtide parameters
tide_parameters <- c(9694,9695)

# download data
abiotic_data <- importAbioticData(tide_parameters, start = 2010, end = 2012)

# add OMES segment
abiotic_data_omes <- assignOMES(abiotic_data, 'T2015', TRUE)
}
