% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TemporalAggregating.R
\name{aggregateDailyPerStation}
\alias{aggregateDailyPerStation}
\title{Aggregate data daily per station}
\usage{
aggregateDailyPerStation(data, stat = "min")
}
\arguments{
\item{data}{A dataframe with \code{datetime}, \code{stationname} and \code{value} columns. For example, the result
of \code{\link[=importAbioticData]{importAbioticData()}} or \code{\link[=importBioticData]{importBioticData()}}.}

\item{stat}{The statistics to aggregate data. Eather "min" or "max". Predifined option is "\code{min}".}
}
\value{
A dataframe.
}
\description{
Aggregates data per day and per station.
}
\examples{
# import data from high- and lowtide parameters
tide_parameters <- c(9694,9695)

# download data
abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)

# calculate daily min per station
abiotic_data_min <- aggregateDailyPerStation(abiotic_data, stat = "min")
}
