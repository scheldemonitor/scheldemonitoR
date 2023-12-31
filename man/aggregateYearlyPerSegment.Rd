% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TemporalAggregating.R
\name{aggregateYearlyPerSegment}
\alias{aggregateYearlyPerSegment}
\title{Aggregate yearly per segment (level 4)}
\usage{
aggregateYearlyPerSegment(data, stat = "mean")
}
\arguments{
\item{data}{A dataframe with \code{datetime}, \code{stationname}, \code{value} and (Scheldt) \code{segment} columns. For example, the result
of \code{\link[=assignGeometry]{assignGeometry()}}.}

\item{stat}{The statistics to aggregate data. Predifined option is "\code{mean}". See Details for the full list.}
}
\value{
A dataframe.
}
\description{
Aggregate yearly per segment (level 4).
}
\details{
Available statistics are:
\itemize{
\item "\code{mean}" calculates Arithmetic Mean, uses \code{\link[base:mean]{base::mean()}},
\item "\code{sd}" calculates Stadard Deviation, uses \code{\link[stats:sd]{stats::sd()}}
\item "\code{se}" calculates Standard Error,
\item "\code{median}" calculates the Median Value. uses \code{\link[stats:median]{stats::median()}},
\item "\code{min}" calculates the minima, uses \code{\link[base:Extremes]{base::min()}},
\item "\code{max}" calculates the maxima, uses \code{\link[base:Extremes]{base::max()}},
\item "\verb{5perc}" calculates the 5th percentile,
\item "\verb{10perc}" calculates the 10th percentile,
\item "\verb{90perc}" calculates the 90th percentile,
\item "\verb{99perc}" calculates the 99th percentile,
\item "\code{sum}" calculates the sum, uses \code{\link[base:sum]{base::sum()}}
}
}
\examples{
# import data from high- and lowtide parameters
tide_parameters <- c(9694,9695)

# download data
abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)

# assign geometry
abiotic_data_geo <- assignGeometry(abiotic_data, "T2021", TRUE)

# calculate monthly mean per segment
abiotic_data_mean <- aggregateYearlyPerSegment(abiotic_data_geo, "mean")
}
