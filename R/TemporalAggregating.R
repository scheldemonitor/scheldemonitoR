#' Aggregate data daily per station
#'
#' Aggregates data per day and per station.
#'
#' @param data A dataframe with `datetime`, `stationname` and `value` columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()].
#' @param stat The statistics to aggregate data. Eather "min" or "max". Predifined option is "`min`".
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # calculate daily min per station
#' abiotic_data_min <- aggregateDailyPerStation(abiotic_data, stat = "min")
aggregateDailyPerStation <- function(data, stat = "min"){
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("min", "max"))
  # Determine if all required columns are present
  if(length(which(c("datetime", "stationname", "value") %in% names(data))) != 3){
    stop("Required input: (1) datetime, (2) stationname and (3) value")
  } else {
    message(paste0("Calculate ", stat, " per day and per station"))
  }

  # Remove stations without stationname
  message(paste0("---Removed ", sum(is.na(data$stationname)),
                 " observations without stationname---"))
  data <- data[!is.na(data$stationname), ]

  # Simplify datetime to date
  data$date <- as.Date(as.POSIXct(data$datetime, "%Y-%m-%dT%H:%M:%SZ"))

  # Split data conditional to zone and date
  lst.dat <- split(data, list(data$stationname, data$date), drop = T)

  # Define output list
  lst.day <- list()

  # For each subset, calculate statistic if more than 1 observation
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("latitude", "longitude", "stationname", "segment", "zone",
                 "region", "datetime", "year", "month", "parametername",
                 "unit", "value", "valuesign", "dataprovider")
    lst.day[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # If more than one observation, adapt information
    if(nrow(lst.dat[[i]]) > 1){
      # Determine requested statistic and add to output
      n.stat <- calculateStatistic(lst.dat[[i]], stat = stat)
      lst.day[[i]]$value <- n.stat

      # Determine (new) datetime and add to output
      if(stat %in% c("min", "max")){
        lst.day[[i]]$datetime <-
          max(lst.dat[[i]]$datetime[which(lst.dat[[i]]$value == n.stat)])
      } else {
        if(i == 1){ message("---Using mean of datetimes for new value---")}
        lst.day[[i]]$datetime <- mean(lst.dat[[i]]$datetime)
      }
    }
  }

  # Merge individual statistics and return sorted overview
  df.day <- do.call("rbind", lst.day)
  message(paste0("---Reduced data with ", nrow(data) - nrow(df.day),
                 " observations through merging---"))
  message(paste0("---Daily ", stat, " per station calculated---"))
  return(df.day[order(df.day$stationname, df.day$datetime), ])
}


#' Aggregate data monthly per station
#'
#' Aggregate data monthly per station.
#'
#' @param data A dataframe with `datetime`, `stationname` and `value` columns. For example, the result.
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()]
#' @param stat The statistics to aggregate data. Predifined option is "`mean`". See Details for the full list.
#'
#' @details Available statistics are:
#'    * "`mean`" calculates Arithmetic Mean, uses [base::mean()],
#'    * "`sd`" calculates Stadard Deviation, uses [stats::sd()]
#'    * "`se`" calculates Standard Error,
#'    * "`median`" calculates the Median Value. uses [stats::median()],
#'    * "`min`" calculates the minima, uses [base::min()],
#'    * "`max`" calculates the maxima, uses [base::max()],
#'    * "`5perc`" calculates the 5th percentile,
#'    * "`10perc`" calculates the 10th percentile,
#'    * "`90perc`" calculates the 90th percentile,
#'    * "`99perc`" calculates the 99th percentile,
#'    * "`sum`" calculates the sum, uses [base::sum()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # calculate monthly mean per station
#' abiotic_data_median <- aggregateMonthlyPerStation(abiotic_data, stat = "median")
aggregateMonthlyPerStation <- function(data, stat = "mean"){
  # Assert arguments:
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("mean", "sd", "se", "median", "min", "max", "5perc", "10perc", "90perc", "99perc", "sum"))

  # calculate month and year
  data$year <- lubridate::year(data$datetime)
  data$month <- lubridate::month(data$datetime)

  # Determine if all required columns are present
  if(length(which(c("year", "month", "stationname", "value") %in% names(data))) != 4){
    stop("Required input: (1) year, (2) month, (3) stationname and (4) value")
  } else {
    message(paste0("Calculate ", stat, " per month and per station"))
  }

  # Remove stations without stationname
  message(paste0("---Removed ", sum(is.na(data$stationname)),
                 " observations without stationname---"))
  data <- data[!is.na(data$stationname), ]

  # Split data conditional to zone, year and month
  lst.dat <- split(data, list(data$stationname, data$year, data$month), drop = T)

  # Define output list
  lst.mnth <- list()

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("latitude", "longitude", "stationname", "parametername",
                 "unit", "segment", "zone", "year", "month")
    lst.mnth[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # Determine requested statistic and add to output
    lst.mnth[[i]]$value <- calculateStatistic(lst.dat[[i]], stat = stat)
  }

  # Merge individual statistics and return sorted overview
  df.mnth <- do.call("rbind", lst.mnth)
  message(paste0("---Monthly ", stat, " per station calculated---"))
  return(df.mnth[order(df.mnth$stationname, df.mnth$year, df.mnth$month), ])
}

#' Aggregate data monthly per segment (level 4)
#'
#' Aggregate data monthly per segment (level 4).
#'
#' @param data A dataframe with `datetime`, `stationname`, `value` and (Scheldt) `segment` columns. For example, the result
#' of [scheldemonitoR::assignGeometry()].
#' @param stat The statistics to aggregate data. Predifined option is "`mean`". See Details for the full list.
#'
#' @details Available statistics are:
#'    * "`mean`" calculates Arithmetic Mean, uses [base::mean()],
#'    * "`sd`" calculates Stadard Deviation, uses [stats::sd()]
#'    * "`se`" calculates Standard Error,
#'    * "`median`" calculates the Median Value. uses [stats::median()],
#'    * "`min`" calculates the minima, uses [base::min()],
#'    * "`max`" calculates the maxima, uses [base::max()],
#'    * "`5perc`" calculates the 5th percentile,
#'    * "`10perc`" calculates the 10th percentile,
#'    * "`90perc`" calculates the 90th percentile,
#'    * "`99perc`" calculates the 99th percentile,
#'    * "`sum`" calculates the sum, uses [base::sum()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # assign geometry
#' abiotic_data_geo <- assignGeometry(abiotic_data, "T2021", TRUE)
#'
#' # calculate monthly mean per segment
#' abiotic_data_mean <- aggregateMonthlyPerSegment(abiotic_data_geo, "mean")
aggregateMonthlyPerSegment <- function(data, stat = "mean"){
  # Assert arguments:
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("mean", "sd", "se", "median", "min", "max", "5perc", "10perc", "90perc", "99perc", "sum"))

  # calculate month and year
  data$year <- lubridate::year(data$datetime)
  data$month <- lubridate::month(data$datetime)

  # Determine if all required columns are present
  if(length(which(c("year", "month", "segment", "value") %in% names(data))) != 4){
    stop("Required input: (1) year, (2) month, (3) segment and (4) value")
  } else {
    message(paste0("Calculate ", stat, " per month and per segment"))
  }

  # Remove stations without segment
  message(paste0("---Removed ", sum(is.na(data$segment)),
                 " observations without segment---"))
  data <- data[!is.na(data$segment), ]

  # Split data conditional to zone, year and month
  lst.dat <- split(data, list(data$segment, data$year, data$month), drop = T)

  # Define output list
  lst.mnth <- list()

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("segment", "zone", "year", "month", "parametername", "unit")
    lst.mnth[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # Determine requested statistic and add to output
    lst.mnth[[i]]$value <- calculateStatistic(lst.dat[[i]], stat = stat)
  }

  # Merge individual statistics and return sorted overview
  df.mnth <- do.call("rbind", lst.mnth)
  message(paste0("---Monthly ", stat, " per segment calculated---"))
  return(df.mnth[order(df.mnth$segment, df.mnth$year, df.mnth$month), ])
}


#' Aggregate data monthly per salinity zone (level 3)
#'
#' Aggregate data monthly per salinity zone (level 3).
#'
#' @param data A dataframe with `datetime`, `stationname`, `value` and (salinity) `zone` columns. For example, the result
#' of [scheldemonitoR::assignGeometry()] or [scheldemonitoR::assignZones()].
#' @param stat The statistics to aggregate data. Predifined option is "`mean`". See Details for the full list
#'
#' @details Available statistics are:
#'    * "`mean`" calculates Arithmetic Mean, uses [base::mean()],
#'    * "`sd`" calculates Stadard Deviation, uses [stats::sd()]
#'    * "`se`" calculates Standard Error,
#'    * "`median`" calculates the Median Value. uses [stats::median()],
#'    * "`min`" calculates the minima, uses [base::min()],
#'    * "`max`" calculates the maxima, uses [base::max()],
#'    * "`5perc`" calculates the 5th percentile,
#'    * "`10perc`" calculates the 10th percentile,
#'    * "`90perc`" calculates the 90th percentile,
#'    * "`99perc`" calculates the 99th percentile,
#'    * "`sum`" calculates the sum, uses [base::sum()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # assign geometry
#' abiotic_data_geo <- assignGeometry(abiotic_data, "T2021", TRUE)
#'
#' # calculate monthly mean per zone
#' abiotic_data_mean <- aggregateMonthlyPerZone(abiotic_data_geo)
aggregateMonthlyPerZone <- function(data, stat = "mean"){
  # Assert arguments:
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("mean", "sd", "se", "median", "min", "max", "5perc", "10perc", "90perc", "99perc", "sum"))

  # calculate month and year
  data$year <- lubridate::year(data$datetime)
  data$month <- lubridate::month(data$datetime)

  # Determine if all required columns are present
  if(length(which(c("year", "month", "zone", "value") %in% names(data))) != 4){
    stop("Required input: (1) year, (2) month, (3) zone and (4) value")
  } else {
    message(paste0("Calculate ", stat, " per month and per salinity zone"))
  }

  # Remove stations without zone
  message(paste0("---Removed ", sum(is.na(data$zone)), " observations without zone---"))
  data <- data[!is.na(data$zone), ]

  # Split data conditional to zone, year and month
  lst.dat <- split(data, list(data$zone, data$year, data$month), drop = T)

  # Define output list
  lst.mnth <- list()

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("zone", "year", "month", "parametername", "unit")
    lst.mnth[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # Determine requested statistic and add to output
    lst.mnth[[i]]$value <- calculateStatistic(lst.dat[[i]], stat = stat)
  }

  # Merge individual statistics and return sorted overview
  df.mnth <- do.call("rbind", lst.mnth)
  message(paste0("---Monthly ", stat, " per salinity zone calculated---"))
  return(df.mnth[order(df.mnth$zone, df.mnth$year, df.mnth$month), ])
}

#' Aggregate data yearly per station
#'
#' Aggregate data yearly per station.
#'
#' @param data A dataframe with `datetime`, `stationname` and `value` columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()].
#' @param stat The statistics to aggregate data. Predifined option is "`mean`". See Details for the full list.
#'
#' @details Available statistics are:
#'    * "`mean`" calculates Arithmetic Mean, uses [base::mean()],
#'    * "`sd`" calculates Stadard Deviation, uses [stats::sd()]
#'    * "`se`" calculates Standard Error,
#'    * "`median`" calculates the Median Value. uses [stats::median()],
#'    * "`min`" calculates the minima, uses [base::min()],
#'    * "`max`" calculates the maxima, uses [base::max()],
#'    * "`5perc`" calculates the 5th percentile,
#'    * "`10perc`" calculates the 10th percentile,
#'    * "`90perc`" calculates the 90th percentile,
#'    * "`99perc`" calculates the 99th percentile,
#'    * "`sum`" calculates the sum, uses [base::sum()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # calculate yearly mean per station
#' abiotic_data_mean <- aggregateYearlyPerStation(abiotic_data, "mean")
aggregateYearlyPerStation <- function(data, stat = "mean"){
  # Assert arguments:
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("mean", "sd", "se", "median", "min", "max", "5perc", "10perc", "90perc", "99perc", "sum"))

  # check if 'year' column is present, if not calculate year
  if(!("year" %in% names(data))){
    data$year <- lubridate::year(data$datetime)
  }
  # Determine if all required columns are present
  if(length(which(c("year", "stationname", "value") %in% names(data))) != 3){
    stop("Required input: (1) year, (2) stationname and (3) value")
  } else {
    message(paste0("Calculate ", stat, " per year and per station"))
  }

  # Remove stations without stationname
  message(paste0("---Removed ", sum(is.na(data$stationname)), " observations without stationname---"))
  data <- data[!is.na(data$stationname), ]

  # Split data conditional to station and year
  lst.dat <- split(data, list(data$stationname, data$year), drop = T)

  # Define output list
  lst.yr <- list()

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("latitude", "longitude", "stationname", "parametername",
                 "unit", "segment", "zone", "year")
    lst.yr[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # Determine requested statistic and add to output
    lst.yr[[i]]$value <- calculateStatistic(lst.dat[[i]], stat = stat)
  }

  # Merge individual statistics and return sorted overview
  df.yr <- do.call("rbind", lst.yr)
  message(paste0("---Yearly ", stat, " per station calculated---"))
  return(df.yr[order(df.yr$stationname, df.yr$year), ])
}

#' Aggregate yearly per segment (level 4)
#'
#' Aggregate yearly per segment (level 4).
#'
#' @param data A dataframe with `datetime`, `stationname`, `value` and (Scheldt) `segment` columns. For example, the result
#' of [scheldemonitoR::assignGeometry()].
#' @param stat The statistics to aggregate data. Predifined option is "`mean`". See Details for the full list.
#'
#' @details Available statistics are:
#'    * "`mean`" calculates Arithmetic Mean, uses [base::mean()],
#'    * "`sd`" calculates Stadard Deviation, uses [stats::sd()]
#'    * "`se`" calculates Standard Error,
#'    * "`median`" calculates the Median Value. uses [stats::median()],
#'    * "`min`" calculates the minima, uses [base::min()],
#'    * "`max`" calculates the maxima, uses [base::max()],
#'    * "`5perc`" calculates the 5th percentile,
#'    * "`10perc`" calculates the 10th percentile,
#'    * "`90perc`" calculates the 90th percentile,
#'    * "`99perc`" calculates the 99th percentile,
#'    * "`sum`" calculates the sum, uses [base::sum()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # assign geometry
#' abiotic_data_geo <- assignGeometry(abiotic_data, "T2021", TRUE)
#'
#' # calculate monthly mean per segment
#' abiotic_data_mean <- aggregateYearlyPerSegment(abiotic_data_geo, "mean")
aggregateYearlyPerSegment <- function(data, stat = "mean"){
  # Assert arguments:
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("mean", "sd", "se", "median", "min", "max", "5perc", "10perc", "90perc", "99perc", "sum"))

  # check if 'year' column is present, if not calculate year
  if(!("year" %in% names(data))){
    data$year <- lubridate::year(data$datetime)
  }

  # Determine if all required columns are present
  if(length(which(c("year", "segment", "value") %in% names(data))) != 3){
    stop("Required input: (1) year, (2) segment and (3) value")
  } else {
    message(paste0("Calculate ", stat, " per year and per segment"))
  }

  # Remove stations without segment
  message(paste0("---Removed ", sum(is.na(data$segment)),
                 " observations without segment---"))
  data <- data[!is.na(data$segment), ]

  # Split data conditional to segment and year
  lst.dat <- split(data, list(data$segment, data$year), drop = T)

  # Define output list
  lst.yr <- list()

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("segment", "zone", "year", "parametername", "unit")
    lst.yr[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # Determine requested statistic and add to output
    lst.yr[[i]]$value <- calculateStatistic(lst.dat[[i]], stat = stat)
  }

  # Merge individual statistics and return sorted overview
  df.yr <- do.call("rbind", lst.yr)
  message(paste0("---Yearly ", stat, " per segment calculated---"))
  return(df.yr[order(df.yr$segment, df.yr$year), ])
}

#' Aggregate data yearly per salinity zone (level 3)
#'
#' Aggregate data yearly per salinity zone (level 3).
#'
#' @param data A dataframe with `datetime`, `stationname`, `value` and (salinity) `zone` columns. For example, the result
#' of [scheldemonitoR::assignGeometry()] or [scheldemonitoR::assignZones()].
#' @param stat The statistics to aggregate data. Predifined option is "`mean`". See Details for the full list.
#'
#' @details Available statistics are:
#'    * "`mean`" calculates Arithmetic Mean, uses [base::mean()],
#'    * "`sd`" calculates Stadard Deviation, uses [stats::sd()]
#'    * "`se`" calculates Standard Error,
#'    * "`median`" calculates the Median Value. uses [stats::median()],
#'    * "`min`" calculates the minima, uses [base::min()],
#'    * "`max`" calculates the maxima, uses [base::max()],
#'    * "`5perc`" calculates the 5th percentile,
#'    * "`10perc`" calculates the 10th percentile,
#'    * "`90perc`" calculates the 90th percentile,
#'    * "`99perc`" calculates the 99th percentile,
#'    * "`sum`" calculates the sum, uses [base::sum()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # assign geometry
#' abiotic_data_geo <- assignGeometry(abiotic_data, "T2021", TRUE)
#'
#' # calculate monthly mean per zone
#' abiotic_data_mean <- aggregateYearlyPerZone(abiotic_data_geo)
aggregateYearlyPerZone <- function(data, stat = "mean"){
  # Assert arguments:
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("mean", "sd", "se", "median", "min", "max", "5perc", "10perc", "90perc", "99perc", "sum"))

  # check if 'year' column is present, if not calculate year
  if(!("year" %in% names(data))){
    data$year <- lubridate::year(data$datetime)
  }

  # Determine if all required columns are present
  if(length(which(c("year", "zone", "value") %in% names(data))) != 3){
    stop("Required input: (1) year, (2) zone and (3) value")
  } else {
    message(paste0("Calculate ", stat, " per year and per zone"))
  }

  # Remove stations without zone
  message(paste0("---Removed ", sum(is.na(data$zone)),
                 " observations without zone---"))
  data <- data[!is.na(data$zone), ]

  # Split data conditional to zone and year
  lst.dat <- split(data, list(data$zone, data$year), drop = T)

  # Define output list
  lst.yr <- list()

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("zone", "year", "parametername", "unit")
    lst.yr[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # Determine requested statistic and add to output
    lst.yr[[i]]$value <- calculateStatistic(lst.dat[[i]], stat = stat)
  }

  # Merge individual statistics and return sorted overview
  df.yr <- do.call("rbind", lst.yr)
  message(paste0("---Yearly ", stat, " per salinity zone calculated---"))
  return(df.yr[order(df.yr$zone, df.yr$year), ])
}

#' Aggregate data for year range per station
#'
#' Aggregate data for year range (max year to min year) per station.
#'
#' @param data A dataframe with `datetime`, `stationname` and `value` columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()].
#' @param stat The statistics to aggregate data. Predifined option is "`mean`". See Details for the full list.
#' @param reduce Logical. If `TRUE` merges individual statistics and simplifies. Default is `FALSE`.
#'
#' @details Available statistics are:
#'    * "`mean`" calculates Arithmetic Mean, uses [base::mean()],
#'    * "`sd`" calculates Stadard Deviation, uses [stats::sd()]
#'    * "`se`" calculates Standard Error,
#'    * "`median`" calculates the Median Value. uses [stats::median()],
#'    * "`min`" calculates the minima, uses [base::min()],
#'    * "`max`" calculates the maxima, uses [base::max()],
#'    * "`5perc`" calculates the 5th percentile,
#'    * "`10perc`" calculates the 10th percentile,
#'    * "`90perc`" calculates the 90th percentile,
#'    * "`99perc`" calculates the 99th percentile,
#'    * "`sum`" calculates the sum, uses [base::sum()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # aggregate data for the whole period
#' abiotic_data_period_mean <- aggregatePeriodPerStation(abiotic_data, "mean", TRUE)
aggregatePeriodPerStation <- function(data, stat = "mean", reduce = FALSE){
  # Assert arguments:
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("mean", "sd", "se", "median", "min", "max", "5perc", "10perc", "90perc", "99perc", "sum"))
  checkmate::assert_choice(reduce, c(TRUE, FALSE))

  # calculate year
  data$year <- lubridate::year(data$datetime)

  # Determine if all required columns are present
  if(length(which(c("year", "stationname", "value") %in% names(data))) != 3){
    stop("Required input: (1) year, (2) stationname and (3) value")
  } else {
    message(paste0("Calculate ", stat, " per period and per station"))
  }

  # Remove stations without stationname
  message(paste0("---Removed ", sum(is.na(data$stationname)),
                 " observations without stationname---"))
  data <- data[!is.na(data$stationname), ]

  # Determine reporting period
  n.per <- paste0(min(data$year, na.rm = T), "-", max(data$year, na.rm = T))

  # Split data conditional to station
  lst.dat <- split(data, list(data$stationname), drop = T)

  # Define output list
  lst.prd <- list()

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("latitude", "longitude", "stationname", "parametername",
                 "segment", "zone")
    lst.prd[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # Add additional information (type and data availability period)
    lst.prd[[i]]$type <- "Station"
    lst.prd[[i]]$data <- n.per

    # Determine requested statistic and add unit
    lst.prd[[i]]$value <- calculateStatistic(lst.dat[[i]], stat = stat)
    lst.prd[[i]]$unit <- ifelse("unit" %in% names(lst.dat[[i]]),
                                lst.dat[[i]]$unit[1], NA)
  }

  # Merge individual statistics and simplify (if requested)
  df.prd <- do.call("rbind", lst.prd)
  df.prd <- df.prd[order(df.prd$stationname), ]
  if(reduce){
    df.prd <- df.prd[, c("stationname", "type", "data", "value", "unit")]
    names(df.prd)[1] <- "location"
  }

  # Return sorted overview
  message(paste0("---Periodical ", stat, " per station calculated---"))
  return(df.prd)
}


#' Aggregate data for year range per segment (level 4)
#'
#' Aggregate data for year range (max year to min year) per segment (level 4).
#'
#' @param data A dataframe with `datetime`, `stationname`, `value` and (Scheldt) `segment` columns. For example, the result
#' of [scheldemonitoR::assignGeometry()].
#' @param stat The statistics to aggregate data. Predifined option is "`mean`". See Details for the full list.
#' @param reduce Logical. If `TRUE` merges individual statistics and simplifies. Default is `FALSE`.
#'
#' @details Available statistics are:
#'    * "`mean`" calculates Arithmetic Mean, uses [base::mean()],
#'    * "`sd`" calculates Stadard Deviation, uses [stats::sd()]
#'    * "`se`" calculates Standard Error,
#'    * "`median`" calculates the Median Value. uses [stats::median()],
#'    * "`min`" calculates the minima, uses [base::min()],
#'    * "`max`" calculates the maxima, uses [base::max()],
#'    * "`5perc`" calculates the 5th percentile,
#'    * "`10perc`" calculates the 10th percentile,
#'    * "`90perc`" calculates the 90th percentile,
#'    * "`99perc`" calculates the 99th percentile,
#'    * "`sum`" calculates the sum, uses [base::sum()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # assign geometry
#' abiotic_data_geo <- assignGeometry(abiotic_data, "T2021", TRUE)
#'
#' # aggregate data for the whole period
#' abiotic_data_period_mean <- aggregatePeriodPerSegment(abiotic_data_geo, reduce = TRUE)
aggregatePeriodPerSegment <- function(data, stat = "mean", reduce = FALSE){
  # Assert arguments:
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("mean", "sd", "se", "median", "min", "max", "5perc", "10perc", "90perc", "99perc", "sum"))
  checkmate::assert_choice(reduce, c(TRUE, FALSE))

  # calculate year
  data$year <- lubridate::year(data$datetime)

  # Determine if all required columns are present
  if(length(which(c("year", "segment", "value") %in% names(data))) != 3){
    stop("Required input: (1) year, (2) segment and (3) value")
  } else {
    message(paste0("Calculate ", stat, " per period and per segment"))
  }

  # Remove stations without segment
  message(paste0("---Removed ", sum(is.na(data$segment)),
                 " observations without segment---"))
  data <- data[!is.na(data$segment), ]

  # Determine reporting period
  n.per <- paste0(min(data$year, na.rm = T), "-", max(data$year, na.rm = T))

  # Split data conditional to segment
  lst.dat <- split(data, list(data$segment), drop = T)

  # Define output list
  lst.prd <- list()

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("parametername", "segment", "zone")
    lst.prd[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # Add additional information (type and data availability period)
    lst.prd[[i]]$type <- "Segment"
    lst.prd[[i]]$data <- n.per

    # Determine requested statistic and add unit
    lst.prd[[i]]$value <- calculateStatistic(lst.dat[[i]], stat = stat)
    lst.prd[[i]]$unit <- ifelse("unit" %in% names(lst.dat[[i]]),
                                lst.dat[[i]]$unit[1], NA)
  }

  # Merge individual statistics and simplify (if requested)
  df.prd <- do.call("rbind", lst.prd)
  df.prd <- df.prd[order(df.prd$segment), ]
  if(reduce){
    df.prd <- df.prd[, c("segment", "type", "data", "value", "unit")]
    names(df.prd)[1] <- "location"
  }

  # Return sorted overview
  message(paste0("---Periodical ", stat, " per segment calculated---"))
  return(df.prd)
}

#' Aggregate data for year range per zone (level 3)
#'
#' Aggregate data for year range (max year to min year) per zone (level 4).
#'
#' @param data A dataframe with `datetime`, `stationname`, `value` and (salinity) `zone` columns. For example, the result
#' of [scheldemonitoR::assignGeometry()] or [scheldemonitoR::assignZones()].
#' @param stat The statistics to aggregate data. Predifined option is "`mean`". See Details for the full list
#' @param reduce Logical. If `TRUE` merges individual statistics and simplifies. Default is `FALSE`.
#'
#' @details Available statistics are:
#'    * "`mean`" calculates Arithmetic Mean, uses [base::mean()],
#'    * "`sd`" calculates Stadard Deviation, uses [stats::sd()]
#'    * "`se`" calculates Standard Error,
#'    * "`median`" calculates the Median Value. uses [stats::median()],
#'    * "`min`" calculates the minima, uses [base::min()],
#'    * "`max`" calculates the maxima, uses [base::max()],
#'    * "`5perc`" calculates the 5th percentile,
#'    * "`10perc`" calculates the 10th percentile,
#'    * "`90perc`" calculates the 90th percentile,
#'    * "`99perc`" calculates the 99th percentile,
#'    * "`sum`" calculates the sum, uses [base::sum()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2012, end = 2015)
#'
#' # assign geometry
#' abiotic_data_geo <- assignGeometry(abiotic_data, "T2021", TRUE)
#'
#' # aggregate data for the whole period
#' abiotic_data_period_mean <- aggregatePeriodPerZone(abiotic_data_geo, reduce = TRUE)
aggregatePeriodPerZone <- function(data, stat = "mean", reduce = FALSE){
  # Assert arguments:
  checkmate::assertDataFrame(data)
  checkmate::assert_choice(stat, c("mean", "sd", "se", "median", "min", "max", "5perc", "10perc", "90perc", "99perc", "sum"))
  checkmate::assert_choice(reduce, c(TRUE, FALSE))

  # calculate year
  data$year <- lubridate::year(data$datetime)

  # Determine if all required columns are present
  if(length(which(c("year", "zone", "value") %in% names(data))) != 3){
    stop("Required input: (1) year, (2) zone and (3) value")
  } else {
    message(paste0("Calculate ", stat, " per period and per salinity zone"))
  }

  # Remove stations without zone
  message(paste0("---Removed ", sum(is.na(data$zone)),
                 " observations without zone---"))
  data <- data[!is.na(data$zone), ]

  # Determine reporting period
  n.per <- paste0(min(data$year, na.rm = T), "-", max(data$year, na.rm = T))

  # Split data conditional to zone
  lst.dat <- split(data, list(data$zone), drop = T)

  # Define output list
  lst.prd <- list()

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Add general information to output
    v.names <- c("parametername", "zone")
    lst.prd[[i]] <- lst.dat[[i]][1, names(lst.dat[[i]]) %in% v.names]

    # Add additional information (type and data availability period)
    lst.prd[[i]]$type <- "Zone"
    lst.prd[[i]]$data <- n.per

    # Determine requested statistic and add unit
    lst.prd[[i]]$value <- calculateStatistic(lst.dat[[i]], stat = stat)
    lst.prd[[i]]$unit <- ifelse("unit" %in% names(lst.dat[[i]]),
                                lst.dat[[i]]$unit[1], NA)
  }

  # Merge individual statistics and simplify (if requested)
  df.prd <- do.call("rbind", lst.prd)
  df.prd <- df.prd[order(df.prd$zone), ]
  if(reduce){
    df.prd <- df.prd[, c("zone", "type", "data", "value", "unit")]
    names(df.prd)[1] <- "location"
  }

  # Return sorted overview
  message(paste0("---Periodical ", stat, " per salinity zone calculated---"))
  return(df.prd)
}

# helper functions ---------------------------------------------------------------------------------
calculateStatistic <- function(data, stat = "mean"){
  # Determine if requested statistic is included
  if(!tolower(stat) %in% c("mean", "sd", "se", "median", "min", "max", "5perc",
                           "10perc", "90perc", "99perc","sum")){
    stop(paste0("Options for statistic are: (1) mean, (2) sd, (3) se, ",
                "(4) median, (5) min, (6) max, (7) 5perc, (8) 10perc, ",
                "(9) 90perc, (10) 99perc and (11) sum"), call. = F)
  }

  # Determine requested statistic
  if(tolower(stat) == "mean"){
    n.stat <- mean(data$value, na.rm = T)
  } else if(tolower(stat) == "sd"){
    n.stat <- stats::sd(data$value, na.rm = T)
  } else if(tolower(stat) == "se"){
    n.stat <- stats::sd(data$value, na.rm = T) / sqrt(nrow(data[!is.na(data$value)]))
  } else if(tolower(stat) == "median"){
    n.stat <- stats::median(data$value, na.rm = T)
  } else if(tolower(stat) == "min"){
    n.stat <- min(data$value, na.rm = T)
  } else if(tolower(stat) == "max"){
    n.stat <- max(data$value, na.rm = T)
  } else if(tolower(stat) == "5perc"){
    n.stat <- stats::quantile(data$value, probs = 0.05, na.rm = T)
  } else if(tolower(stat) == "10perc"){
    n.stat <- stats::quantile(data$value, probs = 0.10, na.rm = T)
  } else if(tolower(stat) == "90perc"){
    n.stat <- stats::quantile(data$value, probs = 0.90, na.rm = T)
  } else if(tolower(stat) == "99perc"){
    n.stat <- stats::quantile(data$value, probs = 0.99, na.rm = T)
  } else if(tolower(stat) == "sum"){
    n.stat <- sum(data$value, na.rm = T)
  }
  # Return calculated statistic
  return(n.stat)
}
