# TODO: is there a better way to bind these global variables? They come from the ggplot2::aes() call.
utils::globalVariables(c("datetime", "value", "year"))
#' Select data for a time period
#'
#' Select data based on input start year and end year.
#'
#' @param data A dataframe with a `datetime` column. For example, the result of [scheldemonitoR::importAbioticData()]
#' or [scheldemonitoR::importBioticData()].
#' @param start Numeric. Start year.
#' @param end Numeric. End year.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2018, end = 2021)
#'
#' # Get the data for a specific time period:
#' abiotic_data_selected_years <- selectTimePeriod(abiotic_data, 2019, 2019)
selectTimePeriod <- function(data, start, end) {
  # Assert arguments
  checkmate::assert_data_frame(data)

  # Determine if all required columns are present
  if(length(which(c("datetime") %in% names(data))) != 1){
    stop("Required input: (1) datetime")
  } else {
    message(paste0("Select data between ", start, " and ", end))
  }

  # Register column names
  v.names <- names(data)

  # Adapt format datetime and extract information from years
  data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%dT%H:%M:%S")
  data$year <- format(data$datetime, format = "%Y")

  # Select subset of the data based on years
  df.new <- data[which(data$year %in% c(start:end)), v.names]

  # Return results
  message(paste0("---", nrow(data) - nrow(df.new), " rows were removed---"))
  return(df.new)
}

# Define function to exclude extreme values
#' Exclude extreme values
#'
#' Exclude extreme values.
#'
#' @param data  dataframe with `datetime` and `value` columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()].
#' @param method Available options "range", "min", "max", "pct" (perentile), "iqr" (Interquartile range).
#' See details. Default is `iqr`.
#' @param limit Numeric. Values higher than this number are considered extremes.
#'
#' @details `method = range` allows to input a value range for `limit`. For example `limit = c(1, 10)`.
#'
#' @return A list of dataframes. The first one is the dataframe with extremes removed; the second is the
#' extreme values found.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2018, end = 2019)
#'
#' # exclude values lower than 0:
#' data_extreme <- excludeExtremeValues(abiotic_data, method = "min", limit = 0)
#'
#' # exclude values by range: lower than 1 and higher than 400
#' data_extreme <- excludeExtremeValues(abiotic_data,
#'                                      method = "range", limit = c(1, 400))
excludeExtremeValues <- function(data, method = "iqr", limit = 3) {
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertChoice(method, c("range", "min", "max", "pct", "iqr"))
  # Determine if requested statistic is included
  if(!tolower(method) %in% c("range", "min", "max", "pct", "iqr")){
    stop(paste0("Options for threshold are: (1) range, (2) min, (3) max, ",
                "(4) pct and (5) iqr"), call. = F)
  }

  # Determine if all required columns are present
  if(length(which(c("datetime", "value") %in% names(data))) != 2){
    stop("Required input: (1) datetime and (2) value", call. = F)
  } else {
    message(paste0("Exclude extreme data values (method = ", method, ")"))
  }

  # Adapt format datetime and extract information from years
  data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%dT%H:%M:%S")
  data$year <- format(data$datetime, format = "%Y")

  # Split data according to requested level
  # if(spatial == "station"){
  #   message("---Identify extreme values at station-level (yearly)---")
  #   lst.dat <- split(data, list(data$stationname, data$year), drop = T)
  # } else {
  #   message("---Options are (1) station. Continue with station---")
  #   lst.dat <- split(data, list(data$stationname, data$year), drop = T)
  # }
  lst.dat <- split(data, list(data$stationname, data$year), drop = T)

  # Define output lists
  lst.new <- lst.extr <- list()

  # Provide feedback to user
  if(tolower(method) == "range" && length(limit) != 2){
    message("---No correct range provided, continue with full range---")
  } else if(tolower(method) == "min" && length(limit) != 1){
    message("---No correct limit provided, continue with observed minimum---")
  } else if(tolower(method) == "max" && length(limit) != 1){
    message("---No correct limit provided, continue with observed maximum---")
  } else if(tolower(method) == "pct" && (length(limit) != 1 || abs(limit) > 1)){
    message("---No correct percentile (0-1) provided, continue with P99---")
  } else if(tolower(method) == "iqr" && length(limit) != 1){
    message("---No limit provided, continue with standard of 3*IQR---")
  } else {
    message("---Excluding extreme values conditional to specified limit---")
  }

  # For each subset, calculate statistic
  for (i in c(1:length(lst.dat))){
    # Initialise data
    df.dat <- lst.dat[[i]]

    # Calculate statistic
    if(tolower(method) == "range"){
      if(length(limit) != 2){
        limit <- range(df.dat$value, na.rm = T)
      }
      v.extr <- which(df.dat$value < limit[1] | df.dat$value > limit[2])
    } else if(tolower(method) == "min"){
      if(length(limit) != 1){
        limit <- min(df.dat$value, na.rm = T)
      }
      v.extr <- which(df.dat$value < limit)
    } else if(tolower(method) == "max"){
      if(length(limit) != 1){
        limit <- max(df.dat$value, na.rm = T)
      }
      v.extr <- which(df.dat$value > limit)
    } else if(tolower(method) == "pct"){
      if(length(limit) != 1 || abs(limit) > 1){
        n.pct <- stats::quantile(df.dat$value, prob = 0.99, na.rm = T)
      } else {
        n.pct <- stats::quantile(df.dat$value, prob = abs(limit), na.rm = T)
      }
      v.extr <- which(df.dat$value > n.pct)
    } else if(tolower(method) == "iqr"){
      if(length(limit) != 1){
        limit <- 3
      }
      n.iqr <- stats::IQR(df.dat$value, na.rm = T)
      v.iqr <- c(stats::quantile(df.dat$value, prob = 0.25, na.rm = T) - limit * n.iqr,
                 stats::quantile(df.dat$value, prob = 0.75, na.rm = T) + limit * n.iqr)
      v.extr <- which(df.dat$value < v.iqr[1] | df.dat$value > v.iqr[2])
    }

    # Save extreme values and return reduced data
    if(length(v.extr) > 0){
      lst.extr[[length(lst.extr) + 1]] <- df.dat[v.extr, ]
      lst.new[[i]] <- df.dat[-v.extr, ]
    } else {
      lst.new[[i]] <- df.dat
    }
  }

  # Bring lists together in data frames
  df.extr <- do.call("rbind", lst.extr)
  df.new <- do.call("rbind", lst.new)

  # Return data
  if(is.null(df.extr)){
    message("---No extreme observations identified---")
  } else {
    message(paste0("---", nrow(df.extr), " extreme observations identified and removed---"))
  }
  return(list(df.new, df.extr))
}

# Define function to update stationnames
#' Update stationnames
#'
#' Update old stationnames to new stationnames. For example `211900` to `Dijle Zenne (211900)`.
#'
#' @param data A dataframe with 'stationname' column. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()]
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2018, end = 2020)
#'
#' # rename stations
#' abiotic_data_updated <- updateStationname(abiotic_data)
updateStationname <- function(data) {
  # Assert arguments
  checkmate::assert_data_frame(data)

  # Determine if all required columns are present
  if(length(which(c("stationname") %in% names(data))) != 1){
    stop("Required input: (1) stationname column")
  } else {
    message(paste0("Upgrade station names"))
  }

  # Load alternative names for stations
  # df.nms <- read.csv(paste0("~/T2021/00_Algemeen/Data/Meetstations/",
  #                           "MeetstationsAlternatieveNaamgeving.csv"),
  #                    header = T, stringsAsFactors = F) # csv was replaced by a internal data dataframe
  df.nms <- AlternativeStationNaming

  # Adapt stationnames to more informative names
  v.stations <- unique(data$stationname); n.count <- 0
  for (i in c(1:length(v.stations))){
    if(v.stations[i] %in% df.nms$db_name){
      n.count <- n.count + 1
      data$stationname[data$stationname == v.stations[i]] <-
        df.nms$new_name[df.nms$db_name == v.stations[i]]
    }
  }

  # Return new data
  message(paste0("---", n.count, " stations were renamed---"))
  return(data)
}

#' Add zeroes for non-observed species in ecological data
#'
#' Add zeroes for non-observed species in ecological data. `addZeroes()` sets a unique list of scientific names and add
#' zeroes for each of the species not found in a station. In this way, the absence of a specie in a station can be taken
#' into account.
#'
#' @param data A dataframe with 'stationname', 'scientificname' and 'observationdate' columns. For example, the result
#' of [scheldemonitoR::importBioticData()].
#' @param level Character. Either `all` or `station`.
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' # make a list of the dataset ids
#' imis_datasets <- c(1073, 5191)
#'
#' # download data
#' biotic_data_imis <- importBioticData(imis_datasets, 2015, 2021, 'imis')
#'
#' # add zeroes
#' biotic_data_zeroes <- addZeroes(biotic_data_imis, level = "station")
addZeroes <- function(data, level = "all") {
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertChoice(level, c("all", "station"))
  # Check if required columns are available
  if(length(which(c("stationname", "scientificname", "observationdate") %in%
                  names(data))) != 3){
    stop("Required input: (1) stationname, (2) scientificname and (3) observationdate")
  } else if(!(tolower(level) %in% c("all", "station"))){
    stop("Options for 'level' are (1) all and (2) station")
  } else {
    message("Add zeroes for non-observed species")
  }

  # Identify indices for column of the species name
  v.idx <- which(names(data) %in% c("scientificname", "value"))

  # Specify the species to be considered if "level = all"
  if(tolower(level) == "all"){ v.species <- unique(data$scientificname) }

  # Split the data according to station
  lst.stat <- split(data, f = list(data$stationname), drop = T)

  # Loop over all stations, add zeroes and store in a temporary list
  lst.zero <- list()
  for (d in c(1:length(lst.stat))){
    # Initialise data
    df.stat <- lst.stat[[d]]

    # Specify the species to be considered if "level = station"
    if(tolower(level) == "station"){ v.species <- unique(df.stat$scientificname) }

    # Split the data according to observationdate
    lst.date <- split(df.stat, f = list(df.stat$observationdate), drop = T)

    # Loop over all dates, add zeroes and store in a temporary list
    for (e in c(1:length(lst.date))){
      # Initialise data
      df.date <- lst.date[[e]]

      # Identify species that are not included in sample
      v.sel <- v.species[!(v.species %in% unique(df.date$scientificname))]

      # Create dataframe with zeroes as observations and save in list
      if(length(v.sel) > 0){
        lst.zero[[length(lst.zero) + 1]] <- cbind(
          df.date[1, -v.idx], "value" = 0, "scientificname" = v.sel,
          row.names = NULL)
      } else {
        lst.zero[[length(lst.zero) + 1]] <- NULL
      }
    }
  }

  # Unpack the created list into a dataframe
  df.zero <- do.call("rbind", lst.zero)

  # Bring both observations and zeroes together
  df.all <- rbind(data, df.zero)

  # Return extended dataset
  message(paste0("---", nrow(df.zero), " zeroes added---"))
  return(df.all)
}

#' Make station-specific temporal plot
#'
#' Make station-specific temporal plot.
#'
#' @param data A dataframe with 'datetime', 'value' and 'stationname' columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()].
#' @param varname Character. Optional. Variable name to include in the title of the plot. Placehorder is `Variable`.
#' @param station Character or list. Station or list of stations to plot. If `NULL` the first station that appears
#' in the dataframe will be selected.  is `NULL`.
#' @param plottype Character. Options are `line` or `boxplot`. Dafualt is `line`.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' # make a list of the dataset ids
#' imis_datasets <- c(1073, 5191)
#'
#' # download data
#' biotic_data_imis <- importBioticData(imis_datasets, 2015, 2021, 'imis')
#'
#' # correct datetime colum name
#' colnames(biotic_data_imis)[colnames(biotic_data_imis) == "observationdate"] <- "datetime"
#'
#' # make temporal plot
#' plot <- exploreDataStation(biotic_data_imis, "Densiteit van zooplankton (ind/m3)", "line")
#' plot
exploreDataStation <- function(data, varname = "Variable", station = NULL, plottype = "line") {
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertChoice(plottype, c("line", "boxplot"))
  # Determine if the required column "datetime" is present
  if(length(which(c("datetime", "value", "stationname") %in% names(data))) != 3){
    stop("Required input: (1) datetime, (2) value and (3) stationname")
  } else {
    message("Exploring available data for specific station")
  }

  # Provide message to specify variable name (if applicable)
  if(varname == "Variable"){
    message("---Recommended to specify variable name---")
  }

  # Check if station is in list of stationname (else, choose first station)
  if(!is.null(station) && station %in% data$stationname){
    message(paste0("---Explore data for station '", station, "'---"))
  } else {
    station <- data$stationname[1]
    message(paste0("---'Station' is an optional parameter, exploring for '",
                   station, "'---"))
  }

  # Select data for further processing
  df.stat <- data[data$stationname == station, ]
  #df.stat$datetime <- as.Date(format(df.stat$datetime, "%Y-%m-%d"))
  df.stat$datetime <- as.Date(df.stat$datetime, format = "%Y-%m-%d")


  # Depending on plot type, the approach differs
  if(plottype == "line"){
    p.stationPlot <- ggplot2::ggplot(df.stat, ggplot2::aes(x = datetime, y = value)) +
      ggplot2::geom_point(colour = "#1E64C8") +
      ggplot2::geom_line(colour = "#1E64C8", linetype = "dashed") +
      ggplot2::ggtitle(paste0("Verloop ", varname, " in station '", station, "'")) +
      ggplot2::scale_x_date("") +
      ggplot2::scale_y_continuous(paste0(varname, " [", df.stat$unit[1], "]")) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(colour = "black"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  } else if(plottype == "boxplot"){
    df.stat$year <- format(df.stat$datetime, "%Y")
    p.stationPlot <- ggplot2::ggplot(df.stat, ggplot2::aes(x = year, y = value)) +
      ggplot2::geom_boxplot(colour = "#1E64C8") +
      ggplot2::geom_point(colour = "#1E64C8", shape = 1, alpha = 0.50) +
      ggplot2::ggtitle(paste0("Verloop ", varname, " in station '", station, "'")) +
      ggplot2::scale_x_discrete("") +
      ggplot2::scale_y_continuous(paste0(varname, " [", df.stat$unit[1], "]")) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(colour = "black"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  } else {
    message(paste0("---No known plottype '", plottype,
                   "', options are (1) line and (2) boxplot---"))
    p.stationPlot <- NULL
  }

  # Return plot
  return(p.stationPlot)
}
