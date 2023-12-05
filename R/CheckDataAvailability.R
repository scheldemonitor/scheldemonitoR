#TODO: find a better way to solve the noves on global variables
utils::globalVariables(c("frequency", "freq", "plot"))
#' Summarize Available data by year
#'
#' Summarize Available data by year
#'
#' @param data A dataframe with `datetime`, `parametername`, and `standardparameterid` columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()].
#' @param start Begin year.
#' @param end End year.
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
#' available_data <- summarizeAvailableDatabyYear(abiotic_data, 2020, 2021)
summarizeAvailableDatabyYear <- function(data, start, end) {
  # Assert arguments
  checkmate::assert_data_frame(data)

  # Determine if all required columns are present
  if(length(which(c("datetime","parametername","standardparameterid") %in% names(data))) != 3){
    stop("Required input: data should have a 'datetime','parametername', 'standardparameterid' column")
  } else {
    message("Assigning stations to segments/zones/regions")
  }

  # Add a year column
  data$year = lubridate::year(data$datetime)

  # Output data frame
  df_out <- data.frame(ParameterName = character(),
                       ParameterID = character(),
                       Year = integer(),
                       StationName = character(),
                       Count = integer(),
                       stringsAsFactors = FALSE)
  #str(df_out)
  rownum <- 0

  if ((dim(data)[1] > 0) || (dim(data)[2] > 0)) {

    for (yy in start:end){

      yy_data <- data[data$year %in% yy,]
      if ((dim(yy_data)[1] > 0) || (dim(yy_data)[2] > 0)) {

        # Find unique station names in data set
        stations <- unique(data[c("stationname")])
        nstations <- dim(stations)[1]

        if(nstations>0) {
          for (j in 1:nstations) {
            # For each station do
            station <- stations[j, c("stationname")]

            data_station <- data.frame()
            data_station <- yy_data[yy_data$stationname %in% station, ]
            parameterNAMES <- as.character(unique(data_station$parametername))

            for (parameter in parameterNAMES) {

              #For a given parameter do:
              rownum <- rownum + 1
              data_station_par <- data_station[as.character(data_station$parametername) %in% parameter, ]
              parid <- as.character(unique(data_station_par$standardparameterid))
              df_out[rownum,] <- c(parameter, parid, yy, as.character(station), dim(data_station_par)[1])
            }
          }
        } else {
          # No data
          rownum <- rownum + 1
          df_out[rownum, ] <- c(NA, NA, yy, NA, 0)
        }
      }
    }
  }
  # Generate output
  return(df_out)
}

# Define graphical depiction of data availability (provider and station)
#' Define a graphical depiction of data availability (provider ans station)
#'
#' Define a graphical depiction of data availability (provider ans station)
#'
#' @param data A dataframe with `datetime`, `dataprovider`, and `stationname` columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()].
#' @param param Character. See Details. Name of the parameter or list of parameters present in `data`, to add
#' to plot labels. Defaul is `NULL`.
#' @param resolution Either "year" or "month". "y" and "m" also accepted. Default is "year".
#'
#' @details `depictDataAvailability()` does not check for unique parameternames. Recommendation is that `data` includes information for
#' just one parameter.
#'
#' @return A list of 2 plots. See Examples.
#'     * First element plots available information per dataprovider.
#'     * Second element plots available information per station.
#' @export
#'
#' @examples
#' # 9694 is the parameter id for high tide in NAP
#' tide_data <- importAbioticData(9694, start = 2018, end = 2021)
#'
#' # plot available data
#' tide_availability_figures <- depictDataAvailability(tide_data, "high tide", "y")
#'
#' # plot both plot elements
#' tide_availability_figures
#'
#' # plot only available data per provider
#' tide_availability_figures[[1]]
#'
#' # plot only available data per station
#' tide_availability_figures[[2]]
depictDataAvailability <- function(data, param = NULL, resolution = "year"){
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertCharacter(param)
  checkmate::assertChoice(resolution, c("year", "month", "y", "m"))

  # Check if required columns are available
  if(length(which(c("datetime", "dataprovider", "stationname") %in% names(data))) != 3){
    stop("Required input: (1) datetime, (2) dataprovider and (3) stationname")
  } else {
    message("Depict data availability per provider and station")
  }

  # Prepare data
  df.dat <- data
  df.dat$date <- as.Date(df.dat$datetime, format = "%Y-%m-%d")

  # Define breaks and labels for graph
  n.dateMin <- as.Date(format(min(df.dat$date), "%Y-01-01"))
  n.dateMax <- as.Date(format(max(df.dat$date), "%Y-01-01"))
  v.breaks <- seq(n.dateMin, n.dateMax, by = "year")

  # Reduce breaks and labels if many dates are considered
  if (length(v.breaks) > 11) {
    v.breaks <- v.breaks[seq(1, length(v.breaks), by = 2)]
  }
  v.labels <- v.breaks

  # Adapt temporal aggregation, depending on input
  if (resolution == "month" || resolution == "m") {
    df.dat$date <- as.Date(format(df.dat$date, format = "%Y-%m-01"))
    v.labels <- format(v.breaks, format = "%Y-%m")
  }
  if (resolution == "year" || resolution == "y") {
    df.dat$date <- as.Date(format(df.dat$date, format = "%Y-01-01"))
    v.labels <- format(v.breaks, format = "%Y")
  }

  # Develop graphs for providers
  v.prov <- ordered(unique(df.dat$dataprovider))
  lst.dataProv <- list()
  for (i in c(1:ceiling(length(v.prov) / 14))) {
    # Determine subset of providers and data
    v.prov.sub <- v.prov[(1 + 14 * (i - 1)):(min(length(v.prov), 14 * i))]
    df.sub <- df.dat[df.dat$dataprovider %in% v.prov.sub, ]

    # Create graphics for subset
    lst.dataProv[[i]] <- ggplot2::ggplot(df.sub, ggplot2::aes(x = date)) +
      ggplot2::geom_bar() +
      ggplot2::scale_x_date("", breaks = v.breaks, labels = v.labels) +
      ggplot2::scale_y_continuous(paste0(
        ifelse(is.null(param), "", paste0(param, " - ")), "Number of observations")) +
      ggplot2::facet_wrap( ~ dataprovider, ncol = 2, scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(colour = "black"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(colour = "black", face = "bold")
      )
  }

  # Develop graphs for stations
  v.stat <- ordered(unique(df.dat$stationname))
  lst.dataStat <- list()
  for (i in c(1:ceiling(length(v.stat) / 16))) {
    # Determine subset of stations and data
    v.stat.sub <- v.stat[(1 + 16 * (i - 1)):(min(length(v.stat), 16 * i))]
    df.sub <- df.dat[df.dat$stationname %in% v.stat.sub, ]

    # Create graphics
    lst.dataStat[[i]] <- ggplot2::ggplot(df.sub, ggplot2::aes(x = date)) +
      ggplot2::geom_bar() +
      ggplot2::scale_x_date("", breaks = v.breaks, labels = v.labels) +
      ggplot2::scale_y_continuous(paste0(
        ifelse(is.null(param), "", paste0(param, " - ")), "Number of observations")) +
      ggplot2::facet_wrap( ~ stationname, ncol = 2, scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(colour = "black"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(colour = "black", face = "bold")
      )
  }

  # Return output
  message(paste0("---Providers: ", length(lst.dataProv)," graph(s) - ",
                 "Stations: ", length(lst.dataStat), " graph(s)"))
  return(c(lst.dataProv, lst.dataStat))
}

#' Describe data availability through heatmap
#'
#' Describe data availability through heatmap, with option to choose to use dataprovider or stations.
#'
#' @param data A dataframe with `datetime`, `dataprovider`, and `stationname` columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()].
#' @param param Character. See Details. Name of the parameter or list of parameters present in `data`, to add
#' to plot labels. Defaul is `NULL`.
#' @param resolution Either "year" or "month". "y" and "m" also accepted. Default is "year".
#' @param type Either "station" or "dataprovider". Default is "station".
#'
#' @details `heatmapDataAvailability()` does not check for unique parameternames. Recommendation is that `data` includes information for
#' just one parameter.
#'
#' @return A heatmap.
#' @export
#'
#' @examples
#' # 9694 is the parameter id for high tide in NAP
#' tide_data <- importAbioticData(9694, start = 2018, end = 2021)
#'
#' # plot available data
#' tide_availability_figures <- heatmapDataAvailability(tide_data, "high tide", "year", "station")
#'
#' # Example with abiotic data ---------------------------------------------------------
#' # abiotic data #949 & 1074
#' fytoplankton_data <- importBioticData(1074, start = 2013, end = 2015, source = "imis")
#'
#' # correct datetime columname
#' colnames(fytoplankton_data)[colnames(fytoplankton_data) == "observationdate"] <- "datetime"
#'
#' # plot available data
#' heatmap_fyto <- heatmapDataAvailability(fytoplankton_data, "high tide", "year", "station")
heatmapDataAvailability <- function(data, param = "parameter", resolution = "year", type = "station") {
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertCharacter(param)
  checkmate::assertChoice(resolution, c("year", "month", "y", "m"))
  checkmate::assertChoice(type, c("station", "dataprovider"))

  # Check if required columns are available
  if(length(which(c("datetime", "dataprovider", "stationname") %in% names(data))) != 3){
    stop("Required input: (1) datetime, (2) dataprovider and (3) stationname")
  } else {
    message("Depict data availability as heatmap")
  }

  # Prepare data
  df.dat <- data
  df.dat$date <- as.Date(df.dat$datetime, format = "%Y-%m-%d")

  # Define breaks and labels for graph
  n.dateMin <- as.Date(format(min(df.dat$date), "%Y-01-01"))
  n.dateMax <- as.Date(format(max(df.dat$date), "%Y-01-01"))
  v.breaks <- seq(n.dateMin, n.dateMax, by = "year")

  # Reduce breaks and labels if many dates are considered
  if (length(v.breaks) > 11) {
    v.breaks <- v.breaks[seq(1, length(v.breaks), by = 2)]
  }
  v.labels <- v.breaks

  # Adapt temporal aggregation, depending on input
  if (resolution == "month" || resolution == "m") {
    df.dat$date <- as.Date(format(df.dat$date, format = "%Y-%m-01"))
    v.labels <- format(v.breaks, format = "%Y-%m")
  }
  if (resolution == "year" || resolution == "y") {
    df.dat$date <- as.Date(format(df.dat$date, format = "%Y-01-01"))
    v.labels <- format(v.breaks, format = "%Y")
  }

  # Generate table with frequencies
  if (type == "station"){
    df.tab <- table(df.dat[, c("stationname", "date")])
  } else if (type == "dataprovider"){
    df.tab <- table(df.dat[, c("dataprovider", "date")])
  } else {
    message(paste0("---Options for type are (1) station and (2) dataprovider. ",
                   "Continue with station---"))
    df.tab <- table(df.dat[, c("stationname", "date")])
  }

  # Prepare plot data
  df.plot <- as.data.frame(df.tab, stringsAsFactors = F)
  names(df.plot) <- c("type", "date", "frequency")
  df.plot$date <- as.Date(df.plot$date, format = "%Y-%m-%d")
  df.plot$type <- factor(df.plot$type, rev(ordered(unique(df.plot$type))))

  # Generate heatmap based on frequency table
  p.heatmap <- ggplot2::ggplot(df.plot, ggplot2::aes(x = date, y = type)) +
    ggplot2::geom_tile(ggplot2::aes(fill = frequency), color = "white", linewidth = 1, linetype = 1) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(frequency > 0, NA_character_, "x")),
              color = "red", size = 2, na.rm = T) +
    ggplot2::scale_x_date("", breaks = v.breaks, labels = v.labels) +
    ggplot2::scale_y_discrete("") +
    ggplot2::scale_fill_gradient(low = "white", high = "#1E64C8") +
    ggplot2::ggtitle(paste0("Available data for ", param)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(colour = "black"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          legend.title = ggplot2::element_blank()
    )

  # Return plot
  message(paste0("---Heatmap generated for ", param, "---"))
  return(p.heatmap)
}

#' Check presence of duplicate datetimes for specific parameter or station(s)
#'
#' Check presence of duplicate datetimes for specific parameter or station(s)
#'
#' @param data A dataframe with `datetime`, `dataprovider`, and `stationname` columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()].
#' @param station Character or list. Station name or list of stationnames to check. Stations must be present in `data`.
#' If empty all distinct stationnames will be checked for duplicate dates. Default is `NULL`
#' @param visual Logical. If 'TRUE' a visual output will be generated. Default is `TRUE`.
#' @param pause Logical. If 'TRUE' function will ask whether to continue with the next station (when `station` > 1).
#' Default is `FALSE`.
#' @param dup_out Logical. If 'TRUE' returns a dataframe with potential duplicates. Default is `TRUE`.
#'
#' @return A dataframe and, if requested (`visual`) a plot
#' @export
#'
#' @examples
#' # 9694 is the parameter id for high tide in NAP
#' biotic_data_aphia <- importBioticData(129938, 1994, 2008, "aphia")
#'
#' # rename datetime column
#' colnames(biotic_data_aphia)[colnames(biotic_data_aphia) == "observationdate"] <- "datetime"
#'
#' # stations to check
#' stations <- c("WS Centr./West.deel WESTSDPLOT4_1212","WS Centr./West.deel WESTSDPLOT4_1213",
#'               "WS Centr./West.deel WESTSDPLOT4_1215", "WS Centr./West.deel WESTSDPLOT4_1221",
#'               "WS Centraal deel WESTSDMDN_0411", "WS Centraal deel WESTSDMDN_0433")
#'
#' # get potential duplicates
#' duplicates <- checkDuplicateDate(biotic_data_aphia, station = stations,
#'                                  visual = TRUE, pause = TRUE, dup_out = TRUE)
checkDuplicateDate <- function(data, station = NULL, visual = TRUE, pause = FALSE, dup_out = TRUE){
  # Determine if required columns are present
  if(length(which(c("datetime") %in% names(data))) != 1){
    stop("Column datetime is required to check for duplicates")
  } else {
    message("Check for duplicates based on datetime")
  }

  # Select stations to focus on
  if(length(station) > 0 && is.character(station)) {
    data <- data[data$stationname %in% station, ]
    message(paste0("---", length(unique(data$stationname)), " stations selected---"))
  }

  # Split data over stationnames
  data$datetime <- as.character(data$datetime)
  lst.dat <- split(data, f = list(data$stationname))

  # Provide output per station
  s.rsp <- "y"; d <- 1
  while (s.rsp == "y" && d <= length(lst.dat)){
    # Initialise data
    df.tmp <- lst.dat[[d]]

    # Derive frequencies
    df.tab <- as.data.frame(table(df.tmp$datetime))
    names(df.tab) <- c("date", "freq")

    # If requested, determine potential duplicates
    if(dup_out){
      v.date <- df.tab$date[df.tab$freq > 1]
      lst.dat[[d]] <- df.tmp[which(df.tmp$datetime %in% v.date), ]
    }

    # If requested, create visual output
    if(visual){
      p.tmp <- ggplot2::ggplot(df.tab, ggplot2::aes(x = date, y = freq)) +
        ggplot2::geom_point(colour = "#1E64C8", fill = "#1E64C8") +
        ggplot2::scale_x_discrete("") +
        ggplot2::scale_y_continuous("Frequentie", limits = c(0, max(df.tab$freq))) +
        ggplot2::ggtitle(paste0("Observaties voor station ", df.tmp$stationname[1])) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
              axis.text = ggplot2::element_text(colour = "black"),
              axis.text.x = ggplot2::element_text(angle = 90))
      plot(p.tmp)

      # If requested, ask user to continue
      if(pause && d < length(lst.dat)){
        s.rsp <- tolower(readline("Proceed with next station? (Y/N) "))
      }
    }

    # Prepare for next iteration
    d <- d + 1
  }

  # Feedback to user
  message("---All stations in dataset checked---")

  # If requested, return observations that are potential duplicates
  if(dup_out){
    df.out <- do.call("rbind", lst.dat)
    rownames(df.out) <- NULL
    return(df.out[order(df.out$datetime), ])
  }
}

