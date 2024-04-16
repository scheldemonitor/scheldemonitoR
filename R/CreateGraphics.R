# TODO: is there a better way to bind these global variables? They come from the ggplot2::aes() call.
utils::globalVariables(c("group", "label", "x.lab", "y.lab", "Periode", "location"))

#' Calculate distance to Vlissingen.
#'
#' @description
#' Uses a river network file creating using the [riverdist] pacakge. The funcion assigns the closes
#' distance to a segment and thn from that segment to Vlissingen for each of the coordinate points
#' from a dataframe.
#'
#' Read more \href{https://jsta.github.io/riverdist/articles/riverdist_vignette.html}{here}.
#'
#' @param data A dataframe with 'latitude' and 'longitude' columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()]
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # import data for 1046: 'Temperatuur in graden celcius in oppervlaktewater'
#' temperature_data <- importAbioticData("1046", start = 2018, end = 2023)
#'
#' # calculate distantce to Vlissingen:
#' temperature_data_vliss <- distToVlissingen(temperature_data)
distToVlissingen <- function(data) {
  # Determine if all required columns are present
  if(length(which(c("latitude", "longitude") %in% names(data))) != 2){
    stop("Required input: (1) latitude and (2) longitude columns")
  } else {
    message("Calculate distance to Vlissingen")
  }

  # Read shapefiles Scheldt (from "Generate_RiverNetwork.R")
  # shp.schelde <- readRDS(paste0("~/T2021/00_Algemeen/Data/Gebiedsindeling/",
  #                               "Lengtesegmenten/Schelde_river_network.rds"))
  shp.schelde <- shp.schelde

  # Extract coordinates, select unique and existing coordinates
  df.coord <- data[, c("longitude", "latitude")]
  df.coord$longitude <- as.numeric(df.coord$longitude)
  df.coord$latitude <- as.numeric(df.coord$latitude)
  df.coord <- unique(df.coord[df.coord$latitude > 0 & df.coord$longitude > 0, ])

  # Transform coordinates to coordinate reference system from Scheldt (via decimal)
  df.coord.dec <- sp::SpatialPoints(coords = df.coord, proj4string = sp::CRS("+proj=longlat"))
  df.coord.utm <- sp::spTransform(df.coord.dec, sp::CRS(shp.schelde$sp@proj4string@projargs))

  # Determine segment and vertex
  df.river <- riverdist::xy2segvert(x = df.coord.utm@coords[, 1],
                                    y = df.coord.utm@coords[, 2],
                                    rivers = shp.schelde)

  # Calculate distance to mouth for each station
  df.coord$distance <- round(sapply(seq(1, nrow(df.river)), function(i) {
    v = df.river[i, ]
    riverdist::mouthdist(seg = v$seg, vert = v$vert, rivers = shp.schelde)
  } ) / 1000, 1)

  # Merge distance with original data and throw message for locations without distance
  df.data <- merge(data, df.coord, by = c("longitude", "latitude"))
  if(sum(is.na(df.data$distance)) > 0){
    message("---Distance could not be calculated for all locations---")
  } else {
    message("---Distance successfully calculated for all locations---")
  }

  # Return location-specific distances from original data
  return(df.data)
}

#' Make surface plot (x = date, y = distance Vlissingen, z = value parameter)
#'
#' Make surface plot (x = date, y = distance Vlissingen, z = value parameter)
#'
#' @param data A dataframe at least with 'latitude', 'longitude' and 'datetime' columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()]. Preferred, a dataframe with
#' column 'distance' as the result of [scheldemonitoR::distToVlissingen()]
#' @param varname Character. Optional. Variable name to include in the title of the plot. Placehorder is `Variable`.
#' @param varunit Character. Optional. Variable unit to include in the title of the plot. Placehorder is `-`.
#' @param maxgapdays Numeric. Maximum desired gap days.
#' @param colclass Numeric. Break points for layer.
#'
#' @return A plot object.
#' @export
#'
#' @examples
#' # import data for 1046: 'Temperatuur in graden celcius in oppervlaktewater'
#' temperature_data <- importAbioticData("1046", start = 2018, end = 2023)
#'
#' # calculate distantce to Vlissingen:
#' temperature_data_vliss <- distToVlissingen(temperature_data)
#'
#' # With column distance calculated ------------------------------------------
#' surface_plot <- makeSurfacePlot(temperature_data_vliss, varname = "Temperatuur", varunit = "C")
#' # Without column distance calculated ---------------------------------------
#' surface_plot <- makeSurfacePlot(temperature_data, varname = "Temperatuur", varunit = "C")
makeSurfacePlot <- function(data, varname = "Variable", varunit = "-", maxgapdays = 80, colclass = 10){
  # Determine if the required column "datetime" is present
  if(!"datetime" %in% names(data)){
    stop("Datetime is required input")
  } else {
    message(paste0("Make surface plot for ", varname))
  }

  # Determine if the optional column "distance" is present
  if(!"distance" %in% names(data)){
    message("--- WARNING: Distance not given, calculating distance ---")
    data <- distToVlissingen(data)
  }

  # Convert datetime to only date (simplification) and sort data
  message("---Simplification of datetime to a date to ease interpolation---")
  data$datetime <- as.Date(data$datetime, format("%Y-%m-%d"))
  data <- data[order(data$datetime, data$distance), ]

  # Select data for interpolation (note: rescaling needed for "interp" function)
  v.date <- as.numeric(data$datetime) - as.numeric(min(data$datetime))
  v.date.scale <- v.date / max(v.date)
  v.dist.scale <- data$distance / max(data$distance)

  # Determine interpolation interval
  message("---Interpolation on a 120x80 grid---")
  v.date.seq <- seq(min(v.date.scale), max(v.date.scale), length.out = 120)
  v.dist.seq <- seq(min(v.dist.scale), max(v.dist.scale), length.out = 80)

  # Perform interpolation
  df.interp <- akima::interp(x = v.date.scale, y = v.dist.scale,
                             z = as.numeric(data$value), xo = v.date.seq,
                             yo = v.dist.seq, duplicate = "mean")

  # Perform back-transformation
  df.interp$x <- as.Date(df.interp$x * max(v.date), origin = min(data$datetime))
  df.interp$y <- df.interp$y * max(data$distance)

  # Define break points for date axis
  v.breaks.x <- seq(as.Date(paste0(format(min(data$datetime), "%Y"), "-01-01")),
                    as.Date(paste0(format(max(data$datetime), "%Y"), "-01-01")),
                    by = "year")

  # Define break points for legend
  v.breaks.z <- seq(0, signif(max(df.interp$z, na.rm = T), digits = 1),
                    length.out = colclass + 1)

  # Identify data gaps in the provided data
  v.date.sort <- unique(sort(data$datetime))
  v.date.gaps <- as.numeric(difftime(v.date.sort[2:length(v.date.sort)],
                                     v.date.sort[1:length(v.date.sort) - 1],
                                     units = "days"))
  v.gap.min <- v.date.sort[c(v.date.gaps > maxgapdays, F)]
  v.gap.max <- v.date.sort[c(F, v.date.gaps > maxgapdays)]

  # Generate plot
  p.surface <- graphics::filled.contour(
    x = as.Date(format(df.interp$x, "%Y-%m-%d")), y = df.interp$y,
    z = df.interp$z, levels = v.breaks.z, plot.axes = {
      # Manage x axis for dates
      graphics::axis(1, at = v.breaks.x, labels = F)
      graphics::text(x = v.breaks.x,
                     y = min(df.interp$y) - ((max(df.interp$y) - min(df.interp$y))/20),
                     labels = format(v.breaks.x, "%Y"), srt = 45, pos = 1, xpd = TRUE)
      # Manage y axis for distance
      graphics::axis(2, unique(round(seq(min(df.interp$y), max(df.interp$y), length.out = 6), -1)))
      # Add rectangles when data is missing
      if(length(v.gap.min) > 0){
        graphics::rect(as.Date(format(v.gap.min, "%Y-%m-%d")), min(df.interp$y),
                       as.Date(format(v.gap.max, "%Y-%m-%d")), max(df.interp$y),
                       col = "white")
        graphics::rect(as.Date(format(v.gap.min, "%Y-%m-%d")), min(df.interp$y),
                       as.Date(format(v.gap.max, "%Y-%m-%d")), max(df.interp$y),
                       density = 10)
      }
    },
    plot.title = {
      graphics::title(main = paste0("Spatiaal-temporeel verloop voor ", varname),
                      xlab = "", line = 3, ylab = "Afstand tot Vlissingen (km)")
    },
    key.title = {
      graphics::title(main = paste0("[", varunit, "]"))
    },
    key.axes = {
      graphics::axis(4, v.breaks.z, labels = round(v.breaks.z, 2))
    }
  )
  # Return output
  # return(p.surface) # Graphical construction cannot be returned ... consider ggplot?
}


# TODO: reduce the ammount of arguments. Why are so many customizable?
#' Make scatter plot statistic versus time (faceted spatially)
#'
#' @description
#' Make scatter plot statistic versus time (faceted spatially)
#' `r lifecycle::badge("experimental")`
#'
#' @param data A dataframe with geometry (`zone`, `segment` or `stationname`) columns,
#' and `datetime` and `value` columns. For example, the result of [scheldemonitoR::assignGeometry()].
#' @param varname Character. Optional. Variable name to include in the title of the plot. Placehorder is `Variable`.
#' @param varunit Character. Optional. Variable unit to include in the title of the plot. Placehorder is `-`.
#' @param spatial Character. Choose spatial object, one of `stationname`, `zone` or `segment`. Default is `zone`.
#' @param temporal Character. Either `year` or `month`. Default is `month`.
#' @param delta Numeric. If higher than 1 function will add background rectangles.
#' @param digits Numeric. Number of digits for the axis labels. Default is `2`.
#' @param upper_x Autocalculated.
#' @param fixed_y Aesthetics for the y axis.
#' @param ticks_y Aesthetics for the y axis.
#' @param limits Limits for each spatial level.
#' @param threshold Numeric. Add a threshold line. Default is `NULL`.
#' @param grouping Wheter to group or not. Default is `NULL`.
#'
#' @return A plot object.
#' @export
#'
#' @examples
#' # import data for 1046: 'Temperatuur in graden celcius in oppervlaktewater'
#' temperature_data <- importAbioticData("1046", start = 2018, end = 2023)
#'
#' # add geometries
#' temperature_data_geo <- assignGeometry(temperature_data, "T2015", external = TRUE)
#'
#' makeTemporalPlot(temperature_data_geo, varname = "Temperature", varunit = "C", temporal = "month")
makeTemporalPlot <- function(data, varname = "Variable", varunit = "-", spatial = "zone", temporal = "year",
                             delta = 0, digits = 2, upper_x = max(data$year), fixed_y = FALSE, ticks_y = 4,
                             limits = NULL, threshold = NULL, grouping = FALSE) {
  #Assert arguments
  checkmate::assert_data_frame(data)

  # check if 'year' column is present, if not calculate year
  if(!("year" %in% names(data))){
    data$year <- lubridate::year(data$datetime)
  }

  # Determine if required columns are present
  if(tolower(temporal) == "month" && !(tolower(temporal) %in% names(data))) {
    data$month <- lubridate::month(data$datetime)
    #stop("Column month is required when specifying temporal resolution to month")
  }
  if(length(which(c("year", "value", tolower(spatial)) %in% names(data))) != 3) {
    stop(paste0("Required input: (1) year, (2) value and (3) ", spatial))
  } else {
    message(paste0("Make faceted temporal plot for ", varname, " (", spatial,
                   ", ", temporal, ")"))
  }

  # Provide message to specify variable name (if applicable)
  if(varname == "Variable"){
    message("---Recommended to specify variable name---")
  }

  # Provide message on type of limits provided by user
  if(!is.list(limits) & !is.null(limits)) {
    message("---For limits-parameter, a list is requested per level---")
    limits <- NULL
  }

  # Identify order of all spatial levels
  if(tolower(spatial) == "zone") {
    v.spat <- v.namesZones
  } else if(tolower(spatial) == "segment") {
    v.spat <- v.namesSegments
  } else if(tolower(spatial) == "stationname") {
    v.spat <- levels(as.factor(data$stationname))
  }
  if(length(v.spat) > 15) {
    message("---WARNING: High number of spatial levels. Subsetting suggested---")
  }

  # Create inclusive dataframe with missing spatial levels
  df.spat <- as.data.frame(v.spat); names(df.spat) <- spatial
  df.spat <- merge(df.spat, data, all.x = T)

  # Adapt formats
  df.spat$year <- as.numeric(df.spat$year)
  df.spat$year[is.na(df.spat$year)] <- round(mean(range(df.spat$year, na.rm = T)), 0)
  df.spat$year <- as.character(df.spat$year)
  if(tolower(temporal) == "month") {
    df.spat$month <- as.numeric(df.spat$month)
    df.spat$month[is.na(df.spat$month)] <- round(mean(range(df.spat$month, na.rm = T)), 0)
  }
  df.spat[,1] <- factor(df.spat[,1], v.spat)

  # Create date-column for graphs
  if(temporal == "year") {
    df.spat$date <- with(df.spat, as.Date(paste0(year, "-01-01")))
  } else if(temporal == "month"){
    df.spat$date <- with(df.spat, as.Date(paste0(year, "-", month, "-01")))
  }

  # Add labels to missing facets
  df.spat$x.lab <- as.Date(mean(range(df.spat$date, na.rm = T)))
  for (i in c(1:length(v.spat))) {
    # Specify y-coordinates
    df.spat$y.lab[df.spat[,1] == v.spat[i]] <-
      ifelse(is.na(df.spat$value[df.spat[,1] == v.spat[i]][1]),
             mean(range(df.spat$value, na.rm = T)),
             mean(range(df.spat$value[df.spat[,1] == v.spat[i]][1], na.rm = T))
      )

    # Specify label text
    df.spat$label[df.spat[,1] == v.spat[i]] <-
      ifelse(is.na(df.spat$value[df.spat[,1] == v.spat[i]][1]),
             "Geen data beschikbaar", NA_character_)
  }

  # Determine limits of each spatial level (if applicable)
  if(!fixed_y) {
    df.lmt <- df.spat
    for (i in c(1:length(levels(df.spat[, 1])))){
      s.tmp <- levels(df.spat[, 1])[i]
      # If values are provided, use those
      if(i <= length(limits) && length(limits[[i]]) == 2){
        df.lmt$value[df.lmt[, 1] == s.tmp] <-
          c(rep(min(limits[[i]]), sum(df.lmt[, 1] == s.tmp) - 1),
            max(limits[[i]]))
      } else {
        df.lmt$value[df.lmt[, 1] == s.tmp] <-
          c(rep(floor(min(df.spat$value[df.spat[, 1] == s.tmp])),
                sum(df.lmt[, 1] == s.tmp) - 1),
            ceiling(max(df.spat$value[df.spat[, 1] == s.tmp])))
      }
    }
    df.lmt$value[is.na(df.lmt$value)] <- mean(df.spat$value)
  }

  # Create basis of plot
  if(grouping) {
    p.trendSpatial <- ggplot2::ggplot(df.spat, ggplot2::aes(x = date, y = value, group = group, color = group))
  } else {
    p.trendSpatial <- ggplot2::ggplot(df.spat, ggplot2::aes(x = date, y = value))
  }

  # Add background rectangles, if applicable
  if(delta > 0) {
    n.max <- upper_x
    n.count <- 1
    while (n.max > min(data$year)) {
      n.min <- n.max - (delta - 1)

      # Add box, conditional to temporal scale
      if((n.count %% 2) == 1 & temporal == "year") {
        p.trendSpatial <- p.trendSpatial +
          ggplot2::geom_rect(xmin = as.Date(paste0(n.min - 1, "-06-30")),
                             xmax = as.Date(paste0(n.max, "-06-30")),
                             ymin = -Inf, ymax = Inf, fill = "grey80", color = NA)
      }
      if((n.count %% 2) == 1 & temporal == "month") {
        p.trendSpatial <- p.trendSpatial +
          ggplot2::geom_rect(xmin = as.Date(paste0(n.min, "-01-01")),
                             xmax = as.Date(paste0(n.max, "-12-31")),
                             ymin = -Inf, ymax = Inf, fill = "grey80", color = NA)
      }
      # Update values
      n.max <- n.min - 1
      n.count <- n.count + 1
    }
  }

  # Display the data, conditional to grouping
  if (grouping) {
    p.trendSpatial <- p.trendSpatial +
      ggplot2::geom_point(na.rm = T) +
      ggplot2::geom_smooth(method = "gam", formula = y ~ s(x, k = 10), na.rm = T)
  } else {
    p.trendSpatial <- p.trendSpatial +
      ggplot2::geom_point(na.rm = T) +
      ggplot2::geom_smooth(method = "gam", formula = y ~ s(x, k = 10), colour = "#1E64C8", fill = "#1E64C8", na.rm = T)
  }

  # Add threshold line (if applicable)
  if(!is.null(threshold)) {
    p.trendSpatial <- p.trendSpatial +
      ggplot2::geom_hline(ggplot2::aes(yintercept = threshold), col = "red", lty = "dotted")
  }

  # Continue with layout of graphs
  # f.facetLimits <- function(y) range(scales::breaks_extended()(y))
  #
  # f.facetBreaks <- function(n) {
  #   function(y) {
  #     breaks <- f.facetLimits(y)
  #     seq(breaks[1], breaks[2], length.out = n)
  #   }
  # }

  # Extra function needed for spacing y-axis. Credit for approach:
  # https://stackoverflow.com/a/28459434
  # Defining breaks function, s is scaling factor (multiplicative expand)
  # f.equalBreaks <- function(n = 4, s = 0.05, ...){
  #   function(x){
  #     # rescaling
  #     d <- s * diff(range(x)) / (1 + 2 * s)
  #     seq(min(x) + d, max(x) - d, length = n)
  #   }
  # }

  # Finalise graphs, with equally spaced and rounded y-axis
  if(fixed_y) {
    p.trendSpatial <- p.trendSpatial +
      ggplot2::facet_wrap(~df.spat[, 1], ncol = 3, drop = FALSE) +
      ggplot2::scale_y_continuous(paste0(varname, " [", varunit, "]"), n.breaks = ticks_y,
                                  label = function(x) sprintf(paste0("%.", digits, "f"), x))
  } else {
    p.trendSpatial <- p.trendSpatial +
      ggplot2::geom_point(data = df.lmt, alpha = 0) +
      ggplot2::facet_wrap(~df.spat[, 1], ncol = 3, scales = "free_y", drop = FALSE) +
      ggplot2::scale_y_continuous(paste0(varname, " [", varunit, "]"),
                                  breaks = equalBreaks(n = ticks_y, s = 0.05),
                                  expand = c(0.05, 0),
                                  label = function(x) sprintf(paste0("%.", digits, "f"), x))
  }

  p.trendSpatial <- p.trendSpatial +
    ggplot2::geom_text(ggplot2::aes(x.lab, y.lab, label = label), colour = "red", size = 3, na.rm = T) +
    # scale_x_date("", date_breaks = paste0(ifelse(delta > 0, delta, 5), " years"),
    #              date_labels = "%Y") +
    ggplot2::scale_x_date("", breaks = as.Date(paste0(seq(upper_x, min(data$year),
                                                          -1 * ifelse(delta > 0, delta, 5)),
                                                      "-01-01")), date_labels = "%Y") +
    ggplot2::scale_colour_manual(values = v.plotColours, na.translate = F) +
    ggplot2::scale_fill_manual(values = v.plotColours, na.translate = F) +
    ggplot2::scale_shape_manual(values = v.plotShapes, na.translate = F) +
    # ggplot2::coord_cartesian(xlim = c("NA_Date_", as.Date(paste0(upper_x, "-12-31"))),
    #                 ylim = c(0.0001, NA)) +
    scheldemonitoR::themeSib()

  # Return graphical object
  message(paste0("---Faceted plot generated for ", varname, "---"))
  return(p.trendSpatial)
}

#' Make barplot for reporting period
#'
#' @description
#' Make barplot for reporting period
#'
#' Check also: `04_Waterkwaliteit/03_Toetsparameter Nutrienten/Scripts/c. Analysis scripts/AnalyseNitrietPeriodiek.R``
#'
#' @param data A dataframe with columns `location` (as returned with [scheldemonitoR::aggregatePeriodPerStation()]),
#' `period` (year period) and `value` (parameter value).
#' @param locations Character or list. List of locations to plot. If null,
#' @param varname Character. Optional. Variable name to include in the title of the plot. Placehorder is `Variable`.
#' @param varunit Character. Optional. Variable unit to include in the title of the plot. Placehorder is `-`.
#' @param threshold Numeric. Add a threshold line. Default is `NULL`.
#'
#' @return A plot.
#' @export
#'
#' @examples
#' # import data for 1046: 'Temperatuur in graden celcius in oppervlaktewater'
#' temperature_data <- importAbioticData("1046", start = 2018, end = 2023)
#'
#' # add geometries
#' temperature_data_geo <- assignGeometry(temperature_data, "T2015", external = TRUE)
#'
#' # aggregate data per station
#' df <- aggregatePeriodPerStation(temperature_data_geo, stat = 'max', reduce = TRUE)
#'
#' # add period. Data can also be filtered for a specific period (2018-2020)
#' # using scheldemonitoR aggregate functions
#' df$period <- paste0("2018-2023")
#'
#' makePeriodPlot(df, location = c("154100","157000","159000", "160200"), "temperature", "C", 18)
makePeriodPlot <- function(data, locations = NULL, varname = "Variable", varunit = "-", threshold = NULL){
  #Assert arguments
  checkmate::assert_data_frame(data)

  # Determine if all required columns are present
  if(length(which(c("location", "period", "value") %in% names(data))) != 3){
    stop("Required input: (1) location, (2) period and (3) value")
  } else {
    message(paste0("Make barplot for ", varname, " (reporting period)"))
  }

  # Provide message to specify variable name (if applicable)
  if(varname == "Variable"){
    message("---Recommended to specify variable name and unit---")
  }

  # allowing to plot data for stations that are not in the predefined list.
  #Select data
  if(is.null(locations)) {
    df.dat <- data
    if (length(unique(df.dat$location > 10))) {
      message("---Plotting data for more than 10 locations is not recommended, specify locations---")
    }
  } else {
    df.dat <- data[data$location %in% locations, ]
  }

  # Adapt formats
  #df.dat <- data

  df.dat$location <- factor(df.dat$location)
  df.dat$value <- as.numeric(df.dat$value)

  # Create barplot
  names(df.dat)[which(names(df.dat) == "period")] <- "Periode"
  p.barplot <- ggplot2::ggplot(df.dat, ggplot2::aes(x = location, y = value)) +
    ggplot2::geom_col(ggplot2::aes(fill = Periode), colour = "black", position = "dodge") +
    ggplot2::scale_x_discrete("", drop = F) +
    ggplot2::scale_y_continuous(paste0(varname, " [", varunit, "]")) +
    ggplot2::scale_fill_brewer(palette = "Blues") +
    scheldemonitoR::themeSib()

  # Add threshold line (if applicable)
  if(!is.null(threshold)){
    p.barplot <- p.barplot +
      ggplot2::geom_hline(ggplot2::aes(yintercept = threshold), col = "red", lty = "dotted")
  }

  # Return barplot
  message(paste0("---Barplot generated for ", varname, "---"))
  return(p.barplot)
}


# helper --------------------------------------------------------------------------------------------
# Extra function needed for spacing y-axis. Credit for approach:
# https://stackoverflow.com/a/28459434
# Defining breaks function, s is scaling factor (multiplicative expand)
equalBreaks <- function(n = 4, s = 0.05, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1 + 2 * s)
    seq(min(x) + d, max(x) - d, length = n)
  }
}

# Suggest order of facets/labels (downstream direction)
v.namesSegments <- c("GetijdeDijle", "GetijdeNete", "GetijdeZenne",
                     "GetijdeDurme", "Rupel", "Ringvaart", "Comp. 19 trGM",
                     "Comp. 19 trMZ", "Comp. 19", "Comp. 18", "Comp. 17",
                     "Comp. 16", "Comp. 15", "Comp. 14", "Comp. 13", "Comp. 12",
                     "Comp. 11", "Comp. 10", "Comp. 9", "Comp. 8", "Comp. 7b",
                     "Comp. 7a", "Comp. 6", "Comp. 5", "Comp. 4", "Comp. 3",
                     "Comp. 2", "Comp. 1", "Monding")
v.namesZones <- c("Zoet zijrivier", "Durme", "Rupel", "Zoet kort verblijf",
                  "Zoet lang verblijf", "Oligohalien", "Saliniteitsgradient",
                  "Mesohalien", "Zwak polyhalien", "Sterk polyhalien", "Monding")
v.namesMix <- c("GetijdeDijle", "GetijdeNete", "GetijdeZenne", "GetijdeDurme",
                "Rupel", "Ringvaart", "Comp. 19 trGM", "Comp. 19 trMZ",
                "Comp. 19", "Comp. 18", "Comp. 17", "Comp. 16", "Comp. 15",
                "Comp. 14", "Comp. 13", "Comp. 12", "Comp. 11", "Comp. 10",
                "Comp. 9", "Comp. 7b", "Mesohalien", "Zwak polyhalien",
                "Sterk polyhalien", "Monding")
v.plotColours <- c("#1E64C8", "#D55E00", "#CC79A7", "#000000", "#E69F00",
                   "#56B4E9", "#009E73", "#F0E442")
v.plotShapes <- c(16, 18, 15, 17, 4, 8, 13, 11)
