#' Assign Scheldt geometries
#'
#' `assignGeometry()` assigns Scheldt segments, zones and regions as extra columns of the input dataframe. The function uses
#' internal shapefiles (`map` argument) to intersect coordinate points from `data`
#'
#' @param data A dataframe with 'latitude' and 'longitude' columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()]
#' @param map Two options:
#'    * "T2015" to use the Scheldt Estuary shapefile of the T2015 report
#'    * "T2021" to use the Scheldt Estuary shapefile of the T2021 report
#' @param external Logical. Select `TRUE` to conditionally check stations outside standard geometry (like boundary stations)
#'
#' @return Input dataframe with extra columns added for `segment`, `zones` and `regions`.
#' @export
#'
#' @details
#'
#' For more information about Scheldt segments, zones and regions, please visit the \href{https://www.scheldemonitor.be/sites/scheldemonitor.be/files/2023-08/Evaluatiemethodiek%20Schelde-estuarium%20Update%202021.pdf}{T2021 SM report}
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2010, end = 2015)
#'
#' # add geometry
#' abiotic_data_geometry <- assignGeometry(abiotic_data, "T2015", external = TRUE)
assignGeometry <- function(data, map, external) {
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertChoice(map, c("T2015", "T2021"))
  # Determine if all required columns are present
  if(length(which(c("latitude","longitude") %in% names(data))) != 2){
    stop("Required input: data should have a 'latitude' and a 'longitude' column")
  } else {
    message("Assigning stations to segments/zones/regions")
  }

  # Read shapefiles of Scheldt Estuary
  if(map == "T2021"){
    message("---Using updated map for T2021---")
    shp.omes <- shp.omes.2021
  } else {
    message("---Using original map from T2015---")
    shp.omes <- shp.omes.2015
  }

  # Extract coordinates and remove duplicates
  df.coord <- data[, c("longitude", "latitude")]
  df.coord <- df.coord[!duplicated(df.coord), ]

  # Create a points collection of coordinates
  df.coord.sf <- do.call(sf::st_sfc,
                         c(lapply(1:nrow(df.coord),
                                  function(i) {sf::st_point(as.numeric(df.coord[i, ]))}),
                           list("crs" = 4326))
  )

  # Apply transformation to other reference system for shapefile and data points
  shp.omes.trans <- sf::st_transform(shp.omes, 2163)
  df.coord.trans <- sf::st_transform(df.coord.sf, 2163)

  # Intersect shape and points & extract segment/zone/region code
  if(map == "T2021"){
    # Extract segment information
    df.coord$segment <- apply(sf::st_intersects(shp.omes.trans,
                                            df.coord.trans, sparse = FALSE), 2,
                              function(col) { as.character(shp.omes.trans[which(col), ]$niveau4) }
    )
    # Extract zone information
    df.coord$zone <- apply(sf::st_intersects(shp.omes.trans,
                                         df.coord.trans, sparse = FALSE), 2,
                           function(col) { as.character(shp.omes.trans[which(col), ]$niveau3) }
    )
    # Extract region information
    df.coord$region <- apply(sf::st_intersects(shp.omes.trans,
                                           df.coord.trans, sparse = FALSE), 2,
                             function(col) { as.character(shp.omes.trans[which(col), ]$niveau2) }
    )
  } else {
    # Extract segment information
    df.coord$segment <- apply(sf::st_intersects(shp.omes.trans,
                                            df.coord.trans, sparse = FALSE), 2,
                              function(col) { as.character(shp.omes.trans[which(col), ]$em_code) }
    )
    # Extract zone information + adapt some names
    df.coord$zone <- apply(sf::st_intersects(shp.omes.trans,
                                         df.coord.trans, sparse = FALSE), 2,
                           function(col) { as.character(shp.omes.trans[which(col), ]$salzone) }
    )
    df.coord$zone[grepl("sterke saliniteit", df.coord$zone)] <- "Saliniteitsgradient"
    df.coord$zone[grepl("korte verblijf", df.coord$zone)] <- "Zoet kort verblijf"
    df.coord$zone[grepl("lange verblijf", df.coord$zone)] <- "Zoet lang verblijf"
    # Extract region information
    df.coord$region <- apply(sf::st_intersects(shp.omes.trans,
                                           df.coord.trans, sparse = FALSE), 2,
                             function(col) { as.character(shp.omes.trans[which(col), ]$naam) }
    )
  }

  df.coord$segment <- unlist(lapply(df.coord$segment,
                                    function(x) ifelse(length(unlist(x)) > 0, x, NA)))
  df.coord$zone <- unlist(lapply(df.coord$zone,
                                 function(x) ifelse(length(unlist(x)) > 0, x, NA)))
  df.coord$region <- unlist(lapply(df.coord$region,
                                   function(x) ifelse(length(unlist(x)) > 0, x, NA)))

  # Assign segments/zones/regions to original data
  df.new <- merge(data, df.coord)

  # Conditionally check stations outside standard geometry
  if(external){
    df.new <- checkExternalStations(df.new)
  }

  # Make geometries the last columns
  df.new <- df.new[, c(names(data), "segment", "zone", "region")]

  # Return data with assigned OMES segments
  return(df.new)
}

#' Assign OMES-segments
#'
#' `assignOMES()` assigns Scheldt OMES segments (= omes) as an extra column of the input dataframe. The function uses
#' the overarching function [scheldemonitoR::assignGeometry()]
#'
#' @param data A dataframe with 'latitude' and 'longitude' columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()]
#' @param map Two options:
#'    * "T2015" to use the Scheldt Estuary shapefile of the T2015 report
#'    * "T2021" to use the Scheldt Estuary shapefile of the T2021 report
#' @param external Logical. Select `TRUE` to conditionally check stations outside standard geometry (like boundary stations)
#'
#' @return Input dataframe with extra column `omes` added for omes segment.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695)
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2010, end = 2012)
#'
#' # add OMES segment
#' abiotic_data_omes <- assignOMES(abiotic_data, 'T2015', TRUE)
assignOMES <- function(data, map, external) {
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertChoice(map, c("T2015", "T2021"))

  # Send feedback to user
  message("Assign stations to segments (level 4)")

  # Assign geometries
  message("---Using general function 'scheldemonitoR::assignGeometry()'---")
  df.new <- assignGeometry(data, map = map, external = external)

  # Make "omes" the last column
  names(df.new)[which(names(df.new) == "segment")] <- "omes"
  df.new <- df.new[, c(names(data), "omes")]

  # If some stations were not assigned, print warning message
  if(sum(is.na(df.new$omes)) > 0){
    message("---WARNING: not all stations were assigned to a segment---")
  } else {
    message("---All stations successfully assigned to a segment---")
  }

  # Return data with assigned OMES segments
  return(df.new)
}

#' Assign salinity zones
#'
#' `assignZones()` assigns Scheldt segment and salinity zone as an extra column of the input dataframe. The function uses
#' the overarching function [scheldemonitoR::assignGeometry()]
#'
#' @param data A dataframe with 'latitude' and 'longitude' columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()]
#' @param map Two options:
#'    * "T2015" to use the Scheldt Estuary shapefile of the T2015 report
#'    * "T2021" to use the Scheldt Estuary shapefile of the T2021 report
#' @param external Logical. Select `TRUE` to conditionally check stations outside standard geometry (like boundary stations)
#'
#' @return Input dataframe with extra columns `segment` and `zone`.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695) # High- and lowtide parameters in NAP+cm
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2010, end = 2012)
#'
#' # assign zones
#' abiotic_data <- assignZones(abiotic_data, "T2015", TRUE)
assignZones <- function(data, map, external) {
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertChoice(map, c("T2015", "T2021"))

  # Send feedback to user
  message("Assign stations to salinity zones (level 3)")

  # Assign geometries
  message("---Using general function 'scheldemonitoR::assignGeometry()'---")
  df.new <- assignGeometry(data, map = map, external = external)

  # Make "zone" the last column
  df.new <- df.new[, c(names(data), "segment", "zone")]

  # If some stations were not assigned, print warning message
  if(sum(is.na(df.new$zone)) > 0){
    message("---WARNING: not all stations were assigned to a salinity zone---")
  } else {
    message("---All stations successfully assigned to a salinity zone---")
  }

  # Return data with assigned salinity zones
  return(df.new)
}

#' Assign region (Westerschelde, Zeeschelde, ...)
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Same functionality as [scheldemonitoR::assignGeometry()], that we recommend to use instead
#'
#' @param data A dataframe with 'latitude' and 'longitude' columns. For example, the result
#' of [scheldemonitoR::importAbioticData()] or [scheldemonitoR::importBioticData()]
#' @param map Two options:
#'    * "T2015" to use the Scheldt Estuary shapefile of the T2015 report
#'    * "T2021" to use the Scheldt Estuary shapefile of the T2021 report
#' @param external Logical. Select `TRUE` to conditionally check stations outside standard geometry (like boundary stations)
#'
#' @return Input dataframe with extra columns `segment` and `zone`.
#' @export
#'
#' @examples
#' # import data from high- and lowtide parameters
#' tide_parameters <- c(9694,9695) # High- and lowtide parameters in NAP+cm
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, start = 2010, end = 2012)
#'
#' # assign zones
#' abiotic_data_region <- assignRegion(abiotic_data, "T2021", TRUE)
assignRegion <- function(data, map, external) {
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertChoice(map, c("T2015", "T2021"))

  # Send feedback to user
  message("Assign stations to region (level 2)")

  # Assign geometries
  message("---Using general function 'scheldemonitoR::assignGeometry()'---")
  df.new <- assignGeometry(data, map = map, external = external)

  # Make "region" the last column
  df.new <- df.new[, c(names(data), "segment", "zone", "region")]

  # If some stations were not assigned, print warning message
  if(sum(is.na(df.new$region)) > 0){
    message("---WARNING: not all stations were assigned to a region---")
  } else {
    message("---All stations successfully assigned to a region---")
  }

  # Return data with assigned regions
  return(df.new)
}


#' Check if a station is a boundary station
#'
#' Checks if stations from a dataframe are considered a boundary station, and if so, reasign thems as boundary.
#'
#' @param data A dataframe with "omes", "segment", "zone" or "regions" columns. For example, the result
#' of [scheldemonitoR::assignGeometry()], [scheldemonitoR::assignOMES()], [scheldemonitoR::assignRegion()] or
#' [scheldemonitoR::assignZones()]
#' @param topic Three options for waterquiality: "WK", "WQ", "Waterkwaliteit"
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' # import data for 1046: 'Temperatuur in graden celcius in oppervlaktewater'
#' temperature_data <- importAbioticData("1046", start = 2018, end = 2023)
#'
#' # assign zones
#' temperature_zone <- assignZones(temperature_data, "T2021", TRUE)
#'
#' # check boundaries
#' temperature_boundaries <- checkBoundaries(temperature_zone, "WK")
checkBoundaries <- function(data, topic = "WK") {
  # Assert arguments
  checkmate::assert_data_frame(data)
  checkmate::assertChoice(topic, c("WK", "WQ", "Waterkwaliteit"))

  # Determine if all required columns are present
  if(length(which(c("stationname") %in% names(data))) != 1) {
    stop("Required input: (1) stationname")
  } else {
    message(paste0("Check boundary stations (topic = ", topic, ")"))
  }

  # Determine which stations should be considered as boundaries
  if(topic %in% c(4, "WQ", "WK", "Waterkwaliteit")){
    v.boundaries <- c("Haven Lillobrug", "Haven Lillobrug (804000)",
                      "Dender", "Dender (499500)",
                      "Zenne Zemst", "Zenne Zemst (341560)")
  } else {
    stop(paste0("---In 'checkBoundaries': no known topic '", topic, "'"))
  }

  # Determine all individual stations
  v.stations <- unique(data$stationname)

  # Check for every station if boundary station
  n.count <- 0
  for (i in c(1:length(v.stations))) {
    if(v.stations[i] %in% v.boundaries) {
      n.count <- n.count + 1
      message(paste0("---Re-assign station '", v.stations[i], "' as Boundary station---"))
      if("omes" %in% names(data)) {
        data$omes[data$stationname == v.stations[i]] <- "Boundary"
      }
      if("segment" %in% names(data)) {
        data$segment[data$stationname == v.stations[i]] <- "Boundary"
      }
      if("zone" %in% names(data)) {
        data$zone[data$stationname == v.stations[i]] <- "Boundary"
      }
      if("region" %in% names(data)) {
        data$region[data$stationname == v.stations[i]] <- "Boundary"
      }
    }
  }

  # Return updated data
  if(n.count == 0) { message("---No stations were re-assigned as boundary---") }
  return(data)
}


# helper functions ---------------------------------------------------------------------------------
checkExternalStations <- function(data){
  # Determine if all required columns are present
  if(length(which(c("stationname", "segment", "zone", "region") %in% names(data))) != 4){
    stop("Required input: (1) stationname, (2) segment, (3) zone and (4) region")
  } else {
    message("Assign external stations to associated geometry")
  }

  # Determine all stations without geometry
  v.stations <- unique(data$stationname[is.na(data$segment)])

  # Run through all stations and assign geometries
  if(length(v.stations) > 0){
    for (i in c(1:length(v.stations))){
      # Case 1: Known boundary stations
      if(v.stations[i] %in% c("Bovenschelde", "Bovenschelde (172100)",
                              "Dender", "Dender (499500)",
                              "Dender Wiezebrug (499900)",
                              "Dijle Mechelen", "Dijle Mechelen (212400)",
                              "Getijdenete Lier (270400)",
                              "Grote Nete", "Grote Nete (253000)",
                              "Kleine Nete", "Kleine Nete (272000)",
                              "Zenne Mechelen (341550)",
                              "Zenne Heffen (341000)")){
        message(paste0("---Station '", v.stations[i], "' added as Boundary station---"))
        data$segment[data$stationname == v.stations[i]] <- "Boundary"
        data$zone[data$stationname == v.stations[i]] <- "Boundary"
        data$region[data$stationname == v.stations[i]] <- "Boundary"
      }

      # Case 2: Nete near Walem (VMM)
      if(v.stations[i] %in% c("Beneden-Nete (250000)")){
        message(paste0("---Station '", v.stations[i], "' added to 'Getijdenete'---"))
        data$segment[data$stationname == v.stations[i]] <- "GetijdeNete"
        data$zone[data$stationname == v.stations[i]] <- "Zoet zijrivier"
        data$region[data$stationname == v.stations[i]] <- "Zijrivieren"
      }

      # Case 3: Baasrode near Dendermonde (VMM)
      if(v.stations[i] %in% c("Baasrode (163500)")){
        message(paste0("---Station '", v.stations[i], "' added to 'Comp. 15'---"))
        data$segment[data$stationname == v.stations[i]] <- "Comp. 15"
        data$zone[data$stationname == v.stations[i]] <- "Zoet lang verblijf"
        data$region[data$stationname == v.stations[i]] <- "Zeeschelde"
      }

      # Case 4: Hansweert for Zooplankton (RWS)
      if(v.stations[i] %in% c("W6 - Hansweert")){
        message(paste0("---Station '", v.stations[i], "' added to 'Comp. 4'---"))
        data$segment[data$stationname == v.stations[i]] <- "Comp. 4"
        data$zone[data$stationname == v.stations[i]] <- "Zwak polyhalien"
        data$region[data$stationname == v.stations[i]] <- "Westerschelde"
      }
    }
  }

  # Return updated dataframe
  return(data)
}
