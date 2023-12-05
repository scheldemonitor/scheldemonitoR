#' Import Abiotic Data
#'
#' `importAbioticData()` uses the \href{https://www.scheldemonitor.be/dataproducts/en/download/}{ScheldeMonitor data download webservices}
#' to download abiotic data, returning a dataframe. To download biotic data please use [importBioticData()]
#'
#' @param parameter A ScheldeMonitor parameter id or a list of parameter ids
#' @param start Start year
#' @param end End year
#'
#' @return A dataframe with the available data for the parameters and years selected
#' @export
#'
#' @examples
#' # Define parameters
#' tide_parameters <- c(9694,9695) # High- and lowtide parameters in NAP+cm
#'
#' # download data
#' abiotic_data <- importAbioticData(tide_parameters, 2010, 2023)
importAbioticData <- function(parameter, start, end) {
  # Check if input has correct options
  checkmate::assert_number(start, FALSE, 1, 10000, null.ok = FALSE)
  checkmate::assert_number(end, FALSE, 1, 10000, null.ok = FALSE)
  # Define time period
  v.years <- c(start:end)

  # Define fixed elements of URL
  v.url <- c('http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Aabiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+standardparameterid+IN+%28',
             '%29+AND+%28%28datetime_search+BETWEEN+%27',
             '-01-01%27+AND+%27',
             '-12-32%27+%29%29%3Bcontext%3A0001%3Bloggedin%3A1&propertyName=stationname%2Clongitude%2Clatitude%2Cdatetime%2Cdepth%2Cparametername%2Cvaluesign%2Cvalue%2Cunit%2Cdataprovider%2Cdatasettitle%2Clod%2Cloq%2Ccategory%2Cseason%2Cclassunit%2Cclass%2Caphiaid%2Cdateprecision%2Cdatafichetitle%2Cdataficheid%2Cstandardparameterid&outputFormat=csv')

  # Define parameter-specific section of URL
  n.par <- paste(parameter, collapse = '%5C%2C')

  # Define temporary list for output
  lst.dat <- list()

  # Import data for each year
  for (i in 1:length(v.years)) {
    message(paste0('Import for year ', v.years[i]))
    # Compose year-specific URL and import data
    s.url <- paste0(v.url[1], n.par, v.url[2], v.years[i], v.url[3],
                    v.years[i], v.url[4])
    lst.dat[[i]] <- data.frame(utils::read.csv(s.url, stringsAsFactors = F))

    # Print feedback to user
    if (nrow(lst.dat[[i]]) == 0) {
      message(paste0('---No data for year ', v.years[i], '---'))
    } else {
      message(paste0('---Data for year ', v.years[i], ' added---'))
    }
  }

  # Generate dataframe from list
  df.dat <- do.call(rbind, lst.dat)

  # Generate output
  return(df.dat)
}

#' Import Biotic Data
#'
#' `importBioticData()` uses the \href{https://www.scheldemonitor.be/dataproducts/en/download/}{ScheldeMonitor data download webservices}
#' to download biotic data, returning a dataframe. To download abiotic data please use [importAbioticData()]
#'
#' @param parameter Two options:
#'    * The id or list of ids of an \href{https://www.vliz.be/nl/imis?module=dataset}{IMIS (Integrated Marine Information System) dataset}.
#'    * The AphiaID or list of AphiaIDs of a scientific name. This information is available in the \href{(https://www.marinespecies.org/index.php)}{WoRMS website} or through the [worrms::wm_name2id()] function.
#' @param start Start year
#' @param end End year
#' @param source Two options:
#'    * `"imis"` if `parameter` is an IMIS dataset id.
#'    * `"aphia"` if `parameter` is an AphiaID
#'
#' @return A dataframe with the available data for the parameters and years selected
#' @export
#'
#' @examples
#' # using source = 'imis' ---------------------------------------------------------------------
#' # Define parameters (IMIS dataset ids):
#' # 1073: OMES: Monitoring zooplankton in the Zeeschelde
#' ## (https://www.vliz.be/nl/imis?module=dataset&dasid=1073)
#' # 5191: MWTL biological monitoring network Westerschelde: Mesozooplankton
#' ## (https://www.vliz.be/nl/imis?module=dataset&dasid=5191)
#'
#' # make a list of the dataset ids
#' imis_datasets <- c(1073, 5191)
#'
#' # download data
#' biotic_data_imis <- importBioticData(imis_datasets, 2015, 2021, 'imis')
#'
#' # using source = 'aphia' --------------------------------------------------------------------
#' # find AphiaID for bristleworm
#' # install.packages(worrms)
#'
#' # find the AphiaID from the taxonomic name:
#' worrms::wm_name2id("Aphelochaeta marioni")
#'
#' # dowloand data using the AphiaID
#' biotic_data_aphia <- importBioticData(129938, 1994, 2008, "aphia")
importBioticData <- function(parameter, start, end, source) {
  # Check if input has correct options
  checkmate::assert_number(start, FALSE, 1, 10000, null.ok = FALSE)
  checkmate::assert_number(end, FALSE, 1, 10000, null.ok = FALSE)
  checkmate::assertChoice(source, c("imis", "aphia"))

  # Define time period
  v.years <- c(start:end)

  # Define fixed elements of URL, separate between imis and aphia
  if(source == 'imis'){
    v.url <- c('http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Abiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+imisdatasetid+IN+%28',
               '%29+AND+%28%28datetime_search+BETWEEN+%27',
               '-01-01%27+AND+%27',
               '-12-32%27+%29%29%3Bcontext%3A0001%3Bloggedin%3A1&propertyName=stationname%2Caphiaid%2Cscientificname%2Cobservationdate%2Clongitude%2Clatitude%2Cvalue%2Cparametername%2Cdataprovider%2Cimisdatasetid%2Cdatasettitle%2Cdatafichetitle%2Cdataficheid%2Careaname%2Cdateprecision%2Cstadium%2Cgender%2Cvaluesign%2Cdepth%2Cclassunit%2Cclass%2Cstandardparameterid&outputFormat=csv')
  } else {
    v.url <- c('http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Abiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+%28%28datetime_search+BETWEEN+%27',
               '-01-01%27+AND+%27',
               '-12-32%27+%29%29%3Bcontext%3A0001%3Baphiaid%3A',
               '%3Bloggedin%3A1&propertyName=stationname%2Caphiaid%2Cscientificname%2Cobservationdate%2Clongitude%2Clatitude%2Cvalue%2Cparametername%2Cdataprovider%2Cimisdatasetid%2Cdatasettitle%2Cdatafichetitle%2Cdataficheid%2Careaname%2Cdateprecision%2Cstadium%2Cgender%2Cvaluesign%2Cdepth%2Cclassunit%2Cclass%2Cstandardparameterid&outputFormat=csv')
  }

  # Define parameter-specific section of URL
  n.par <- paste(parameter, collapse = '%5C%2C')

  # Define temporary list for output
  lst.dat <- list()

  # Import data for each year
  for (i in 1:length(v.years)) {
    message(paste0('Import for year ', v.years[i]))
    # Compose year-specific URL and import data
    if(source == 'imis'){
      s.url <- paste0(v.url[1], n.par, v.url[2], v.years[i], v.url[3],
                      v.years[i], v.url[4])
    } else {
      s.url <- paste0(v.url[1], v.years[i], v.url[2], v.years[i], v.url[3],
                      n.par, v.url[4])
    }

    lst.dat[[i]] <- data.frame(utils::read.csv(s.url, stringsAsFactors = F))

    # Print feedback to user
    if (nrow(lst.dat[[i]]) == 0) {
      message(paste0('---No data for year ', v.years[i], '---'))
    } else {
      message(paste0('---Data for year ', v.years[i], ' added---'))
    }
  }

  # Generate dataframe from list
  df.dat <- do.call(rbind, lst.dat)

  # Generate output
  return(df.dat)
}
