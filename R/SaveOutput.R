#' Saves aggregated by Period data
#'
#' Saves aggregated by Period data.
#'
#' @param data A dataframe with columns 'location', 'period' and 'value' columns. Like
#' the result of [scheldemonitoR::aggregatePeriodPerZone()].
#' @param locations Character or list. Spatial resolution to filter columbn location.
#' Default is `NULL`.
#' @param cols Character or list. Columns to select from dataframe.
#' @param fileout Path ans extension to save the file to. For example `Plots/myfile.txt`.
#'
#' @return A file.
#' @export
#'
#' @examples
#' library(dplyr)
#' # import data
#' temperature_data <- importAbioticData("1046", start = 2018, end = 2023)
#' # assign geometry
#' temperature_data_geo <- assignGeometry(temperature_data, "T2015", external = TRUE)
#' # aggregate data per period
#' temperature_data_geo_period <- aggregatePeriodPerZone(temperature_data_geo, stat = "mean")
#' # rename columns
#' temperature_data_geo_period <- temperature_data_geo_period %>%
#'                                         rename(location = zone, period = data)
#' # save info
#' savePeriodWide(temperature_data_geo_period, fileout = "mifile.txt")
savePeriodWide <- function(data, locations = NULL, cols = NULL, fileout){
  # Determine if all required columns are present
  if(length(which(c('location', 'period', 'value') %in% names(data))) != 3){
    stop('Required input: (1) location, (2) period and (3) value')
  } else {
    message(paste0('Save output data for period in wide format'))
  }

  # Reduce data according to selected spatial resolution
  if(is.null(locations)){ locations <- data$location }
  df.tmp <- data[data$location %in% locations, ]

  # Select only essential columns and remove duplicates ('Monding')
  if(is.null(cols)){
    v.cols <- c('location', 'period', 'value')
  } else {
    v.cols <- c('location', c(cols), 'period', 'value')
  }
  df.tmp <- df.tmp[, v.cols]
  df.tmp <- df.tmp[!duplicated(df.tmp), ]

  # Reshape data to wide format and finetune column names
  df.out <- stats::reshape(df.tmp, idvar = v.cols[1:(length(v.cols) - 2)],
                    v.names = 'value', timevar = 'period', direction = 'wide')
  names(df.out) <- gsub('value.', '', names(df.out))

  # Order data according to location
  df.out$location <- factor(df.out$location, unique(locations))
  df.out <- df.out[order(df.out$location), ]

  # Write data
  utils::write.table(df.out, row.names = F, sep = ';', file = fileout)
  message(paste0('---Output data saved in wide format---'))
}
