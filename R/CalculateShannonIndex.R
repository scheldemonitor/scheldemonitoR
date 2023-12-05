#' Calculate Shannon index
#'
#' `calculateShannonIndex()` determines the Shannon index for a vector with values greater than 0. Value indicates abundance.
#' This can be e.g total number, density or coverage.
#'
#' @param value A column of a dataframe or a list of values.
#'
#' @return A double. The calculated Shannon Index for that set of values.
#' @export
#'
#' @examples
#' # example with dataframe
#' # create a dataframe with species, and their densities:
#' df <- data.frame(species = c("s1", "s2", "s3"), value = 1:3)
#'
#' # calculate index for the densities (value) in my dataset
#' shannonIndex <- scheldemonitoR::calculateShannonIndex(value = df$value)
#'
#' # example with a list
#' density_values <- c(1,2,3,4)
#' shannonIndex <- scheldemonitoR::calculateShannonIndex(density_values)
#'
#' # or directly
#' shannonIndex <- scheldemonitoR::calculateShannonIndex(c(1,2,3,4))
calculateShannonIndex <- function(value){
  out <- NULL
  # remove NA and select values > 0
  value <- value[value > 0 & !is.na(value)]
  sum_val <- sum(value)

  if(sum_val == 0) {
    print("Warning: cannot calculate shannon index for just zeros")
    shannon_index <- NA
  } else {
    pi <- (value/sum_val)
    out <- data.frame(value, pi)
    # sum(pi) # should be 1
    out$lnpi <- log(out$pi)
    out$pilnpi <- out$lnpi * out$pi
    shannon_index <- -sum(out$pilnpi)
  }

  return(shannon_index)
}
