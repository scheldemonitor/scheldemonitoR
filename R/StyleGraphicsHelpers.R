# some functions used to style plots accordung to SM aesthetics.

#' Apply theme of 'Schelde in beeld' (sib)
#'
#' Apply theme of 'Schelde in beeld' (sib)
#'
#' @return plot object.
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
#' plotsib <- plot + themeSib()
#' plotsib
themeSib <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = 'black'),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(colour = 'black'))
}

#' Export plot as an image (png)
#'
#' Export plot as an image (png)
#'
#' @param p Plot object. For example the result of [scheldemonitoR::exploreDataStation()].
#' @param fname Character. Filename.
#' @param w Numeric. Desired width of the plot. Default = `16`.
#' @param h Numeric. Desired height of the plot. Default = `10`
#'
#' @return A plot saved to current directory.
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
#' exportPlot(plot, "densityplot.png") # save to current directory
#'
#' # save to specific directory
#' # mydirectory <- "/Mydirectory"
#' # exportPlot(file.path(mydirectory, "densityplot.png"))
exportPlot <- function(p, fname, w = 16, h = 10) {
  ggplot2::ggsave(plot = p,
                  filename = fname,
                  device = "png",
                  width = w,
                  height = h,
                  units = "cm",
                  dpi = 300)
}

#' Change first charachter of a string to uppercase
#'
#' Change first charachter of a string to uppercase
#'
#' @param x character.
#'
#' @return character.
#' @export
#'
#' @examples
#' firstUp("scheldemonitor")
firstUp <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Add grey squares in the background, representing a 6yr period
#'
#' Add grey squares in the background, representing a 6yr period. For easier interpretation of the graphic.
#'
#' @param p Plot object. For example the result of [scheldemonitoR::exploreDataStation()].
#' @param maxyr Numeric. End of the 6yr period. Default `2021.5`.
#' @param nperiods Numeric. Number of periods. Default `3`.
#' @param shift_period Numeric. See `Details`.
#'
#' @details For non-breeding birds we have data where a "year" runs
#' from 1jul till 1jul. The T2021 period was for nonbreeding birds was defined
#' as the period 2015-2020 instead of 2016-2021. Therefore the background has to
#' be shifted with 1year.
#'
#' @return A plot object.
#' @export
#'
#' @examples
#' # make a list of the dataset ids
#' imis_datasets <- c(1073, 5191)
#'
#' # download data
#' biotic_data_imis <- importBioticData(imis_datasets, start = 1980, end = 2022, source = 'imis')
#'
#' # correct datetime colum name
#' colnames(biotic_data_imis)[colnames(biotic_data_imis) == "observationdate"] <- "datetime"
#'
#' # make temporal plot
#' plot <- exploreDataStation(biotic_data_imis, "Densiteit van zooplankton (ind/m3)", "line")
#' # add background
#' p <- plot %>% addRectangle()
# addRectangle <- function(p, maxyr= 2021.5, nperiods=3, shift_period = 0) {
#   maxyr = maxyr - shift_period
#   for(i in 1:nperiods) {
#     p <- p %>%
#       append_layers(ggplot2::geom_rect(data = data.frame(m = 1),
#                                        fill = 'grey80', alpha = 0.5,
#                                        xmin = maxyr - 12*i + 6,
#                                        xmax = maxyr - 12*i + 12,
#                                        ymin = -Inf, ymax = Inf),
#                     position = "bottom")
#     }
#   return(p)
# }

# helper function ------------------------------------------------------------------------------
# background functions used in addRectangle()
# append_layers
# edit_layers
#
# copied from:
# https://github.com/aphalo/gginnards/blob/master/R/layer-manip.R
# append_layers <- function(x, object, position = "top") {
#   is_list_of_layers <- function(list) {
#     n <- length(list)
#     if (n == 0) return(TRUE)
#     are.layers <- sapply(object, methods::is, class2 = "Layer")
#     if (all(are.layers)) return(TRUE)
#     are.sf.layers <- sapply(object, methods::is, class2 = "LayerSF")
#     are.sf.coords <- sapply(object, methods::is, class2 = "CoordSf")
#     all(are.layers | are.sf.layers | are.sf.coords)
#   }
#   stopifnot(ggplot2::is.ggplot(x))
#   stopifnot(methods::is(object, "Layer") ||
#               is.list(object) && is_list_of_layers(object))
#   z <- x + object
#   if (length(z$layers) > length(x$layers) && position != "top") {
#     z <- edit_layers(x = z,
#                      match_type = NULL,
#                      position = position,
#                      idx = (length(x$layers) + 1):length(z$layers),
#                      action = "move")
#   }
#   z
# }
# edit_layers <- function(x, match_type = NULL, position = 0L, idx = NULL, action) {
#   stopifnot(ggplot2::is.ggplot(x))
#   stopifnot(xor(is.null(match_type), is.null(idx)))
#   if (length(position) > 1) {
#     warning("'position' is not vectorized, using 'position[1]'")
#     position <- position[1]
#   }
#   if (is.numeric(idx)) {
#     # Convert into logical vector---equivalent to the inverse of which()
#     idx <- seq_len(length(x$layers)) %in% as.integer(idx)
#   }
#   if (is.null(idx)) {
#     # Check what to search for
#     known_fields <- c("geom", "stat", "position")
#     matched_field <-
#       known_fields[sapply(known_fields, grepl, x = tolower(match_type))]
#     if (length(matched_field) == 0L) {
#       stop("Argument '", match_type,
#            "' not in supported fields: ", known_fields, ".")
#     }
#     # Find layers that match the requested type.
#     idx <- sapply(x$layers,
#                   function(y) {
#                     class(y[[matched_field]])[1] == match_type
#                   })
#   }
#   if (any(is.na(idx))) {
#     idx <- logical()
#   }
#   if (action == "delete") {
#     if (sum(idx) > 0L) {
#       # Delete the layers.
#       x$layers[idx] <- NULL
#     }
#   } else if ((action == "move")) {
#     if (sum(idx) > 0L) {
#       # Move the layers.
#       if (position == "top") {
#         x$layers <- c(x$layers[!idx], x$layers[idx])
#       } else if (position == "bottom") {
#         x$layers <- c(x$layers[idx], x$layers[!idx])
#       } else if (is.integer(position) && !is.na(position)) {
#         # We avoid position overflow and underflow
#         # We use only the topmost position
#         position <- max(0L, position)
#         position <- min(position, length(x$layers))
#         x$layers <- append(x$layers[!idx], x$layers[idx],
#                            ifelse(position > length(x$layers[!idx]),
#                                   length(x$layers[!idx]),
#                                   position))
#       } else {
#         stop("Position must be one of 'top' or 'bottom' or a positive integer.")
#       }
#     }
#   } else if (action == "which") {
#     x <- which(idx)
#   }
#   x
# }
