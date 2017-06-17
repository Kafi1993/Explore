#' Stacked Barcharts
#' 
#' Draw a stacked barchart for a general overview of items from a dataset.
#' 
#'@author Joschka Cremers
#'@param dataframe The (typically psychological) dataframe to be analyized.
#' 
#'@return Plots a stacked barchart with items a the y-axis and relative frequencies on x-axis.
#' 
#'@example
#' data("big5_e")
#' stackedBars("big5_e")

library(ggplot2)

###changes structure of the dataframe to make it usable for ggplot
#'@export
convertData <- function(rawdata){
  stacked_data <- stack(rawdata)
  # converts columns to factors, so that it can be used by ggplot as a categorical variable
  as.data.frame(sapply(stacked_data, as.factor))
}

# Plot-function
#'@export
stackedBars <- function(dataframe){
  dataframe <- convertData(dataframe)
  # ind codes for the items - shown on x-axis
  # values contains the responses of participants/values of the items
  ggplot(data = dataframe, mapping = aes(x = ind, fill = values)) +
    geom_bar(position = "fill") + #oder dodge
    coord_flip()
}

