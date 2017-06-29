#' Convert Data
#' 
#' Converts the structure of a data.frame to make it suitable for ggplot-usage in stackedBars and parallelBars functions.
#' 
#'@author Joschka Cremers
#'@param dframe data.frame to be converted, typically psychological, delivered by superordinate functions.
#'
#'@return Returns a two column-dataframe with stacked item-values in column 1 and coding for original item-affiliation in column 2.
#' 
#'@export
convertData <- function(dframe){
  stacked_data <- stack(dframe)
  # converts columns to factors, so that it can be used by ggplot as a categorical variable
  stacked_data[,1] <- as.factor(stacked_data[,1])
  return(stacked_data)
}