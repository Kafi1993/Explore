#' Parallel Barchart
#' 
#' Draw a parallel barchart for a general overview of items from a dataset.
#' 
#'@author Joschka Cremers
#'@param dframe data.frame to be analyzed, typically psychological.
#'@param pattern optional, selects those items which match the specified pattern. May also be column-index-values.
#'@param n.col specify the maximum number of columns to be drawn.
#'@param palette specify a color-palette from RColorBrewer. Advised to use bidirectional (e.g. "RdBu") and categorical (e.g. "Set3") color-schemes if appropriate.
#'@param direction set to -1 to reverse the color-scheme.
#'
#'@usage parallelBars(n.col = 15, palette = "Blues", direction = 1)
#'
#'@return Plots parallel bars in a chart with a maximum of 15 items at the x-axis and frequencies on y-axis.
#' 
#'@examples
#' # Load big5-data from Explore-package
#' data("big5")
#' 
#' # Draw only Extraversion-items in graph
#' parallelBars(big5, pattern = "^E", palette = "YlGn")
#'
#'@export
parallelBars <- function(dframe, pattern = "", n.col = 15, palette = "Blues", direction = 1){
  
  dframe <- divideByPattern(dframe = dframe, pattern = pattern, n.col = n.col)
  dframe <- convertData(dframe)
  
  # ind codes for the items - shown on x-axis
  # values contains the responses of participants/values of the items
  ggplot(data = dframe, mapping = aes(x = ind, fill = values)) +
    geom_bar(position = "dodge") +
    labs(x = " ") +
    scale_fill_brewer(palette = palette, direction = direction) +
    guides(fill = guide_legend(reverse = T))
}

