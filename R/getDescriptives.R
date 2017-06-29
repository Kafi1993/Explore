#' Explore
#'
#' Returns all the relevant statistical basics (number of NA, mean, median, standard deviation, minimum, maximum, kurtosis and skew) to get a quick overview of your data.
#'
#'  
#' 
#' @author Karl-Philipp Floesch, Joschka Cremers, Leonie Kotz
#' 
#'
#' @param rawdata data frame input, typically a psychological dataset.
#' @param output Should the output be a list or a dataframe? ("list", "dataframe", default is list)
#' 
#' 
#' @examples data("mtcars")
#' getDescriptives(mtcars, "list")
#' or
#' getDescriptives(mtcars, "dataframe")
#' 
#' @return A list or dataframe (with all the following outputs: number of NA, mean, median, standard deviation, minimum, maximum, kurtosis and skew)
#' @export 
getDescriptives <- function(rawdata, output = "list"){
  numericData <- extractNumeric(rawdata)
  
  li <- list()
  
  li$mean <- meanAll(numericData)
  li$median <- medianAll(numericData)
  li$sd <- sdAll(numericData)
  li$min <- minAll(numericData)
  li$max <- maxAll(numericData)
  li$kurtosis <- kurtosisAll(numericData)
  li$skew <- skewAll(numericData)
  li$na <- naCount(numericData)
  
  if (output == "list") dataOut <- li
  
  else if(output == "dataframe"){
    
   dataOut <-  data.frame(li[1:length(li)])
  }
  
  return(dataOut)
}






















