<<<<<<< HEAD
#' Get Descriptives
=======
#' Explore
>>>>>>> bafa8349a523c3da60a20d01cbc15091a5d23849
#'
#' Returns all the relevant statistical basics (number of NA, mean, median, standard deviation, minimum, maximum, kurtosis and skew) to get a quick overview of your data.
#'
#'  
#' 
#' @author Leonie Kotz
#' 
#'
#' @param rawdata data frame input, typically a psychological dataset.
#' @param output Should the output be a dataframe or a list? ("dataframe", "list", default is dataframe)
#' 
#' 
#' @examples # Load the big5-dataset from the Explore package
#' data("big5")
#' # if you want to get the results as a dataframe
#' getDescriptives(big5, "dataframe")
#' # or if you want to get them as a list
#' getDescriptives(big5, "list")
#' 
#' @return A dataframe or list (with all the following outputs: number of NA, mean, median, standard deviation, minimum, maximum, kurtosis and skew)
#' @export 
getDescriptives <- function(rawdata, output = "dataframe"){
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
  
  if (output == "dataframe") dataOut <- data.frame(li[1:length(li)])
  
  else if(output == "list"){
    
   dataOut <-  li
  }
  
  return(dataOut)
}






















