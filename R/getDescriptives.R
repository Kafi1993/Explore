#' Explore 
#'  
#' Kurzbeschreibung
#'  
#' ausführlichere Beschreibung
#' @author Karl-Philipp Flösch, Joschka Cremers, Leonie Kotz
#' 
#'
#' @param rawdata data.frame input, typically a psychological dataset.
#' 
#' 
#' @examples 
#' 
#' @return list 
#' @export 
getDescriptives <- function(rawdata){
  numericData <- extractNumeric(rawdata)
  
  li <- list()
  
  li$na <- naCount(numericData)
  li$mean <- meanAll(numericData)
  li$median <- medianAll(numericData)
  li$sd <- sdAll(numericData)
  li$min <- minAll(numericData)
  li$max <- maxAll(numericData)
  li$kurtosis <- kurtosisAll(numericData)
  li$skew <- skewAll(numericData)
  
  return(li)
}

