
# Hilfs-FUnktionen

extractNumeric <- function(dframe){
  dframe[sapply(dframe, is.numeric)] 
}



naCount <- function(dframe){
  sapply(dframe, function(column){sum(is.na(column))})
}



meanAll <- function(dframe){
  round(sapply(dframe, mean, na.rm = T), digits = 2)
}



medianAll <- function(dframe){
  round(sapply(dframe, median, na.rm = T), digits = 2)
} 



sdAll <- function(dframe){
  round(sapply(dframe, sd, na.rm = T), digits = 2)
}


minAll <- function(dframe){
  round(sapply(dframe, min, na.rm = T), digits = 2)
}


maxAll <- function(dframe){
  round(sapply(dframe, max, na.rm = T), digits = 2)
}


skewAll <- function(dframe){
  round(sapply(dframe, skew, na.rm = T), digits = 2)
}


kurtosisAll <- function(dframe){
  round(sapply(dframe, kurtosi, na.rm = T), digits = 2)
}

# Hauptfuntktion
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


###changes structure of the dataframe to make it usable for ggplot

convertData <- function(rawdata){
  stacked_data <- stack(rawdata)
  # converts columns to factors, so that it can be used by ggplot as a categorical variable
  as.data.frame(sapply(stacked_data, as.factor))
}

# Plot-function

stackedBars <- function(dataframe){
  numericData <- extractNumeric(dataframe)
  conv_numericData <- convertData(numericData)
  
  # ind codes for the items - shown on x-axis
  # values contains the responses of participants/values of the items
  ggplot(data = conv_numericData, mapping = aes(x = ind, fill = values)) +
    geom_bar(position = "fill") + #oder dodge
    coord_flip()
}
