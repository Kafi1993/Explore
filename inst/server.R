# server.R

library(ggplot2)
library(psych)

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
  dataframe <- convertData(dataframe)
  # ind codes for the items - shown on x-axis
  # values contains the responses of participants/values of the items
  ggplot(data = dataframe, mapping = aes(x = ind, fill = values)) +
    geom_bar(position = "fill") + #oder dodge
    coord_flip()
}



shinyServer(function(input, output) {
    output$datatable <- renderDataTable({
     
      inFile <- input$inFile
      
      if (is.null(inFile))
        return(NULL)

      read.csv(inFile$datapath, header = input$head, sep = input$sep, 
               quote = input$quote, dec = input$dec, na.strings = input$miss)
      
    })
    
    
    output$summary <- renderDataTable({

      inFile <- input$inFile

      if (is.null(inFile))
        return(NULL)

      dataset <-  read.csv(inFile$datapath, header = input$head, sep = input$sep,
                          quote = input$quote, dec = input$dec, na.strings = input$miss)

      list <- getDescriptives(dataset)

      df <- data.frame("Item" = names(list$mean),"Mean" = list$mean, "SD" = list$sd,"Median" = list$median, "Min" = list$min,
                 "Max" = list$max, "Skew" = list$skew, "Kurtosis" = list$kurtosis ,"Missings" = list$na)

    })
    
    output$graphics <- renderPlot({
      
      inFile <- input$inFile
      
      if (is.null(inFile))
        return(NULL)
      
      dataset <-  read.csv(inFile$datapath, header = input$head, sep = input$sep,
                           quote = input$quote, dec = input$dec, na.strings = input$miss)
      
      # Plot-function
      
      stackedBars(dataset)
      
    })
  }
)  