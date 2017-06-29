# server.R

library(shiny)
library(ggplot2)
library(psych)
source("helpers.R")


shinyServer(function(input, output) {
  
  # Read in datafile ----------------
  
    data <- reactive({
      
      inFile <- input$inFile 
      
      read.csv(inFile$datapath, header = input$head, sep = input$sep, stringsAsFactors = input$sASf,
               quote = input$quote, dec = input$dec, na.strings = input$miss)
      
    })
  
  
  # Showing data ----------------------
      
  output$datatable <- renderDataTable({
    
    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    
    
    data()
      
    })
    
  
  # Descriptives Table --------------------  
  
  output$summary <- renderDataTable({

    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    

    list <- getDescriptives(data())

    df <- data.frame("Item" = names(list$mean),"Mean" = list$mean, "SD" = list$sd,"Median" = list$median, "Min" = list$min,
               "Max" = list$max, "Skew" = list$skew, "Kurtosis" = list$kurtosis ,"Missings" = list$na)

  })
  
  
  # Graphics All Items ---------------------
  
  ### Dynamic Column Range Panel 
  
  output$RangeColSelect <- renderUI({
    
    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    
    numData <- extractNumeric(data())  
    
    sliderInput("ColRange", "Range of columns", min = 1, max = ncol(numData), 
                value = c(1, ncol(numData)), step = 1)
    
  })
  
  
  ### Subset data with selected Columns

  dataForGraph <- reactive({

    minC <- input$ColRange[1]
    maxC <- input$ColRange[2]

    data()[minC:maxC]


  })

    
  ### Outout
    
  output$graphics <- renderPlot({

    if (is.null(input$inFile))
      return(NULL)
    
    validate(
      need(input$ColRange, "Please wait a moment")
    )
    
    stackedBars(dataForGraph())
      
      
    })
  
  # Graphics Single Item ---------------------
  
  ### Dynamic Column Range Panel 1 
  
  output$ItemSelect1 <- renderUI({
    
    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    
    selectInput("SingVar", "Choose a variable to display", choices = colnames(data()))
   
  })
  
  
  ### Dynamic Column Range Panel 2
  
  output$ItemSelect2 <- renderUI({
    
    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    
    selectInput("SingVar", "Choose a variable to display", choices = colnames(data()))
    
  })
  
  
  }
)  









