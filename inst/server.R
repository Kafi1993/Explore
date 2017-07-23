# server.R

library(shiny)
library(ggplot2)
library(psych)
library(colourpicker)
source("helpers.R")


shinyServer(function(input, output, session) {
  # **********************************************************************************************
  # Read in datafile =============================================================================
  
    uploadData <- reactive({
      
      inFile <- input$inFile 
      
      read.csv(inFile$datapath, header = input$head, sep = input$sep, stringsAsFactors = input$sASf,
               quote = input$quote, dec = input$dec, na.strings = input$miss)
      
    })
  
  # **********************************************************************************************
  # Showing uploadData =================================================================================
      
  output$datatable <- renderDataTable({
    
    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    
    
    uploadData()
      
    })
    
  # **********************************************************************************************
  # Descriptives Table ============================================================================  
  
  output$summary <- renderDataTable({

    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    

    list <- getDescriptives(uploadData())

    df <- data.frame("Item" = names(list$mean),"Mean" = list$mean, "SD" = list$sd,"Median" = list$median, "Min" = list$min,
               "Max" = list$max, "Skew" = list$skew, "Kurtosis" = list$kurtosis ,"Missings" = list$na)

  })
  
  # **********************************************************************************************
  # Graphics All Items ============================================================================
  
  numData <- reactive({
    
    extractNumeric(uploadData())
    
  })
  
  # Vector containing Item names
  RangeChoice <- reactive({
    
    colNum <- 1:ncol(numData())
    names(colNum) <- names(numData())
    colNum
    
  })
  
  
  ### Dynamic Column Range Panels
  
  output$RangeColSelect1 <- renderUI({
    
    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    
    selectInput("ColRange1", "Show Variables from:", choices = c("", RangeChoice()))
    
  })
  
  output$RangeColSelect2 <- renderUI({
    
    validate(
      need(input$inFile != "", " ")
    ) 
    
    selectInput("ColRange2", "to:", choices = c("", RangeChoice()))
    
  })
  
  
  ### Dynamic Plot Setting Panel (for vertical bars)
  
  output$VerticalSettings <- renderUI({
    
    radioButtons("VerticalType", "Should bars be grouped or stacked?", choices = c("Grouped bars", "Stacked bars"))
    
  })
  
  
  ### Subset uploadData with selected Columns
  
  dataForGraph <- reactive({

    minC <- as.integer(input$ColRange1)
    maxC <- as.integer(input$ColRange2)

    numData()[minC:maxC]

  })
  
  ### Dimensions of Plot for plotOutput
  
  PlotAllDims <- reactive({
    
    c(as.numeric(input$PlotAllHeight), as.numeric(input$PlotAllWidth))

  })
  
  
  observe({
    
    input$SizeReset
    updateTextInput(session, "PlotAllHeight", value = "400")
    updateTextInput(session, "PlotAllWidth", value = "1600")
    
  })


  ### Outout
  
  observe(
    output$plotAll <- renderPlot({
  
      if (is.null(input$inFile))
        return(NULL)
  
      validate(
        need(input$ColRange1, "Please choose a lower bound of columns to display"),
        need(input$ColRange2, "Please choose an upper bound of columns to display")
      )
      
      
      # Some parameters  
      col_n <- as.integer(input$ColRange2) # Number of allowed columns
      if (input$Rev_Col) ColDir <- -1 else ColDir <- 1 # Direction of color palette
      
      # Horizontal Bars
      if (input$PlotOrient == "horizontal") {
        
        if(input$Unit_PlotAll_horiz) FreqType <- position_stack else FreqType <- position_fill
      
        stackedBars(dataForGraph(), type = FreqType, n.col = col_n, palette = input$Col_PlotAll,
                    direction = ColDir)
        
      # Vertical Bars
      } else {
        
        if(input$VerticalType == "Stacked bars" && input$Unit_PlotAll_vert) FreqType <- position_stack()
        else if (input$VerticalType == "Stacked bars") FreqType <- position_fill()
        else if (input$VerticalType == "Grouped bars") FreqType <- position_dodge()
        
        parallelBars(dataForGraph(), position = FreqType, n.col = col_n, palette = input$Col_PlotAll,
                     direction = ColDir)
        
      }
  
    }, height = PlotAllDims()[1], width = PlotAllDims()[2]
    )
  )
  
  # **********************************************************************************************
  # Graphics Single Items =========================================================================
  
  ## Select Items ----------------------------
  
  ### Choices for Item Select Menu
  
  columnChoice <- reactive({

    colNum <- 1:ncol(uploadData())
    names(colNum) <- names(uploadData())

  })

  ### Dynamic Item Select Panel 1 
  
  output$ItemSelect1 <- renderUI({
    
    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    
    selectInput("PlotVar1", "Choose a variable to display", choices = c(Choose = "", columnChoice()))

  })
  
  ### Dynamic Item Select Panel  2
  
  output$ItemSelect2 <- renderUI({
    
    validate(
      need(input$inFile != "", "Please load a data set")
    ) 
    
    selectInput("PlotVar2", "Choose a variable to display", choices = c(Choose = "", columnChoice()))
    
  })
  
  
  ## Dynamic Options for different PlotTypes ---------------------------
  
  ### Dynamic Plot Type Panels 1 ----------------

  #### For Histogram
  
  output$hist1Breaks <- renderUI({
    
    validate(
      need(input$PlotVar1, "Please select a variable to show all options")
    )
    
    if (input$Package1) {
    
      numericInput("binwidth_Hist1", "Width of bars:", 
                   value = 1, min = 0, step = .1)
    
    } else
      
      numericInput("Breaks_Hist1", "Number of breaks:", 
                   value = nclass.Sturges(uploadData()[[input$PlotVar1]]), min = 1)
    
  }) 
  
  
  #### For Scatterplot
  # None
  

  ### For Boxplot
  
  output$viewMeanSpecs1_1 <- renderUI({
    
    if(input$showMean1) {
      
      colourInput("Col_BoxMean1", "Choose a color", value = "red")
      
    } else return(NULL)
    
  })
  
  
  output$viewMeanSpecs1_2 <- renderUI({
    
    if(input$showMean1) {
      
      numericInput("Box_pShape1", "Change shape of points (from R graphics)", value = 15, min = 0, max = 25)
      
    } else return(NULL)
    
  })
  
  ### Dynamic Plot Type Panels 2 ---------------
  
  #### For Histogram
  
  output$hist2Breaks <- renderUI({
    
    validate(
      need(input$PlotVar2, "Please select a variable to show all options")
    )
    
    if (input$Package2) {
      
      numericInput("binwidth_Hist2", "Width of bars:", 
                   value = 1, min = 0, step = .1)
      
    } else
      
      numericInput("Breaks_Hist2", "Number of breaks:", 
                   value = nclass.Sturges(uploadData()[[input$PlotVar2]]), min = 1)
    
  }) 
  
  
  
  #### For Scatterplot
  # None
  
  
  ### For Boxplot
  
  output$viewMeanSpecs2_1 <- renderUI({
    
    if(input$showMean2) {
      
      colourInput("Col_BoxMean2", "Choose a color", value = "red")
      
    } else return(NULL)
    
  })
  
  
  output$viewMeanSpecs2_2 <- renderUI({
    
    if(input$showMean2) {
      
      numericInput("Box_pShape2", "Change shape of points (from R graphics)", value = 15, min = 0, max = 25)
      
    } else return(NULL)
    
  })
  
  ## Create Plots =========================================================
  
  ### Some Specs for adding mean to Boxplot
  showMean <- reactive({
    
    c(input$showMean1, input$showMean2)
    
  })
  
  horiz <- reactive({
    
    c(input$horiz1, input$horiz2)
    
  })
  
  ### Define uploadData to plot for R base
  plotData1 <- reactive({
    
   uploadData()[[input$PlotVar1]]

  })
  
  plotData2 <- reactive({
    
    uploadData()[[input$PlotVar2]]
    
  })
  
  ### Plot 1 ------------------------------------------------------------------
  
  output$dataPlot1 <- renderPlot({

    if (is.null(input$inFile))
      return(NULL)

    validate(
      need(input$PlotVar1, "Please select a variable to plot")
    )
    
    # R base Graphics ----------------------
    if (input$Package1 == FALSE){
      
      # Histogram -------------
      if (input$PlotType1 == "hist") {
      
        hist(plotData1(), main = paste("Histogram of Variable", input$PlotVar1), freq = input$Unit_Hist1, 
             xlab = input$PlotVar1, col = input$Col_Hist1, breaks = input$Breaks_Hist1)
      
      # Scatterplot -------------------
      } else if (input$PlotType1 == "scatter") {
        
        # Jittered uploadData?
        if (input$jitter1 == FALSE) {
        
          plot(plotData1(), main = paste("Scatterplot of Variable", input$PlotVar1), pch = input$pShape1,
               ylab = input$PlotVar1, col = input$Col_Scatter1)
        
        } else {
          
          plot(jitter(plotData1()), main = paste("Scatterplot of Variable", input$PlotVar1), pch = input$pShape1,
               ylab = input$PlotVar1, col = input$Col_Scatter1)
          
        }
      
      # Boxplot -------------------
      } else if (input$PlotType1 == "box") {
        
        # Add mean?
        if (showMean()[1] == TRUE) {
          
          boxplot(plotData1(), main = paste("Scatterplot of Variable", input$PlotVar1), 
               xlab = input$PlotVar1, col = input$Col_Box1, 
               notch = input$notched1, horizontal = input$horiz1)

          if (horiz()[1] == TRUE) {
            
            points(mean(plotData1()), y = 1, col = input$Col_BoxMean1, pch = input$Box_pShape1, cex = 1.5)
            
          } else {
            
            points(mean(plotData1()), col = input$Col_BoxMean1, pch = input$Box_pShape1, cex = 1.5)
            
          }
          
        } else {
          
          boxplot(plotData1(), main = paste("Scatterplot of Variable", input$PlotVar1), 
                  xlab = input$PlotVar1, col = input$Col_Box1, 
                  notch = input$notched1, horizontal = input$horiz1)
          
        }
      }
    
      
    # ggplot 2 Graphics -------------------- 
    } else if (input$Package1 == TRUE) {
      
      # Histogram -------------
      if (input$PlotType1 == "hist") {
        
        # Plot frequency
        if (input$Unit_Hist1 == TRUE) {
          
          ggplot(data = uploadData(), aes(x = uploadData()[[input$PlotVar1]])) + 
            geom_histogram(binwidth = input$binwidth_Hist1, fill = input$Col_Hist1) +
            xlab(input$PlotVar1) + ggtitle(paste("Histogram of Variable", input$PlotVar1)) +
            theme(plot.title = element_text(size = 16, hjust = .5))
        
        # Plot density
        } else {
          
          ggplot(data = uploadData(), aes(x = uploadData()[[input$PlotVar1]])) + 
            geom_histogram(aes( y = ..density..), binwidth = input$binwidth_Hist1, fill = input$Col_Hist1) +
            xlab(input$PlotVar1) + ggtitle(paste("Histogram of Variable", input$PlotVar1)) +
            theme(plot.title = element_text(size = 16, hjust = .5))
        }
      
      # Scatterplot --------------------- 
      } else if (input$PlotType1 == "scatter") {
        
        # Jittered uploadData?
        if (input$jitter1 == FALSE) {
          
          ggplot(data = uploadData(), aes(y = seq(1,length(uploadData()[[input$PlotVar1]])), x = uploadData()[[input$PlotVar1]])) +
            geom_point(color = input$Col_Scatter1, shape = input$pShape1) +
            xlab(input$PlotVar1) + ylab("Index") + ggtitle(paste("Scatterplot of Variable", input$PlotVar1)) +
            theme(plot.title = element_text(size = 16, hjust = .5))
        
        } else {
          
          ggplot(data = uploadData(), aes(y = seq(1,length(uploadData()[[input$PlotVar1]])), x = uploadData()[[input$PlotVar1]])) +
            geom_jitter(color = input$Col_Scatter1, shape = input$pShape1) +
            xlab(input$PlotVar1) + ylab("Index") + ggtitle(paste("Scatterplot of Variable", input$PlotVar1)) +
            theme(plot.title = element_text(size = 16, hjust = .5))
        }
      
      # Boxplot -------------------
      } else if (input$PlotType1 == "box") {
        
        # Add mean?
        if (showMean()[1] == TRUE) {
            
            ggBoxplot1 <- ggplot(uploadData(), aes(x = "", y = uploadData()[[input$PlotVar1]])) + 
              geom_boxplot(fill = input$Col_Box1, notch = input$notched1, width = .3) +
              ylab(input$PlotVar1) + ylab("") + ggtitle(paste("Boxplot of Variable", input$PlotVar1)) +
              theme(plot.title = element_text(size = 16, hjust = .5))  +
              
              stat_summary(fun.y = mean, geom = "point", size = 4, shape =input$Box_pShape1, 
                           color = input$Col_BoxMean1)
          
        } else {
          
          ggBoxplot1 <- ggplot(uploadData(), aes(x = "", y = uploadData()[[input$PlotVar1]])) + 
            geom_boxplot(fill = input$Col_Box1, notch = input$notched1, width = .3) +
            ylab(input$PlotVar1) + ylab(" ") + ggtitle(paste("Boxplot of Variable", input$PlotVar1)) +
            theme(plot.title = element_text(size = 16, hjust = .5))
          
        }
        
        if (input$horiz1 == TRUE) ggBoxplot1 + coord_flip()
          else ggBoxplot1 
        
      }
    }
  })
  
 
  ### Plot 2 --------------------------------------------
  
  output$dataPlot2 <- renderPlot({
    
    if (is.null(input$inFile))
      return(NULL)
    
    validate(
      need(input$PlotVar2, "Please select a variable to plot")
    )
    
    # R base Graphics ----------------------
    if (input$Package2 == FALSE){
      
      # Histogram -------------
      if (input$PlotType2 == "hist") {
        
        hist(plotData2(), main = paste("Histogram of Variable", input$PlotVar2), freq = input$Unit_Hist2, 
             xlab = input$PlotVar2, col = input$Col_Hist2, breaks = input$Breaks_Hist2)
        
        # Scatterplot -------------------
      } else if (input$PlotType2 == "scatter") {
        
        # Jittered uploadData?
        if (input$jitter2 == FALSE) {
          
          plot(plotData2(), main = paste("Scatterplot of Variable", input$PlotVar2), pch = input$pShape2,
               ylab = input$PlotVar2, col = input$Col_Scatter2)
          
        } else {
          
          plot(jitter(plotData2()), main = paste("Scatterplot of Variable", input$PlotVar2), pch = input$pShape2,
               ylab = input$PlotVar2, col = input$Col_Scatter2)
          
        }
        
        # Boxplot -------------------
      } else if (input$PlotType2 == "box") {
        
        # Add mean?
        if (showMean()[2] == TRUE) {
          
          boxplot(plotData2(), main = paste("Scatterplot of Variable", input$PlotVar2), 
                  xlab = input$PlotVar2, col = input$Col_Box2, 
                  notch = input$notched2, horizontal = input$horiz2)
          
          if (horiz()[2] == TRUE) {
            
            points(mean(plotData2()), y = 1, col = input$Col_BoxMean2, pch = input$Box_pShape2, cex = 1.5)
            
          } else {
            
            points(mean(plotData2()), col = input$Col_BoxMean2, pch = input$Box_pShape2, cex = 1.5)
            
          }
          
        } else {
          
          boxplot(plotData2(), main = paste("Scatterplot of Variable", input$PlotVar2), 
                  xlab = input$PlotVar2, col = input$Col_Box2, 
                  notch = input$notched2, horizontal = input$horiz2)
          
        }
      }
      
      
    # ggplot 2 Graphics -------------------- 
    } else if (input$Package2 == TRUE) {
      
      # Histogram -------------
      if (input$PlotType2 == "hist") {
        
        # Plot frequency
        if (input$Unit_Hist2 == TRUE) {
          
          ggplot(data = uploadData(), aes(x = uploadData()[[input$PlotVar2]])) + 
            geom_histogram(binwidth = input$binwidth_Hist2, fill = input$Col_Hist2) +
            xlab(input$PlotVar2) + ggtitle(paste("Histogram of Variable", input$PlotVar2)) +
            theme(plot.title = element_text(size = 16, hjust = .5))
          
          # Plot density
        } else {
          
          ggplot(data = uploadData(), aes(x = uploadData()[[input$PlotVar2]])) + 
            geom_histogram(aes( y = ..density..), binwidth = input$binwidth_Hist2, fill = input$Col_Hist2) +
            xlab(input$PlotVar2) + ggtitle(paste("Histogram of Variable", input$PlotVar2)) +
            theme(plot.title = element_text(size = 16, hjust = .5))
        }
        
        # Scatterplot --------------------- 
      } else if (input$PlotType2 == "scatter") {
        
        # Jittered uploadData?
        if (input$jitter2 == FALSE) {
          
          ggplot(data = uploadData(), aes(y = seq(1,length(uploadData()[[input$PlotVar2]])), x = uploadData()[[input$PlotVar2]])) +
            geom_point(color = input$Col_Scatter2, shape = input$pShape2) +
            xlab(input$PlotVar2) + ylab("Index") + ggtitle(paste("Scatterplot of Variable", input$PlotVar2)) +
            theme(plot.title = element_text(size = 16, hjust = .5))
          
        } else {
          
          ggplot(data = uploadData(), aes(y = seq(1,length(uploadData()[[input$PlotVar2]])), x = uploadData()[[input$PlotVar2]])) +
            geom_jitter(color = input$Col_Scatter2, shape = input$pShape2) +
            xlab(input$PlotVar2) + ylab("Index") + ggtitle(paste("Scatterplot of Variable", input$PlotVar2)) +
            theme(plot.title = element_text(size = 16, hjust = .5))
        }
        
        # Boxplot -------------------
      } else if (input$PlotType2 == "box") {
        
        # Add mean?
        if (showMean()[2] == TRUE) {
          
          ggBoxplot2 <- ggplot(data = uploadData(), aes(x = "", y = uploadData()[[input$PlotVar2]])) + 
            geom_boxplot(fill = input$Col_Box2, notch = input$notched2) +
            ylab(input$PlotVar2) + ylab("") + ggtitle(paste("Boxplot of Variable", input$PlotVar2)) +
            theme(plot.title = element_text(size = 16, hjust = .3))  +
            
            stat_summary(fun.y = mean, geom = "point", size = 4, shape =input$Box_pShape2, 
                         color = input$Col_BoxMean2)
          
        } else {
          
          ggBoxplot2 <- ggplot(data = uploadData(), aes(x = "", y = uploadData()[[input$PlotVar2]])) + 
            geom_boxplot(fill = input$Col_Box2, notch = input$notched2) +
            ylab(input$PlotVar2) + ylab(" ") + ggtitle(paste("Boxplot of Variable", input$PlotVar2)) +
            theme(plot.title = element_text(size = 16, hjust = .3))
          
        }
        
        if (input$horiz2 == TRUE) ggBoxplot2 + coord_flip()
          else ggBoxplot2 
        
      }
    }
    
  })
})  


