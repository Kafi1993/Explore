# ui.R

shinyUI(navbarPage(strong("Data ExploreR"), 
                   
  # Panel Data Import -----------------------
  
  tabPanel("Data Import",
    
    titlePanel("ExploreR Data Import"),
    
    sidebarLayout(position = "left",
      sidebarPanel(width = 3,
      
        helpText("Load in your datafile and specify its properties."),
        
        fileInput("inFile", "Choose File"),
        
        checkboxInput("head", strong("Header"), TRUE),
        
        checkboxInput("sASf", strong("Strings as Factors"), FALSE),
        
        selectInput("sep", "Data seperator",
                    choices = c(Tab = '\t', Comma = ',', Semicolon = ';'),
                    selected = ';'),
        
        selectInput("dec", "Decimal seperator",
                    choices = c(Dot = '.', Comma = ','),
                    selected = '.'),
        
        selectInput("quote", "Quote",
                    choices = c('None' = ' ',
                                'Single Quote' = "'",
                                'Double Quote' = '"'),
                    selected = '"'),
        
        textInput("miss", "Missing data",
                  value = "NA", width = "50%")
        
      ),
  
      mainPanel(dataTableOutput('datatable') 
      )
      
    )
    
  ),
  
  # Panel Data Summary ------------
  
  tabPanel("Data Summary",

    
    mainPanel(h2("ExploreR Data Summary"),
              br(),
              dataTableOutput('summary'))

  ),
  
  
  # Menu Graphics --------------------
  
  navbarMenu("Graphics",
             
    ### Panel All Items
    
    tabPanel("All Items",
        fluidPage(
           h2("ExploreR Graphics"),
           br(),
           fluidRow(
             column(2,
                    h4(strong("Subset data")),
                    br(),
                    uiOutput('RangeColSelect1'),
                    
                    uiOutput('RangeColSelect2')),
             
             #Plot Size
             column(2,
                    h4(strong("Plot Size")),
                    br(),
                    textInput("PlotAllWidth", "Width of the displayed plot (in px):",
                                value = "1600", width = "90%"),
                    
                    textInput("PlotAllHeight", "Height of the displayed plot (in px):",
                                value = "400", width = "90%"),
                    
                    actionButton("SizeReset", "Reset plot size")),
             
             #Plot Settings
             column(3,
                    h4(strong("Plot Settings")),
                    br(),
                    radioButtons("PlotOrient", "How should the bars be oriented?", 
                                 choices = c("Vertical bars" = "vertical",
                                             "Horizontal bars" = "horizontal")),
                    
                    conditionalPanel(
                      condition = "input.PlotOrient == 'vertical'",
                      uiOutput('VerticalSettings')),
                    
                    conditionalPanel(
                      condition = "input.PlotOrient == 'vertical' && input.VerticalType == 'Stacked bars'",
                      checkboxInput("Unit_PlotAll_vert", "Plot absolute Frequency", value = FALSE)),
                    
                    conditionalPanel(
                      condition = "input.PlotOrient == 'horizontal'",
                      checkboxInput("Unit_PlotAll_horiz", "Plot absolute Frequency", value = FALSE)),
                    
                    selectInput("Col_PlotAll", "Choose a color palette", 
                                choices = c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", 
                                            "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", 
                                            "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")),
             
                    checkboxInput("Rev_Col", "Reverse order of color palette", value = FALSE)),
             
             #Info
             column(3,
                    h4(strong("Info")),
                    p("Using this tool you can select a range of columns (variables/items) from your 
                      dataset (at least two) and display them at once as barplot. Only numerical variables 
                      will be provided for selection. It's appropriate to use only variables with a
                      restricted numerical variability (e.g. a rating scale) as it is intended for
                      barplots.", style = "font-si16pt"))
            
             
           ),
           hr(),
           plotOutput('plotAll')
        )

    ),
    
    ### Panel Single Items
    
    tabPanel("Single Items",
      fluidPage(
         h2("ExploreR Graphics"),
         br(),
         fluidRow(
           column(2,
                  h4(strong("Select Item 1")),
                  br(),
                  uiOutput('ItemSelect1'),
           
                  # Choose Plot and Package 1
                  checkboxInput("Package1", "Use ggplot2 graphics"),
                  radioButtons("PlotType1", "Which type of plot shoud be displayed?", 
                              choices = c("Histogram" = "hist",
                                          "Scatterplot" = "scatter",
                                          "Boxplot" = "box"), selected = NULL)
                  
           ),
           
           # Plot Settings 1
           column(3, 
                  br(),
                  br(),
                  h4("Plot Settings"),
                  
                  # Histogram
                  conditionalPanel(
                    condition = "input.PlotType1 == 'hist'",
                    colourInput("Col_Hist1", "Choose a color", value = "black"),
                    uiOutput('hist1Breaks'),
                    checkboxInput("Unit_Hist1", "Plot absolute Frequency", value = TRUE)),
                  
                  # Scatterplot
                  conditionalPanel(
                    condition = "input.PlotType1 == 'scatter'",
                    colourInput("Col_Scatter1", "Choose a color", value = "black"),
                    numericInput("pShape1", "Change shape of points (from R graphics)", value = 20, min = 0, max = 25),
                    checkboxInput("jitter1", "Add noise to data (jitter)")),
                  
                  #Boxplot
                  conditionalPanel(
                    condition = "input.PlotType1 == 'box'",
                    colourInput("Col_Box1", "Choose a color", value = "white"),
                    checkboxInput("notched1", "Notched boxplot"),
                    checkboxInput("horiz1", "Horizontal boxplot"),
                    checkboxInput("showMean1", "Add mean of data")),
                  
                  conditionalPanel(
                    condition = "input.PlotType1 == 'box'",
                    uiOutput('viewMeanSpecs1_1'),
                    uiOutput('viewMeanSpecs1_2'))
       
           ),
           column(2, offset = 1,
                  h4(strong("Select Item 2")),
                  br(),
                  uiOutput('ItemSelect2'),
           
                  # Choose Plot and Package 2
                  checkboxInput("Package2", "Use ggplot2 graphics"),
                  radioButtons("PlotType2", "Which type of plot shoud be displayed?", 
                               choices = c("Histogram" = "hist",
                                           "Scatterplot" = "scatter",
                                           "Boxplot" = "box"), selected = NULL)
         ),
         
         # Plot Settings 2
         column(3, 
                br(),
                br(),
                h4("Plot Settings"),
                
                #Histogram
                conditionalPanel(
                  condition = "input.PlotType2 == 'hist'",
                  colourInput("Col_Hist2", "Choose a color", value = "black"),
                  uiOutput('hist2Breaks'),
                  checkboxInput("Unit_Hist2", "Plot (absolute) Frequency", value = TRUE)),
      
                #Scatterplot
                conditionalPanel(
                  condition = "input.PlotType2 == 'scatter'",
                  colourInput("Col_Scatter2", "Choose a color", value = "black"),
                  numericInput("pShape2", "Change shape of points (from R graphics)", value = 20, min = 0, max = 25),
                  checkboxInput("jitter2", "Add noise to data (jitter)")),
                
                #Bopxplot
                conditionalPanel(
                  condition = "input.PlotType2 == 'box'",
                  colourInput("Col_Box2", "Choose a color", value = "white"),
                  checkboxInput("notched2", "Notched boxplot"),
                  checkboxInput("horiz2", "Horizontal boxplot"),
                  checkboxInput("showMean2", "Add mean of data")),
                
                conditionalPanel(
                  condition = "input.PlotType2 == 'box'",
                  uiOutput('viewMeanSpecs2_1'),
                  uiOutput('viewMeanSpecs2_2'))
                
        )
      ),
         
      hr(),
         
      fluidRow(
        column(5,
              plotOutput('dataPlot1')
             
            ),
            
        column(5, offset = 1,
              plotOutput('dataPlot2')
                   
            )
    
          )
        )
      )
    )
  )
)


