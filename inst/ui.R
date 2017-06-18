# ui.R

shinyUI(navbarPage(strong("Data ExploreR"), 
  tabPanel("Data Import",
    
    titlePanel("ExploreR Data Import"),
    
    sidebarLayout(position = "left",
      sidebarPanel(
      
        helpText("Load in your datafile and specify its properties."),
        
        fileInput("inFile", "Choose File"),
        
        checkboxInput("head", strong("Header"), TRUE),
        
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
  
  tabPanel("Data Summary",
           
    fluidRow(
    
    mainPanel(h2("ExploreR Data Summary"),
              br(),
              dataTableOutput('summary'))
    
    )
  ),
  tabPanel("Graphics",
           h2("ExploreR Graphics"),
           br(),
           fluidRow(
             column(3,
                    h4(strong("Subset data")),
                    br(),
                    sliderInput("ColRange", "Range of columns", 
                                min = 1, max = 20, value = c(1, 20))
             ),
             
             column(4,
                    h4(strong("Test")))
             
           ),
           
           hr(),
           mainPanel(plotOutput('graphics'))
           
           
    )
  )
)


