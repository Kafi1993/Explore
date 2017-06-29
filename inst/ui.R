# ui.R

shinyUI(navbarPage(strong("Data ExploreR"), 
  tabPanel("Data Import",
    
    titlePanel("ExploreR Data Import"),
    
    sidebarLayout(position = "left",
      sidebarPanel(
      
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
  
  tabPanel("Data Summary",
           
    fluidRow(
    
    mainPanel(h2("ExploreR Data Summary"),
              br(),
              dataTableOutput('summary'))
    
    )
  ),
  navbarMenu("Graphics",
    tabPanel("All Items",
        fluidPage(
           h2("ExploreR Graphics"),
           br(),
           fluidRow(
             column(3,
                    h4(strong("Subset data")),
                    br(),
                    uiOutput('RangeColSelect'))
             
           ),
           
           hr(),
           plotOutput('graphics')
        
        )
    ),
    
    tabPanel("Single Item",
             h2("ExploreR Graphics"),
             br(),
             column(4,
                    h4(strong("Select Item 1")),
                    br(),
                    uiOutput('ItemSelect1')),
                    
                   
            column(4, offset = 1,
                   h4(strong("Select Item 2")),
                   br(),
                   uiOutput('ItemSelect2'))      
            
             
      
    )
    )
  )
)


