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
           h2("ExploreR Graphics"),
           br(),
           fluidRow(
             column(3,
                    h4(strong("Subset data")),
                    br(),
                    uiOutput('moreControls_G1')
                    
             )
             
           ),
           
           hr(),
           mainPanel(plotOutput('graphics'))
           
      ),
    
    tabPanel("Single Items",
             h2("ExploreR Graphics"),
             br(),
             column(3,
                    h4(strong("Select Item")),
                    br(),
                    uiOutput('moreControls_G2')
                    
             )
      
    )
    )
  )
)


