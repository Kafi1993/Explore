# ui.R

shinyUI(fluidPage(
  
  titlePanel(h1(strong("Data ExploreR"))),
  
  sidebarLayout(position = "left",
    sidebarPanel(
    
      helpText("Load in your dataset and specify its properties."),
      
      fileInput("dataframe", "Choose File"),
      
      br(),
      
      selectInput("sep", "Choose a data seperator:",
                  choices = c("Tab", "Comma", "Semicolon")),
      
      selectInput("dec", "Choose a decimal seperator:",
                  choices = c("Comma", "Dot"))
      
    ),

    mainPanel("Main Panel")
    
  )
))


