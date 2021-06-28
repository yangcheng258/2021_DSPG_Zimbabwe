# Imports
library(shiny)
library(shinydashboard)
library(shinythemes)

## User Interface
ui <- dashboardPage(
  skin = 'midnight',
  dashboardHeader(
    title = 'Zimbabwe\n Multidimenzional Poverty Index'
  ),
  
  
  dashboardSidebar(),
  
  
  dashboardBody(
    # Everything has to be put in a row or column
    fluidPage(
      # Change the theme
      theme = shinytheme('superhero'),
      # Make a box with a plot inside of it
      box(
        title = "Poverty Plot",
        plotOutput("plot1", height = 250),
        width = 12
        ),
      
      box(
        
        title = 'Deprivation Cutoff',
        sliderInput("slider", "K-Threshold Value", 0, 5, 1),
        width = 12
      )
    )
  )
)

# Server

server <- function(input, output) {
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    
  })
}

# Functions Call

shinyApp(ui, server)