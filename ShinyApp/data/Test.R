#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinyjs)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  # Application title
  titlePanel("More graphs"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("ranking1"),
      plotlyOutput("ranking2"),
      plotlyOutput("ranking3")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$ranking1 <- renderPlotly({
    # generate bins based on input$bins from ui.R
    data <- data.frame(
      name=c("north","south","south-east","north-west","south-west","north-east","west","east"),
      val1=sample(seq(1,10), 8 ),
      val2 = sample(seq(1,10), 8 ),
      val3 = sample(seq(1,10), 8 )
    )
    
    # Dataset 2: several values per group (natively provided in R)
    # mpg
    # load the library
    library(forcats)
    
    # Reorder following the value of another column:
    p1 <-  data %>%
      mutate(name = fct_reorder(name, val1)) %>%
      ggplot( aes(x=name, y=val1)) +
      geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
      coord_flip() +
      labs(x = "", y="M0_____", title = "M0") +
      theme_classic()
    
    ggplotly(p1)
    
  })
  
  
  
  output$ranking2 <- renderPlotly({
    # generate bins based on input$bins from ui.R
    data <- data.frame(
      name=c("north","south","south-east","north-west","south-west","north-east","west","east"),
      val1=sample(seq(1,10), 8 ),
      val2 = sample(seq(1,10), 8 ),
      val3 = sample(seq(1,10), 8 )
    )
    
    # Dataset 2: several values per group (natively provided in R)
    # mpg
    # load the library
    library(forcats)
    
    # Reorder following the value of another column:
    p2 <-  data %>%
      mutate(name = fct_reorder(name, val2)) %>%
      ggplot( aes(x=name, y=val2)) +
      geom_bar(stat="identity", fill="#f68061", alpha=.6, width=.4) +
      coord_flip() +
      labs(x = "", y="M2_____", title = "M2") +
      theme_classic()
    
    ggplotly(p2)
    
  })
  
  
  output$ranking3 <- renderPlotly({
    # generate bins based on input$bins from ui.R
    data <- data.frame(
      name=c("north","south","south-east","north-west","south-west","north-east","west","east"),
      val1=sample(seq(1,10), 8 ),
      val2 = sample(seq(1,10), 8 ),
      val3 = sample(seq(1,10), 8 )
    )
    
    # Dataset 2: several values per group (natively provided in R)
    # mpg
    # load the library
    library(forcats)
    
    # Reorder following the value of another column:
    p3 <-  data %>%
      mutate(name = fct_reorder(name, val3)) %>%
      ggplot( aes(x=name, y=val3)) +
      geom_bar(stat="identity", fill="#f68061", alpha=.6, width=.4) +
      coord_flip() +
      labs(x = "", y="M3_____", title = "M3") +
      theme_classic()
    
    ggplotly(p3)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
