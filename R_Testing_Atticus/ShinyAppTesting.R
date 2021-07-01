# Set Working Directory
#setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/R_Testing_Atticus")

# clean the memory
rm(list=ls())

# Imports
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(rgdal)
library(dplyr)
library(sf)
library(gpclib)
#gpclibPermit()
library(maptools)
library(broom)

## SETTING UP MPI Data

# Loads in the shapefile
ZimMap <- readOGR(dsn = paste0(getwd(),"/R_Testing_Atticus/ProvinceShapes"), layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# Loading the MPI data and combining
id <- ZimMap@data[["ADM1_EN"]]
MPIData = read.csv(file = 'ProvinceData.csv')

# Rename the district to id
colnames(MPIData)[1] <- "id"

### 'fortify' the data to get a dataframe format required by ggplot2  By Yang
ZimMap_fortified <- tidy(ZimMap, region = "ADM1_EN")

# Currently we need to manually merge the two together
datapoly <- merge(ZimMap_fortified, MPIData , by = c("id"))

## Sidebar

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Poverty Index",
      tabName = 'M0'
    ),
    menuItem(
      "Adjusted Poverty Gap",
      tabName = "M1"
    ),
    menuItem(
      "Adjusted Poverty Severity",
      tabName = "M2"
    )
    
  )
)


## User Interface
ui <- dashboardPage(
  skin = 'midnight',
  dashboardHeader(
    title = 'Zimbabwe\n Multidimenzional Poverty Index'
  ),
  
  
  sidebar,
  
  dashboardBody(tabItems(
    tabItem(
      tabName = "M0",
      # Everything has to be put in a row or column
      fluidPage(
        # Change the theme
        theme = shinytheme('superhero'),
        # Make a box with a plot inside of it
        box(
          title = "Multidimensional Poverty Inde (By Province)",
          plotOutput("M0_plot", height = 300, width = 400),
          width = 12
        ),
        
        box(
          
          title = 'Deprivation Cutoff',
          sliderInput("slider0", "K-Threshold Value", 1, 9, 3),
          width = 12)
)
    ),
    
    tabItem(
      tabName = "M1",
      
      fluidPage(
        box(
          title = "Adjusted Poverty Gap (By Province)",
          plotOutput("M1_plot", height = 300, width = 440),
          width = 12
        ),
        box(
          title = "Deprivation Cutoff",
          sliderInput("slider1", "K-Threshold Value", 1, 9, 3),
          width = 12
        )
      )
      ),
    
    tabItem(
      tabName = "M2",
      
      fluidPage(
        box(
          title = "Adjusted Poverty Severity (By Province)",
          plotOutput("M2_plot", height = 300, width = 480),
          width = 12
        ),
        box(
          title = "Deprivation Cutoff",
          sliderInput("slider2", "K-Threshold Value", 1, 9, 3),
          width = 12
        )
      )
    )

  ))
)

# Server

server <- function(input, output) {
  histdata <- rnorm(500)
  
  
  output$M0_plot <- renderPlot({
    ggplot(datapoly, aes(x=long, y=lat, group = group)) +  
      geom_polygon(aes(fill = switch(input$slider0, 
                                     M0_k1,
                                     M0_k2,
                                     M0_k3,
                                     M0_k4,
                                     M0_k5,
                                     M0_k6,
                                     M0_k7,
                                     M0_k8,
                                     M0_k9), group = id)) + 
      scale_fill_gradient(low='grey', high = 'maroon') +
      labs(fill = "Poverty Index")
  })
  
  output$M1_plot <- renderPlot({
    ggplot(datapoly, aes(x=long, y=lat, group = group)) +  
      geom_polygon(aes(fill = switch(input$slider1, 
                                     M1_k1,
                                     M1_k2,
                                     M1_k3,
                                     M1_k4,
                                     M1_k5,
                                     M1_k6,
                                     M1_k7,
                                     M1_k8,
                                     M1_k9), group = id)) + 
      scale_fill_gradient(low='grey', high = 'maroon') +
      labs(fill = "Adj. Poverty Gap")
  })
  
  output$M2_plot <- renderPlot({
    ggplot(datapoly, aes(x=long, y=lat, group = group)) +  
      geom_polygon(aes(fill = switch(input$slider2, 
                                     M2_k1,
                                     M2_k2,
                                     M2_k3,
                                     M2_k4,
                                     M2_k5,
                                     M2_k6,
                                     M2_k7,
                                     M2_k8,
                                     M2_k9), group = id)) + 
      scale_fill_gradient(low='grey', high = 'maroon') +
      labs(fill = "Adj. Poverty Severity")
  })
}

# Functions Call

shinyApp(ui, server)