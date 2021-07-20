# Set Working Directory
setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/R_Testing_Atticus")

# clean the memory
rm(list=ls())

## Imports----------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(rgdal)
library(dplyr)
library(sf)
library(gpclib)
gpclibPermit()
library(maptools)
library(broom)
library(leaflet)

## SETTING Up Shapefile Data----------------------------------------------------

# Loads in the shapefile
Dist_91_ZimMap <- readOGR(dsn = paste0(getwd(),"/zwe_admbnda_adm2_zimstat_ocha_20180911"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")
Dist_60_ZimMap <- readOGR(dsn = paste0(getwd(),"/shapefiles"), layer="gadm36_ZWE_2")
Province_ZimMap <- readOGR(dsn = paste0(getwd(),"/ProvinceShapes"), layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# Loading the MPI data and combining

## READING CSV Datafiles -------------------------------------------------------
National_2017 = read.csv(file = "FinalMPIs/2017_National.csv")
National_Urban_2017 = read.csv(file = "FinalMPIs/2017_National_Urban.csv")
National_Rural_2017 = read.csv(file = "FinalMPIs/2017_National_Rural.csv")
District_2017 = read.csv(file = "FinalMPIs/2017_District.csv")
District_Urban_2017 = read.csv(file = "FinalMPIs/2017_District_Urban.csv")
District_Rural_2017 = read.csv(file = "FinalMPIs/2017_District_Rural.csv")
District_91_2017 = read.csv(file = 'MappingData.csv')

## DISTRICT DATA CLEANING-------------------------------------------------------

Dist_60_ZimMap@data$NAME_2[47] = "Bulilima"
Dist_60_ZimMap@data$NAME_2[50] = "Mangwe"
Dist_60_ZimMap@data$NAME_2[24] = "Uzumba Maramba Pfungwe (UMP)"
Dist_60_ZimMap@data$NAME_2[25] = "Hwedza"

colnames(District_2017)[2] <- "NAME_2"
colnames(District_Urban_2017)[2] <- "NAME_2"
colnames(District_Rural_2017)[2] <- "NAME_2"

Dist_60_Total = Dist_60_ZimMap
Dist_60_Urban = Dist_60_ZimMap
Dist_60_Rural = Dist_60_ZimMap


Dist_60_Total@data = merge(Dist_60_Total@data, District_2017, by = c("NAME_2"), sort = FALSE)
Dist_60_Urban@data = merge(Dist_60_ZimMap@data, District_Urban_2017, by = c("NAME_2"), sort = FALSE)
Dist_60_Rural@data = merge(Dist_60_Rural@data, District_Rural_2017, by = c("NAME_2"), sort = FALSE)

## 91 DISTRICT DATA CLEANING---------------------------------------------------- 
# and the MPI file. We renamed the shapefile districts to the 
# districts used in the PICES Survey
names = c("Beitbridge Rural"	,
          "Beitbridge Urban"	,
          "Bikita"          	,
          "Bindura Rural"   	,
          "Bindura"         	,
          "Binga"           	,
          "Bubi"            	,
          "Buhera"          	,
          "Bulawayo Urban"  	,
          "Bulilima"        	,
          "Muzarabani"      	,
          "Chegutu Rural"   	,
          "Chegutu"         	,
          "Chikomba"        	,
          "Chimamimani"     	,
          "Chinhoyi"        	,
          "Chipinge"        	,
          "Chipinge Urban"  	,
          "Chiredzi"        	,
          "Chiredzi Town"   	,
          "Chirumhanzu"     	,
          "Chitungwiza"     	,
          "Chivi"           	,
          "Epworth"         	,
          "Gokwe North"     	,
          "Gokwe South"     	,
          "Gokwe Centre"    	,
          "Goromonzi"       	,
          "Guruve"          	,
          "Gutu"            	,
          "Gwanda Rural"    	,
          "Gwanda"          	,
          "Gweru Rural"     	,
          "Gweru"           	,
          "Harare Urban"    	,
          "Harare Rural"    	,
          "Hurungwe"        	,
          "Hwange Rural"    	,
          "Hwange"          	,
          "Hwedza"          	,
          "Insiza"          	,
          "Kadoma"          	,
          "Kariba Rural"    	,
          "Kariba"          	,
          "Karoi"           	,
          "Kwekwe Rural"    	,
          "Kwekwe"          	,
          "Lupane"          	,
          "Makonde"         	,
          "Makoni"          	,
          "Mangwe"          	,
          "Marondera Rural" 	,
          "Marondera Urban" 	,
          "Masvingo Rural"  	,
          "Masvingo Urban"  	,
          "Matobo"          	,
          "Mazowe"          	,
          "Mberengwa"       	,
          "Mbire"           	,
          "Mhondoro-Ngezi"  	,
          "Mountt Darwin"   	,
          "Madzi"           	,
          "Murehwa"         	,
          "Mutare Rural"    	,
          "Mutare Urban"    	,
          "Mutasa"          	,
          "Mutoko"          	,
          "Mvuri"           	,
          "Mwezeni"         	,
          "Nkayi"           	,
          "Norton"          	,
          "Nyanga"          	,
          "Plumtree"        	,
          "Redcliff"        	,
          "Rusape"          	,
          "Rushinga"        	,
          "Ruwa Local Board"	,
          "Sanyati"         	,
          "Seke"            	,
          "Shamva"          	,
          "Shurugwi Rural"  	,
          "Shurugwi Urban"  	,
          "Tsholotsho"      	,
          "Umguza"          	,
          "Umzingwane"      	,
          "UMP"             	,
          "Victoria Falls"  	,
          "Zaka"            	,
          "Zwinba" 	,
          "Zvishavane Rural"	,
          "Zvishavane Urban")

# Saves the new names to the shapefile
Dist_91_ZimMap@data[["ADM2_EN"]] <- names

# Rename the district to match the shapefile
colnames(District_91_2017)[1] <- "ADM2_EN"




# Merges the MPI data with the shapefile data while preserving the order of 
# the shapefile names to match up with the polygons
Dist_91_ZimMap@data = merge(Dist_91_ZimMap@data, District_91_2017, by = c("ADM2_EN"), sort = FALSE)

# Rename the district to id


## Sidebar----------------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Standard 91 District Map",
      tabName = '91_Dist'
    ),
    menuItem(
      "Decomposed 60 District Map",
      tabName = '60_Dist'
    )
    
  )
)

dist_91_options = c(withMathJax("Adjusted Headcount Ratio \\((M_{0})\\)"),
                    withMathJax("Adjusted Poverty Gap \\((M_{1})\\)"),
                    withMathJax("Adjusted Poverty Severity \\((M_{2})\\)"))

## User Interface---------------------------------------------------------------
ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(
    title = 'Zimbabwe Multidimenzional Poverty Index'
  ),
  
  
  sidebar,
  
  dashboardBody(tabItems(
    ## First Sidebar ----------------------------
    tabItem(
      tabName = "91_Dist",
      # Everything has to be put in a row or column
      fluidPage(
        box(
          title = "91 District Poverty Map of Zimbabwe",
          leafletOutput("Leaflet_Map_91"),
          width = 12,
          height = 600
        ),
        box(
          sliderInput("slider_91", "K-Threshold Value", 1, 9, 3),
          width = 12
        ))),
    ## Second Sidebar Item-----------------------------------
    tabItem(
      tabName = "60_Dist",
      # Everything has to be put in a row or column
      fluidPage(
        box(
          title = "91 District Poverty Map of Zimbabwe",
          leafletOutput("Leaflet_Map_60"),
          width = 12,
          height = 600
        ),
        box(
          sliderInput("slider_60", "K-Threshold Value", 1, 9, 3),
          width = 12
        ))
    )
    )))


get_polygon <- function(map, data, palette, palette_data, labels, group_name) {
  return (addPolygons(map = map,
                      data = data,
                      fillColor = ~palette(palette_data),
                      weight = 2,
                      opacity = .5,
                      color = "white",
                      fillOpacity = 0.7,
                      highlight = highlightOptions(
                        weight = 5,
                        color = 'white',
                        fillOpacity = 1,
                        bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"),
                      group = group_name))
}

get_label <- function(name_data, metric_name, metric, national_metric) {
  label <- sprintf(
    paste0("<strong>%s</strong><br/>
    <strong>" , metric_name , ":</strong> %g<br/>
    <strong>National " , metric_name , ":</strong> %g"),
    name_data, metric, national_metric) %>% lapply(htmltools::HTML)
  return(label)
}
  

server <- function(input, output) {
  
  output$Leaflet_Map_91 <- renderLeaflet({
    
    M0 <- switch(input$slider_91,
                 Dist_91_ZimMap@data$M0_k1,
                 Dist_91_ZimMap@data$M0_k2,
                 Dist_91_ZimMap@data$M0_k3,
                 Dist_91_ZimMap@data$M0_k4,
                 Dist_91_ZimMap@data$M0_k5,
                 Dist_91_ZimMap@data$M0_k6,
                 Dist_91_ZimMap@data$M0_k7,
                 Dist_91_ZimMap@data$M0_k8,
                 Dist_91_ZimMap@data$M0_k9
    )
    
    M1 = switch(input$slider_91,
                Dist_91_ZimMap@data$M1_k1,
                Dist_91_ZimMap@data$M1_k2,
                Dist_91_ZimMap@data$M1_k3,
                Dist_91_ZimMap@data$M1_k4,
                Dist_91_ZimMap@data$M1_k5,
                Dist_91_ZimMap@data$M1_k6,
                Dist_91_ZimMap@data$M1_k7,
                Dist_91_ZimMap@data$M1_k8,
                Dist_91_ZimMap@data$M1_k9)
    
    M2 = switch(input$slider_91,
                Dist_91_ZimMap@data$M2_k1,
                Dist_91_ZimMap@data$M2_k2,
                Dist_91_ZimMap@data$M2_k3,
                Dist_91_ZimMap@data$M2_k4,
                Dist_91_ZimMap@data$M2_k5,
                Dist_91_ZimMap@data$M2_k6,
                Dist_91_ZimMap@data$M2_k7,
                Dist_91_ZimMap@data$M2_k8,
                Dist_91_ZimMap@data$M2_k9)
    

  
    
    pal <- colorNumeric(
      palette = "inferno",
      domain = M0,
      reverse = TRUE)
    
    M0_labels <- get_label(Dist_91_ZimMap@data$ADM2_EN, "M<sub>0</sub>", M0, switch(input$slider_91,
                                                                                    National_2017$M0_k1[1],
                                                                                    National_2017$M0_k2[1],
                                                                                    National_2017$M0_k3[1],
                                                                                    National_2017$M0_k4[1],
                                                                                    National_2017$M0_k5[1],
                                                                                    National_2017$M0_k6[1],
                                                                                    National_2017$M0_k7[1],
                                                                                    National_2017$M0_k8[1],
                                                                                    National_2017$M0_k9[1]))
    
    M1_labels <- get_label(Dist_91_ZimMap@data$ADM2_EN, "M<sub>1</sub>", M1, switch(input$slider_91,
                                                                                    National_2017$M1_k1[1],
                                                                                    National_2017$M1_k2[1],
                                                                                    National_2017$M1_k3[1],
                                                                                    National_2017$M1_k4[1],
                                                                                    National_2017$M1_k5[1],
                                                                                    National_2017$M1_k6[1],
                                                                                    National_2017$M1_k7[1],
                                                                                    National_2017$M1_k8[1],
                                                                                    National_2017$M1_k9[1]))
    
    M2_labels <- get_label(Dist_91_ZimMap@data$ADM2_EN, "M<sub>2</sub>", M2, switch(input$slider_91,
                                                                                    National_2017$M2_k1[1],
                                                                                    National_2017$M2_k2[1],
                                                                                    National_2017$M2_k3[1],
                                                                                    National_2017$M2_k4[1],
                                                                                    National_2017$M2_k5[1],
                                                                                    National_2017$M2_k6[1],
                                                                                    National_2017$M2_k7[1],
                                                                                    National_2017$M2_k8[1],
                                                                                    National_2017$M2_k9[1]))


    
    leaflet(
      options = leafletOptions(
        minZoom = 0, maxZoom= 18,
        drag = FALSE)) %>% addTiles() %>%
      setView(lng = 30, lat=-19, zoom=6) %>% 
      get_polygon(Dist_91_ZimMap, pal, M0, M0_labels, "M0") %>%
      get_polygon(Dist_91_ZimMap, pal, M1, M1_labels, "M1") %>%
      get_polygon(Dist_91_ZimMap, pal, M2, M2_labels, "M2") %>%
      clearControls() %>%
      addLayersControl(
        baseGroups = c("M0", "M1", "M2"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend(pal = pal, values = M0, opacity = 0.7, title = switch(input$slider_91,
                                                                      "Index with K=1",
                                                                      "Index with K=2",
                                                                      "Index with K=3",
                                                                      "Index with K=4",
                                                                      "Index with K=5",
                                                                      "Index with K=6",
                                                                      "Index with K=7",
                                                                      "Index with K=8",
                                                                      "Index with K=9",),
                position = "bottomright")
      
  })
  
  output$Leaflet_Map_60 <- renderLeaflet({
    
    ## Total District Data------------------------------------------------------
    M0_Total = switch(input$slider_60,
                      Dist_60_Total@data$M0_k1,
                      Dist_60_Total@data$M0_k2,
                      Dist_60_Total@data$M0_k3,
                      Dist_60_Total@data$M0_k4,
                      Dist_60_Total@data$M0_k5,
                      Dist_60_Total@data$M0_k6,
                      Dist_60_Total@data$M0_k7,
                      Dist_60_Total@data$M0_k8,
                      Dist_60_Total@data$M0_k9)
    
    M1_Total = switch(input$slider_60,
                      Dist_60_Total@data$M1_k1,
                      Dist_60_Total@data$M1_k2,
                      Dist_60_Total@data$M1_k3,
                      Dist_60_Total@data$M1_k4,
                      Dist_60_Total@data$M1_k5,
                      Dist_60_Total@data$M1_k6,
                      Dist_60_Total@data$M1_k7,
                      Dist_60_Total@data$M1_k8,
                      Dist_60_Total@data$M1_k9)
    
    M2_Total = switch(input$slider_60,
                      Dist_60_Total@data$M2_k1,
                      Dist_60_Total@data$M2_k2,
                      Dist_60_Total@data$M2_k3,
                      Dist_60_Total@data$M2_k4,
                      Dist_60_Total@data$M2_k5,
                      Dist_60_Total@data$M2_k6,
                      Dist_60_Total@data$M2_k7,
                      Dist_60_Total@data$M2_k8,
                      Dist_60_Total@data$M2_k9)
    
    
    ## Urban District Data -----------------------------------------------------
    M0_Urban = switch(input$slider_60,
                      Dist_60_Urban@data$M0_k1,
                      Dist_60_Urban@data$M0_k2,
                      Dist_60_Urban@data$M0_k3,
                      Dist_60_Urban@data$M0_k4,
                      Dist_60_Urban@data$M0_k5,
                      Dist_60_Urban@data$M0_k6,
                      Dist_60_Urban@data$M0_k7,
                      Dist_60_Urban@data$M0_k8,
                      Dist_60_Urban@data$M0_k9)
    
    M1_Urban = switch(input$slider_60,
                      Dist_60_Urban@data$M1_k1,
                      Dist_60_Urban@data$M1_k2,
                      Dist_60_Urban@data$M1_k3,
                      Dist_60_Urban@data$M1_k4,
                      Dist_60_Urban@data$M1_k5,
                      Dist_60_Urban@data$M1_k6,
                      Dist_60_Urban@data$M1_k7,
                      Dist_60_Urban@data$M1_k8,
                      Dist_60_Urban@data$M1_k9)
    
    M2_Urban = switch(input$slider_60,
                      Dist_60_Urban@data$M2_k1,
                      Dist_60_Urban@data$M2_k2,
                      Dist_60_Urban@data$M2_k3,
                      Dist_60_Urban@data$M2_k4,
                      Dist_60_Urban@data$M2_k5,
                      Dist_60_Urban@data$M2_k6,
                      Dist_60_Urban@data$M2_k7,
                      Dist_60_Urban@data$M2_k8,
                      Dist_60_Urban@data$M2_k9)
    
    pal <- colorNumeric(
      palette = "inferno",
      domain = M0_Total,
      reverse = TRUE)
    
    M0_labels <- sprintf(
      "<strong>%s</strong>
    <strong><br/>M<sub>0</sub>:</strong> %g<br/>
      <strong>National M<sub>0</sub></strong>: %g",
      Dist_60_Total@data$NAME_2, M0_Total, switch(input$slider_91,
                                              National_2017$M0_k1[1],
                                              National_2017$M0_k2[1],
                                              National_2017$M0_k3[1],
                                              National_2017$M0_k4[1],
                                              National_2017$M0_k5[1],
                                              National_2017$M0_k6[1],
                                              National_2017$M0_k7[1],
                                              National_2017$M0_k8[1],
                                              National_2017$M0_k9[1])
    ) %>% lapply(htmltools::HTML)

    
    map <- leaflet(
      options = leafletOptions(
        minZoom = 0, maxZoom= 18,
        drag = FALSE)) %>% addTiles() %>%
      setView(lng = 30, lat=-19, zoom=6) %>%
      get_polygon(Dist_60_Total, pal, M0_Total, M0_labels, "M0_Total")
    
    
    
  })   
}

# Functions Call

shinyApp(ui, server)
