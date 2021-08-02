# Set Working Directory
setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/ShinyApp")

# clean the memory
rm(list=ls())

## IMPORTS ---------------------------------------------------------------------
library(leaflet)
library(ggplot2)
library(rgdal)
library(dplyr)
library(sf)
library(gpclib)
library(maptools)
library(ggpolypath)
gpclibPermit()


## LOADING DATA ----------------------------------------------------------------

# Loading the shapefile
Dist_91_Map <-  readOGR(dsn = paste0(getwd(),"/Shapefiles/91DistrictShapefiles"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")

# Loading the MPI data at the district level
Dist_91_Total_2017 = read.csv(file = 'MappingData/OriginalMPI/2017/2017_91_District.csv')
Dist_91_Urban_2017 = read.csv(file = 'MappingData/OriginalMPI/2017/2017_91_District_Urban.csv')
Dist_91_Rural_2017 = read.csv(file = 'MappingData/OriginalMPI/2017/2017_91_District_Rural.csv')
National_2017 = read.csv(file = 'MappingData/OriginalMPI/2017/2017_National.csv')

## NAME FIXING------------------------------------------------------------------
# This is to fix naming discrepancies between the shapefile
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
Dist_91_Map@data[["ADM2_EN"]] <- names

## MERGING DATA-----------------------------------------------------------------

# Renames the columns in the data to merge
colnames(Dist_91_Total_2017)[2] <- "ADM2_EN"
colnames(Dist_91_Urban_2017)[2] <- "ADM2_EN"
colnames(Dist_91_Rural_2017)[2] <- "ADM2_EN"

# To avoid overlap in data, three different maps are created to host the rural, 
# urban and total MPI Data and decompositions 
Dist_91_Total_Map = Dist_91_Map
Dist_91_Urban_Map = Dist_91_Map
Dist_91_Rural_Map = Dist_91_Map

# Merges the shapefiles with the data csv files 
Dist_91_Total_Map@data = merge(Dist_91_Total_Map@data, Dist_91_Total_2017, by = c("ADM2_EN"), sort = FALSE)
Dist_91_Urban_Map@data = merge(Dist_91_Urban_Map@data, Dist_91_Urban_2017, by = c("ADM2_EN"), sort = FALSE)
Dist_91_Rural_Map@data = merge(Dist_91_Rural_Map@data, Dist_91_Rural_2017, by = c("ADM2_EN"), sort = FALSE)

## MAPPING FUNCTIONs------------------------------------------------------------
# This section utilizes a function that condenses the use of labels and polygons
# in the leaflet maps. These functions clean up the code so that there isn't 
# a lot of repeated lines :)
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
  print(label)
  return(label)
}

## MAPPING DATA-----------------------------------------------------------------

# Here is where you set the threshold. In the ShinyApp maps, this will be a slider,
# but to keep the structure consistent, I just set the threshold here and kept 
# the rest of the code the same :) when implementing into the shinyapp just change
# the name k_threshold to the name of the slider

k_threshold = 3


# Creating variables for M0, M1 and M2
M0_Total <- switch(k_threshold,
             Dist_91_Total_Map@data$M0_k1,
             Dist_91_Total_Map@data$M0_k2,
             Dist_91_Total_Map@data$M0_k3,
             Dist_91_Total_Map@data$M0_k4,
             Dist_91_Total_Map@data$M0_k5,
             Dist_91_Total_Map@data$M0_k6,
             Dist_91_Total_Map@data$M0_k7,
             Dist_91_Total_Map@data$M0_k8,
             Dist_91_Total_Map@data$M0_k9
)

M1_Total = switch(k_threshold,
            Dist_91_Total_Map@data$M1_k1,
            Dist_91_Total_Map@data$M1_k2,
            Dist_91_Total_Map@data$M1_k3,
            Dist_91_Total_Map@data$M1_k4,
            Dist_91_Total_Map@data$M1_k5,
            Dist_91_Total_Map@data$M1_k6,
            Dist_91_Total_Map@data$M1_k7,
            Dist_91_Total_Map@data$M1_k8,
            Dist_91_Total_Map@data$M1_k9)

M2_Total = switch(k_threshold,
            Dist_91_Total_Map@data$M2_k1,
            Dist_91_Total_Map@data$M2_k2,
            Dist_91_Total_Map@data$M2_k3,
            Dist_91_Total_Map@data$M2_k4,
            Dist_91_Total_Map@data$M2_k5,
            Dist_91_Total_Map@data$M2_k6,
            Dist_91_Total_Map@data$M2_k7,
            Dist_91_Total_Map@data$M2_k8,
            Dist_91_Total_Map@data$M2_k9)


# Urban variables for M0, M1 and M2 
M0_Urban <- switch(k_threshold,
                 Dist_91_Urban_Map@data$M0_k1,
                 Dist_91_Urban_Map@data$M0_k2,
                 Dist_91_Urban_Map@data$M0_k3,
                 Dist_91_Urban_Map@data$M0_k4,
                 Dist_91_Urban_Map@data$M0_k5,
                 Dist_91_Urban_Map@data$M0_k6,
                 Dist_91_Urban_Map@data$M0_k7,
                 Dist_91_Urban_Map@data$M0_k8,
                 Dist_91_Urban_Map@data$M0_k9
)

M1_Urban = switch(k_threshold,
                  Dist_91_Urban_Map@data$M1_k1,
                  Dist_91_Urban_Map@data$M1_k2,
                  Dist_91_Urban_Map@data$M1_k3,
                  Dist_91_Urban_Map@data$M1_k4,
                  Dist_91_Urban_Map@data$M1_k5,
                  Dist_91_Urban_Map@data$M1_k6,
                  Dist_91_Urban_Map@data$M1_k7,
                  Dist_91_Urban_Map@data$M1_k8,
                  Dist_91_Urban_Map@data$M1_k9)

M2_Urban = switch(k_threshold,
                  Dist_91_Urban_Map@data$M2_k1,
                  Dist_91_Urban_Map@data$M2_k2,
                  Dist_91_Urban_Map@data$M2_k3,
                  Dist_91_Urban_Map@data$M2_k4,
                  Dist_91_Urban_Map@data$M2_k5,
                  Dist_91_Urban_Map@data$M2_k6,
                  Dist_91_Urban_Map@data$M2_k7,
                  Dist_91_Urban_Map@data$M2_k8,
                  Dist_91_Urban_Map@data$M2_k9)

# Rural MPI Variables

# Urban variables for M0, M1 and M2 
M0_Rural <- switch(k_threshold,
                   Dist_91_Rural_Map@data$M0_k1,
                   Dist_91_Rural_Map@data$M0_k2,
                   Dist_91_Rural_Map@data$M0_k3,
                   Dist_91_Rural_Map@data$M0_k4,
                   Dist_91_Rural_Map@data$M0_k5,
                   Dist_91_Rural_Map@data$M0_k6,
                   Dist_91_Rural_Map@data$M0_k7,
                   Dist_91_Rural_Map@data$M0_k8,
                   Dist_91_Rural_Map@data$M0_k9
)

M1_Rural = switch(k_threshold,
                  Dist_91_Rural_Map@data$M1_k1,
                  Dist_91_Rural_Map@data$M1_k2,
                  Dist_91_Rural_Map@data$M1_k3,
                  Dist_91_Rural_Map@data$M1_k4,
                  Dist_91_Rural_Map@data$M1_k5,
                  Dist_91_Rural_Map@data$M1_k6,
                  Dist_91_Rural_Map@data$M1_k7,
                  Dist_91_Rural_Map@data$M1_k8,
                  Dist_91_Rural_Map@data$M1_k9)

M2_Rural = switch(k_threshold,
                  Dist_91_Rural_Map@data$M2_k1,
                  Dist_91_Rural_Map@data$M2_k2,
                  Dist_91_Rural_Map@data$M2_k3,
                  Dist_91_Rural_Map@data$M2_k4,
                  Dist_91_Rural_Map@data$M2_k5,
                  Dist_91_Rural_Map@data$M2_k6,
                  Dist_91_Rural_Map@data$M2_k7,
                  Dist_91_Rural_Map@data$M2_k8,
                  Dist_91_Rural_Map@data$M2_k9)

# 1 = Total, 2 = Urban, 3 = Rural
UrbRurSelection = 3

M0 = switch(UrbRurSelection,
            M0_Total,
            M0_Urban,
            M0_Rural)

M1 = switch(UrbRurSelection,
            M1_Total,
            M1_Urban,
            M1_Rural)

M2 = switch(UrbRurSelection,
            M2_Total,
            M2_Urban,
            M2_Rural)

# This is the color palette used in the graphs
pal <- colorNumeric(
  palette = "viridis",
  domain = c(0, max(M0, na.rm = TRUE)),
  reverse = TRUE)

# This creates labels for M0, M1 and M2 
M0_labels <- get_label(Dist_91_Map@data$ADM2_EN, "M<sub>0</sub>", M0, switch(k_threshold,
                                                                             National_2017$M0_k1[1],
                                                                             National_2017$M0_k2[1],
                                                                             National_2017$M0_k3[1],
                                                                             National_2017$M0_k4[1],
                                                                             National_2017$M0_k5[1],
                                                                             National_2017$M0_k6[1],
                                                                             National_2017$M0_k7[1],
                                                                             National_2017$M0_k8[1],
                                                                             National_2017$M0_k9[1]))

M1_labels <- get_label(Dist_91_Map@data$ADM2_EN, "M<sub>1</sub>", M1, switch(k_threshold,
                                                                             National_2017$M1_k1[1],
                                                                             National_2017$M1_k2[1],
                                                                             National_2017$M1_k3[1],
                                                                             National_2017$M1_k4[1],
                                                                             National_2017$M1_k5[1],
                                                                             National_2017$M1_k6[1],
                                                                             National_2017$M1_k7[1],
                                                                             National_2017$M1_k8[1],
                                                                             National_2017$M1_k9[1]))

M2_labels <- get_label(Dist_91_Map@data$ADM2_EN, "M<sub>2</sub>", M2, switch(k_threshold,
                                                                             National_2017$M2_k1[1],
                                                                             National_2017$M2_k2[1],
                                                                             National_2017$M2_k3[1],
                                                                             National_2017$M2_k4[1],
                                                                             National_2017$M2_k5[1],
                                                                             National_2017$M2_k6[1],
                                                                             National_2017$M2_k7[1],
                                                                             National_2017$M2_k8[1],
                                                                             National_2017$M2_k9[1]))

## MAPPING----------------------------------------------------------------------
# These lines of code fix the positioning of the "No Data" label. Previously, it
# was appearing to the right of the data instead of at the bottom where it was 
# supposed to. 
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

# This is where the map gets plotted 
leaflet(
  options = leafletOptions(
    minZoom = 0, maxZoom= 18,
    drag = FALSE)) %>% addTiles() %>%
  setView(lng = 30, lat=-19, zoom=6) %>% 
  get_polygon(Dist_91_Map, pal, M0, M0_labels, "M0") %>%
  get_polygon(Dist_91_Map, pal, M1, M1_labels, "M1") %>%
  get_polygon(Dist_91_Map, pal, M2, M2_labels, "M2") %>%
  clearControls() %>%
  addLayersControl(
    baseGroups = c("M0", "M1", "M2"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(pal = pal, values = c(0, max(M0, na.rm = TRUE)), opacity = 0.7, title = paste0("Index with k = ", k_threshold),
            position = "bottomright") %>%
  htmlwidgets::prependContent(html_fix)






