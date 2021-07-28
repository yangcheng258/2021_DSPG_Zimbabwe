# Set Working Directory
setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/ShinyApp")

# clean the memory
rm(list=ls())

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

create_scatter <- function(names, x_data, y_data, x_label, y_label, title) {
  M0_Comparison = data.frame(names, M0_2011, M0_2017)
  colnames(M0_Comparison)[1] = "Name"
  return (ggplot(M0_Comparison, aes(x = x_data, y = y_data)) +
            geom_label_repel(aes(label = Name), size = 3, max.overlaps = 4,
                             
                             min.segment.length = unit(0, 'lines'),
                             nudge_y = 0.01) +
            geom_point(
              color= M0_2011,
              fill="#69b3a2",
              shape=22,
              alpha=1,
              size=2,
              stroke = 1
            ) +
            ggtitle(title) +
            xlab(x_label) +
            ylab(y_label) +
            theme_ipsum() +
            geom_abline()) 
  
  
}

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(rgdal)
library(stringr)
library(shinyjs)
library(dplyr)
library(sf)
library(gpclib)
library(maptools)
library(shinydashboard)
library(ggpolypath)
gpclibPermit()

# MAPS
MAP_2017_91_T_o <- readOGR(dsn = "./data/shapefiles/91DistrictShapefiles", layer="zwe_admbnda_adm2_zimstat_ocha_20180911")
MAP_2017_60_T_o <- readOGR(dsn = "./data/shapefiles/60DistrictShapefiles", layer="gadm36_ZWE_2")
MAP_2017_10_T_o <- readOGR(dsn = "./data/shapefiles/ProvinceShapefiles", layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# MPI Data

# 2017 Data
MPI_2017_91_T_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_91_District.csv')
MPI_2017_91_U_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_91_District_Urban.csv')
MPI_2017_91_R_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_91_District_Rural.csv')
MPI_2017_91_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_91_District.csv')
MPI_2017_91_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_91_District_Urban.csv')
MPI_2017_91_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_91_District_Rural.csv')
MPI_2017_60_T_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_District.csv')
MPI_2017_60_U_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_District_Urban.csv')
MPI_2017_60_R_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_District_Rural.csv')
MPI_2017_60_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_District.csv')
MPI_2017_60_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_District_Urban.csv')
MPI_2017_60_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_District_Rural.csv')
MPI_2017_10_T_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_Province.csv')
MPI_2017_10_U_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_Province_Urban.csv')
MPI_2017_10_R_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_Province_Rural.csv')
MPI_2017_10_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_Province.csv')
MPI_2017_10_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_Province_Urban.csv')
MPI_2017_10_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_Province_Rural.csv')
MPI_2017_1_T_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_National.csv')
MPI_2017_1_U_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_National_Urban.csv')
MPI_2017_1_R_o = read.csv(file = './data/MappingData/OriginalMPI/2017/2017_National_Rural.csv')
MPI_2017_1_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_National.csv')
MPI_2017_1_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_National_Urban.csv')
MPI_2017_1_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2017/2017_National_Rural.csv')
# 2011 Data
MPI_2011_60_T_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_District.csv')
MPI_2011_60_U_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_District_Urban.csv')
MPI_2011_60_R_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_District_Rural.csv')
MPI_2011_60_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_District.csv')
MPI_2011_60_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_District_Urban.csv')
MPI_2011_60_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_District_Rural.csv')
MPI_2011_10_T_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_Province.csv')
MPI_2011_10_U_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_Province_Urban.csv')
MPI_2011_10_R_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_Province_Rural.csv')
MPI_2011_10_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_Province.csv')
MPI_2011_10_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_Province_Urban.csv')
MPI_2011_10_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_Province_Rural.csv')
MPI_2011_1_T_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_National.csv')
MPI_2011_1_U_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_National_Urban.csv')
MPI_2011_1_R_o = read.csv(file = './data/MappingData/OriginalMPI/2011/2011_National_Rural.csv')
MPI_2011_1_T_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_National.csv')
MPI_2011_1_U_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_National_Urban.csv')
MPI_2011_1_R_n = read.csv(file = './data/MappingData/AdjustedMPI/2011/2011_National_Rural.csv')

## 91 District Processing
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

MAP_2017_91_T_o@data[["ADM2_EN"]] <- names

# Renames the columns in the data to merge
colnames(MPI_2017_91_T_o)[2] <- "ADM2_EN"
colnames(MPI_2017_91_U_o)[2] <- "ADM2_EN"
colnames(MPI_2017_91_R_o)[2] <- "ADM2_EN"
colnames(MPI_2017_91_T_n)[2] <- "ADM2_EN"
colnames(MPI_2017_91_U_n)[2] <- "ADM2_EN"
colnames(MPI_2017_91_R_n)[2] <- "ADM2_EN"


# To avoid overlap in data, three different maps are created to host the rural, 
# urban and total MPI Data and decompositions 
MAP_2017_91_U_o = MAP_2017_91_T_o
MAP_2017_91_R_o = MAP_2017_91_T_o
MAP_2017_91_T_n = MAP_2017_91_T_o
MAP_2017_91_U_n = MAP_2017_91_T_o
MAP_2017_91_R_n = MAP_2017_91_T_o

# Merges the Map data together
MAP_2017_91_T_o@data = merge(MAP_2017_91_T_o@data, MPI_2017_91_T_o, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_U_o@data = merge(MAP_2017_91_U_o@data, MPI_2017_91_U_o, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_R_o@data = merge(MAP_2017_91_R_o@data, MPI_2017_91_R_o, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_T_n@data = merge(MAP_2017_91_T_n@data, MPI_2017_91_T_n, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_U_n@data = merge(MAP_2017_91_U_n@data, MPI_2017_91_U_n, by = c("ADM2_EN"), sort = FALSE)
MAP_2017_91_R_n@data = merge(MAP_2017_91_R_n@data, MPI_2017_91_R_n, by = c("ADM2_EN"), sort = FALSE)

## 60 District Maps

# Renames the columns in the data to merge
colnames(MPI_2017_60_T_o)[2] <- "NAME_2"
colnames(MPI_2017_60_U_o)[2] <- "NAME_2"
colnames(MPI_2017_60_R_o)[2] <- "NAME_2"
colnames(MPI_2017_60_T_n)[2] <- "NAME_2"
colnames(MPI_2017_60_U_n)[2] <- "NAME_2"
colnames(MPI_2017_60_R_n)[2] <- "NAME_2"

colnames(MPI_2011_60_T_o)[2] <- "NAME_2"
colnames(MPI_2011_60_U_o)[2] <- "NAME_2"
colnames(MPI_2011_60_R_o)[2] <- "NAME_2"
colnames(MPI_2011_60_T_n)[2] <- "NAME_2"
colnames(MPI_2011_60_U_n)[2] <- "NAME_2"
colnames(MPI_2011_60_R_n)[2] <- "NAME_2"

MAP_2017_60_T_o@data$NAME_2[47] = "Bulilima"
MAP_2017_60_T_o@data$NAME_2[50] = "Mangwe"
MAP_2017_60_T_o@data$NAME_2[24] = "Uzumba Maramba Pfungwe (UMP)"
MAP_2017_60_T_o@data$NAME_2[25] = "Hwedza"

MAP_2017_60_U_o = MAP_2017_60_T_o
MAP_2017_60_R_o = MAP_2017_60_T_o
MAP_2017_60_T_n = MAP_2017_60_T_o
MAP_2017_60_U_n = MAP_2017_60_T_o
MAP_2017_60_R_n = MAP_2017_60_T_o

MAP_2011_60_T_o = MAP_2017_60_T_o
MAP_2011_60_U_o = MAP_2017_60_T_o
MAP_2011_60_R_o = MAP_2017_60_T_o
MAP_2011_60_T_n = MAP_2017_60_T_o
MAP_2011_60_U_n = MAP_2017_60_T_o
MAP_2011_60_R_n = MAP_2017_60_T_o

MAP_2017_60_T_o@data = merge(MAP_2017_60_T_o@data, MPI_2017_60_T_o, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_U_o@data = merge(MAP_2017_60_U_o@data, MPI_2017_60_U_o, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_R_o@data = merge(MAP_2017_60_R_o@data, MPI_2017_60_R_o, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_T_n@data = merge(MAP_2017_60_T_n@data, MPI_2017_60_T_n, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_U_n@data = merge(MAP_2017_60_U_n@data, MPI_2017_60_U_n, by = c("NAME_2"), sort = FALSE)
MAP_2017_60_R_n@data = merge(MAP_2017_60_R_n@data, MPI_2017_60_R_n, by = c("NAME_2"), sort = FALSE)

MAP_2011_60_T_o@data = merge(MAP_2011_60_T_o@data, MPI_2011_60_T_o, by = c("NAME_2"), sort = FALSE)
MAP_2011_60_U_o@data = merge(MAP_2011_60_U_o@data, MPI_2011_60_U_o, by = c("NAME_2"), sort = FALSE)
MAP_2011_60_R_o@data = merge(MAP_2011_60_R_o@data, MPI_2011_60_R_o, by = c("NAME_2"), sort = FALSE)
MAP_2011_60_T_n@data = merge(MAP_2011_60_T_n@data, MPI_2011_60_T_n, by = c("NAME_2"), sort = FALSE)
MAP_2011_60_U_n@data = merge(MAP_2011_60_U_n@data, MPI_2011_60_U_n, by = c("NAME_2"), sort = FALSE)
MAP_2011_60_R_n@data = merge(MAP_2011_60_R_n@data, MPI_2011_60_R_n, by = c("NAME_2"), sort = FALSE)

## Province Data

colnames(MPI_2017_10_T_o)[2] <- "ADM1_EN"
colnames(MPI_2017_10_U_o)[2] <- "ADM1_EN"
colnames(MPI_2017_10_R_o)[2] <- "ADM1_EN"
colnames(MPI_2017_10_T_n)[2] <- "ADM1_EN"
colnames(MPI_2017_10_U_n)[2] <- "ADM1_EN"
colnames(MPI_2017_10_R_n)[2] <- "ADM1_EN"

colnames(MPI_2011_10_T_o)[2] <- "ADM1_EN"
colnames(MPI_2011_10_U_o)[2] <- "ADM1_EN"
colnames(MPI_2011_10_R_o)[2] <- "ADM1_EN"
colnames(MPI_2011_10_T_n)[2] <- "ADM1_EN"
colnames(MPI_2011_10_U_n)[2] <- "ADM1_EN"
colnames(MPI_2011_10_R_n)[2] <- "ADM1_EN"

MAP_2017_10_U_o = MAP_2017_10_T_o
MAP_2017_10_R_o = MAP_2017_10_T_o
MAP_2017_10_T_n = MAP_2017_10_T_o
MAP_2017_10_U_n = MAP_2017_10_T_o
MAP_2017_10_R_n = MAP_2017_10_T_o

MAP_2011_10_T_o = MAP_2017_10_T_o
MAP_2011_10_U_o = MAP_2017_10_T_o
MAP_2011_10_R_o = MAP_2017_10_T_o
MAP_2011_10_T_n = MAP_2017_10_T_o
MAP_2011_10_U_n = MAP_2017_10_T_o
MAP_2011_10_R_n = MAP_2017_10_T_o

MAP_2017_10_T_o@data = merge(MAP_2017_10_T_o@data, MPI_2017_10_T_o, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_U_o@data = merge(MAP_2017_10_U_o@data, MPI_2017_10_U_o, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_R_o@data = merge(MAP_2017_10_R_o@data, MPI_2017_10_R_o, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_T_n@data = merge(MAP_2017_10_T_n@data, MPI_2017_10_T_n, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_U_n@data = merge(MAP_2017_10_U_n@data, MPI_2017_10_U_n, by = c("ADM1_EN"), sort = FALSE)
MAP_2017_10_R_n@data = merge(MAP_2017_10_R_n@data, MPI_2017_10_R_n, by = c("ADM1_EN"), sort = FALSE)

MAP_2011_10_T_o@data = merge(MAP_2011_10_T_o@data, MPI_2011_10_T_o, by = c("ADM1_EN"), sort = FALSE)
MAP_2011_10_U_o@data = merge(MAP_2011_10_U_o@data, MPI_2011_10_U_o, by = c("ADM1_EN"), sort = FALSE)
MAP_2011_10_R_o@data = merge(MAP_2011_10_R_o@data, MPI_2011_10_R_o, by = c("ADM1_EN"), sort = FALSE)
MAP_2011_10_T_n@data = merge(MAP_2011_10_T_n@data, MPI_2011_10_T_n, by = c("ADM1_EN"), sort = FALSE)
MAP_2011_10_U_n@data = merge(MAP_2011_10_U_n@data, MPI_2011_10_U_n, by = c("ADM1_EN"), sort = FALSE)
MAP_2011_10_R_n@data = merge(MAP_2011_10_R_n@data, MPI_2011_10_R_n, by = c("ADM1_EN"), sort = FALSE)

UrbRurSelection = 1

k_threshold = 3

map_2017 = switch(UrbRurSelection, MAP_2017_60_T_o, MAP_2017_60_U_o, MAP_2017_60_R_o)
map_2011 = switch(UrbRurSelection, MAP_2011_60_T_o, MAP_2017_60_U_o, MAP_2017_60_R_o)

M0_2017 = switch(k_threshold,
            map_2017@data$M0_k1,
            map_2017@data$M0_k2,
            map_2017@data$M0_k3,
            map_2017@data$M0_k4,
            map_2017@data$M0_k5,
            map_2017@data$M0_k6,
            map_2017@data$M0_k7,
            map_2017@data$M0_k8,
            map_2017@data$M0_k9)

M0_2011 = switch(k_threshold,
                 map_2011@data$M0_k1,
                 map_2011@data$M0_k2,
                 map_2011@data$M0_k3,
                 map_2011@data$M0_k4,
                 map_2011@data$M0_k5,
                 map_2011@data$M0_k6,
                 map_2011@data$M0_k7,
                 map_2011@data$M0_k8,
                 map_2011@data$M0_k9)

M0_change = M0_2017 - M0_2011

map_2017@data$M0_change = M0_change

change_labels <- sprintf(
  paste0("<strong>%s</strong><br/>
    <strong>" , "M<sub>0</sub> Change" , ":</strong> %g<br/>"),
  map_2017@data$NAME_2, M0_change) %>% lapply(htmltools::HTML)

css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

pal <- colorNumeric(
  palette = "viridis",
  domain = M0_change,
  reverse = TRUE)

# This is where the map gets plotted 
leaflet(
  options = leafletOptions(
    minZoom = 0, maxZoom= 18,
    drag = FALSE)) %>% addTiles() %>%
  setView(lng = 30, lat=-19, zoom=6) %>% 
  get_polygon(map_2017, pal, M0_change, change_labels, "M0") %>%
  clearControls() %>%
  addLayersControl(
    baseGroups = c("M0", "M1", "M2"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(pal = pal, values = M0_change, opacity = 0.7, title = paste0("Index with k = ", k_threshold),
            position = "bottomright") %>%
  htmlwidgets::prependContent(html_fix)
