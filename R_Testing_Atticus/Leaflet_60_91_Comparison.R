# Set Working Directory
setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/R_Testing_Atticus")

# clean the memory
rm(list=ls())

## IMPORTS -----------------------
library(leaflet)
library(ggplot2)
library(rgdal)
library(dplyr)
library(sf)
library(gpclib)
library(maptools)
library(ggpolypath)
gpclibPermit()

## DATA LOADING ----------------------------
ZimMap <-  readOGR(dsn = paste0(getwd(),"/shapefiles"), layer="gadm36_ZWE_2")
ZimMap2 <- readOGR(dsn = paste0(getwd(),"/zwe_admbnda_adm2_zimstat_ocha_20180911"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")


labels <- sprintf(
  "<strong>%s</strong>
    ",
  ZimMap@data$NAME_2
) %>% lapply(htmltools::HTML)

labels2 <- sprintf(
  "<strong>%s</strong>
    ",
  ZimMap2@data$ADM2_EN
) %>% lapply(htmltools::HTML)

M <- leaflet(
  options = leafletOptions(
    minZoom = 0, maxZoom= 18,
    drag = FALSE)) %>% addTiles() %>%
  setView(lng = 30, lat=-19, zoom=7) %>%
  addPolygons(data = ZimMap2,
              color = 'red',
              label = labels2,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = ZimMap,
              highlight = highlightOptions(
                weight = 5,
                color = 'white',
                fillOpacity = 1,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  ) 
  

M

