
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

# This reads in the CSV files for the data
National_2017 = read.csv(file = "FinalMPIs/2017_National.csv")
National_Urban_2017 = read.csv(file = "FinalMPIs/2017_National_Urban.csv")
National_Rural_2017 = read.csv(file = "FinalMPIs/2017_National_Rural.csv")
District_2017 = read.csv(file = "FinalMPIs/2017_District.csv")
District_Urban_2017 = read.csv(file = "FinalMPIs/2017_District_Urban.csv")
District_Rural_2017 = read.csv(file = "FinalMPIs/2017_District_Rural.csv")

# This is for merging the data
MPIData = District_Urban_2017

ZimMap@data$NAME_2[47] = "Bulilima"
ZimMap@data$NAME_2[50] = "Mangwe"
ZimMap@data$NAME_2[24] = "Uzumba Maramba Pfungwe (UMP)"
ZimMap@data$NAME_2[25] = "Hwedza"

colnames(MPIData)[2] <- "NAME_2"






ZimMap@data = merge(ZimMap@data, MPIData, by = c("NAME_2"), sort = FALSE)



## This is for mapping --------------------------------------------------
pal <- colorNumeric(
  palette = "inferno",
  domain = ZimMap@data$M0_k3,
  reverse = TRUE)

labels <- sprintf(
  "<strong>%s</strong>
    <strong><br/>M<sub>0</sub>:</strong> %g
    <strong><br/>M<sub>1</sub>:</strong> %g
    <strong><br/>M<sub>2</sub>:</strong> %g",
  ZimMap@data$NAME_2, ZimMap@data$M0_k3, ZimMap@data$M1_k3, ZimMap@data$M2_k3
) %>% lapply(htmltools::HTML)

M <- leaflet(
  options = leafletOptions(
    minZoom = 0, maxZoom= 18,
    drag = FALSE)) %>% addTiles() %>%
  setView(lng = 30, lat=-19, zoom=7) %>%
  addPolygons(data = ZimMap,
              fillColor = ~pal(ZimMap@data$M0_k3),
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
                direction = "auto")
  ) %>%
  addLegend(pal = pal, values = ZimMap@data$M0_k3, opacity = 0.7, title = "M0 with K3",
            position = "bottomright")


# Displays the map
M
