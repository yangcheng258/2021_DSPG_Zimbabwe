# Set Working Directory
setwd("D:/Virginia Tech/DSPG/2021_DSPG_Zimbabwe/R_Testing_Atticus")

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

# Loads the shapefile
Dist_60_Map <- readOGR(dsn = paste0(getwd(),"/Shapefiles/60DistrictShapefiles"), layer="gadm36_ZWE_2")

# Loads the district data
Dist_60_Total_2017 = read.csv("MappingData/2017_District.csv")
Dist_60_Urban_2017 = read.csv("MappingData/2017_District_Urban.csv")
Dist_60_Rural_2017 = read.csv("MappingData/2017_District_Rural.csv")

# Loads the national data
National_2017 = read.csv(file = "MappingData/2017_National.csv")
National_Urban_2017 = read.csv(file = "MappingData/2017_National_Urban.csv")
National_Rural_2017 = read.csv(file = "MappingData/2017_National_Rural.csv")



## CLEANING DISTRICT DATA-------------------------------------------------------

# Fixes four spelling changes in the shapefile
Dist_60_Map@data$NAME_2[47] = "Bulilima"
Dist_60_Map@data$NAME_2[50] = "Mangwe"
Dist_60_Map@data$NAME_2[24] = "Uzumba Maramba Pfungwe (UMP)"
Dist_60_Map@data$NAME_2[25] = "Hwedza"

# Renames the columns in the data to merge
colnames(Dist_60_Total_2017)[2] <- "NAME_2"
colnames(Dist_60_Urban_2017)[2] <- "NAME_2"
colnames(Dist_60_Urban_2017)[2] <- "NAME_2"

# To avoid overlap in data, three different maps are created to host the rural, 
# urban and total MPI Data and decompositions 
Dist_60_Total_Map = Dist_60_Map
Dist_60_Urban_Map = Dist_60_Map
Dist_60_Rural_Map = Dist_60_Map

# Merges the shapefiles with the data csv files 
Dist_60_Total_Map@data = merge(Dist_60_Total_Map@data, Dist_60_Total_2017, by = c("NAME_2"), sort = FALSE)
Dist_60_Urban_Map@data = merge(Dist_60_Urban_Map@data, Dist_60_Urban_2017, by = c("NAME_2"), sort = FALSE)
Dist_60_Rural_Map@data = merge(Dist_60_Rural_Map@data, Dist_60_Urban_2017, by = c("NAME_2"), sort = FALSE)

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
  return(label)
}

## MAPPING DATA-----------------------------------------------------------------

# Here is where you set the threshold. In the ShinyApp maps, this will be a slider,
# but to keep the structure consistent, I just set the threshold here and kept 
# the rest of the code the same :) when implementing into the shinyapp just change
# the name k_threshold to the name of the slider

k_threshold = 3


# Variables for total data are created 
M0_Total = switch(k_threshold,
                  Dist_60_Total_Map@data$M0_k1,
                  Dist_60_Total_Map@data$M0_k2,
                  Dist_60_Total_Map@data$M0_k3,
                  Dist_60_Total_Map@data$M0_k4,
                  Dist_60_Total_Map@data$M0_k5,
                  Dist_60_Total_Map@data$M0_k6,
                  Dist_60_Total_Map@data$M0_k7,
                  Dist_60_Total_Map@data$M0_k8,
                  Dist_60_Total_Map@data$M0_k9)

M1_Total = switch(k_threshold,
                  Dist_60_Total_Map@data$M1_k1,
                  Dist_60_Total_Map@data$M1_k2,
                  Dist_60_Total_Map@data$M1_k3,
                  Dist_60_Total_Map@data$M1_k4,
                  Dist_60_Total_Map@data$M1_k5,
                  Dist_60_Total_Map@data$M1_k6,
                  Dist_60_Total_Map@data$M1_k7,
                  Dist_60_Total_Map@data$M1_k8,
                  Dist_60_Total_Map@data$M1_k9)

M2_Total = switch(k_threshold,
                  Dist_60_Total_Map@data$M2_k1,
                  Dist_60_Total_Map@data$M2_k2,
                  Dist_60_Total_Map@data$M2_k3,
                  Dist_60_Total_Map@data$M2_k4,
                  Dist_60_Total_Map@data$M2_k5,
                  Dist_60_Total_Map@data$M2_k6,
                  Dist_60_Total_Map@data$M2_k7,
                  Dist_60_Total_Map@data$M2_k8,
                  Dist_60_Total_Map@data$M2_k9)

# Variables for Urban data are created 
M0_Urban = switch(k_threshold,
                  Dist_60_Urban_Map@data$M0_k1,
                  Dist_60_Urban_Map@data$M0_k2,
                  Dist_60_Urban_Map@data$M0_k3,
                  Dist_60_Urban_Map@data$M0_k4,
                  Dist_60_Urban_Map@data$M0_k5,
                  Dist_60_Urban_Map@data$M0_k6,
                  Dist_60_Urban_Map@data$M0_k7,
                  Dist_60_Urban_Map@data$M0_k8,
                  Dist_60_Urban_Map@data$M0_k9)

M1_Urban = switch(k_threshold,
                  Dist_60_Urban_Map@data$M1_k1,
                  Dist_60_Urban_Map@data$M1_k2,
                  Dist_60_Urban_Map@data$M1_k3,
                  Dist_60_Urban_Map@data$M1_k4,
                  Dist_60_Urban_Map@data$M1_k5,
                  Dist_60_Urban_Map@data$M1_k6,
                  Dist_60_Urban_Map@data$M1_k7,
                  Dist_60_Urban_Map@data$M1_k8,
                  Dist_60_Urban_Map@data$M1_k9)

M2_Urban = switch(k_threshold,
                  Dist_60_Urban_Map@data$M2_k1,
                  Dist_60_Urban_Map@data$M2_k2,
                  Dist_60_Urban_Map@data$M2_k3,
                  Dist_60_Urban_Map@data$M2_k4,
                  Dist_60_Urban_Map@data$M2_k5,
                  Dist_60_Urban_Map@data$M2_k6,
                  Dist_60_Urban_Map@data$M2_k7,
                  Dist_60_Urban_Map@data$M2_k8,
                  Dist_60_Urban_Map@data$M2_k9)

# Variables for total data are created 
M0_Rural = switch(k_threshold,
                  Dist_60_Rural_Map@data$M0_k1,
                  Dist_60_Rural_Map@data$M0_k2,
                  Dist_60_Rural_Map@data$M0_k3,
                  Dist_60_Rural_Map@data$M0_k4,
                  Dist_60_Rural_Map@data$M0_k5,
                  Dist_60_Rural_Map@data$M0_k6,
                  Dist_60_Rural_Map@data$M0_k7,
                  Dist_60_Rural_Map@data$M0_k8,
                  Dist_60_Rural_Map@data$M0_k9)

M1_Rural = switch(k_threshold,
                  Dist_60_Rural_Map@data$M1_k1,
                  Dist_60_Rural_Map@data$M1_k2,
                  Dist_60_Rural_Map@data$M1_k3,
                  Dist_60_Rural_Map@data$M1_k4,
                  Dist_60_Rural_Map@data$M1_k5,
                  Dist_60_Rural_Map@data$M1_k6,
                  Dist_60_Rural_Map@data$M1_k7,
                  Dist_60_Rural_Map@data$M1_k8,
                  Dist_60_Rural_Map@data$M1_k9)

M2_Total = switch(k_threshold,
                  Dist_60_Rural_Map@data$M2_k1,
                  Dist_60_Rural_Map@data$M2_k2,
                  Dist_60_Rural_Map@data$M2_k3,
                  Dist_60_Rural_Map@data$M2_k4,
                  Dist_60_Rural_Map@data$M2_k5,
                  Dist_60_Rural_Map@data$M2_k6,
                  Dist_60_Rural_Map@data$M2_k7,
                  Dist_60_Rural_Map@data$M2_k8,
                  Dist_60_Rural_Map@data$M2_k9)

# This is the color palette used in the graphs
pal <- colorNumeric(
  palette = "viridis",
  domain = M0_Total,
  reverse = TRUE)

# We need to select how these variables affect M0, M1 and M2 by selecting
# which index to look at 1 = M0, 2 = M1, 3 = M2
mpi_selection = 1

# This allocates the total MPI Variables
education_max_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_education_max, Dist_60_Total_Map@data$M1_education_max,Dist_60_Total_Map@data$M2_education_max)
education_dropout_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_education_dropout, Dist_60_Total_Map@data$M1_education_dropout,Dist_60_Total_Map@data$M2_education_dropout)
health_chronic_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_health_chronic, Dist_60_Total_Map@data$M1_health_chronic,Dist_60_Total_Map@data$M2_health_chronic)
health_visit_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_health_visit, Dist_60_Total_Map@data$M1_health_visit,Dist_60_Total_Map@data$M2_health_visit)
employment_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_employment, Dist_60_Total_Map@data$M1_employment,Dist_60_Total_Map@data$M2_employment)
assets_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_assets, Dist_60_Total_Map@data$M1_assets,Dist_60_Total_Map@data$M2_assets)
services_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_services, Dist_60_Total_Map@data$M1_services,Dist_60_Total_Map@data$M2_services)
electricity_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_electricity, Dist_60_Total_Map@data$M1_electricity,Dist_60_Total_Map@data$M2_electricity)
cooking_fuel_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_cooking_fuel, Dist_60_Total_Map@data$M1_cooking_fuel,Dist_60_Total_Map@data$M2_cooking_fuel)
water_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_water, Dist_60_Total_Map@data$M1_water,Dist_60_Total_Map@data$M2_water)
toilet_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_toilet, Dist_60_Total_Map@data$M1_toilet,Dist_60_Total_Map@data$M2_toilet)
land_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_land, Dist_60_Total_Map@data$M1_land,Dist_60_Total_Map@data$M2_land)
livestock_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_livestock, Dist_60_Total_Map@data$M1_livestock,Dist_60_Total_Map@data$M2_livestock)
rural_equip_Total = switch(mpi_selection, Dist_60_Total_Map@data$M0_rural_equip, Dist_60_Total_Map@data$M1_rural_equip,Dist_60_Total_Map@data$M2_rural_equip)

# This allocates the urban MPI Variables
education_max_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_education_max, Dist_60_Urban_Map@data$M1_education_max,Dist_60_Urban_Map@data$M2_education_max)
education_dropout_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_education_dropout, Dist_60_Urban_Map@data$M1_education_dropout,Dist_60_Urban_Map@data$M2_education_dropout)
health_chronic_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_health_chronic, Dist_60_Urban_Map@data$M1_health_chronic,Dist_60_Urban_Map@data$M2_health_chronic)
health_visit_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_health_visit, Dist_60_Urban_Map@data$M1_health_visit,Dist_60_Urban_Map@data$M2_health_visit)
employment_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_employment, Dist_60_Urban_Map@data$M1_employment,Dist_60_Urban_Map@data$M2_employment)
assets_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_assets, Dist_60_Urban_Map@data$M1_assets,Dist_60_Urban_Map@data$M2_assets)
services_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_services, Dist_60_Urban_Map@data$M1_services,Dist_60_Urban_Map@data$M2_services)
electricity_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_electricity, Dist_60_Urban_Map@data$M1_electricity,Dist_60_Urban_Map@data$M2_electricity)
cooking_fuel_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_cooking_fuel, Dist_60_Urban_Map@data$M1_cooking_fuel,Dist_60_Urban_Map@data$M2_cooking_fuel)
water_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_water, Dist_60_Urban_Map@data$M1_water,Dist_60_Urban_Map@data$M2_water)
toilet_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_toilet, Dist_60_Urban_Map@data$M1_toilet,Dist_60_Urban_Map@data$M2_toilet)
land_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_land, Dist_60_Urban_Map@data$M1_land,Dist_60_Urban_Map@data$M2_land)
livestock_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_livestock, Dist_60_Urban_Map@data$M1_livestock,Dist_60_Urban_Map@data$M2_livestock)
rural_equip_Urban = switch(mpi_selection, Dist_60_Urban_Map@data$M0_rural_equip, Dist_60_Urban_Map@data$M1_rural_equip,Dist_60_Urban_Map@data$M2_rural_equip)

# This allocates the rural MPI variables
education_max_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_education_max, Dist_60_Rural_Map@data$M1_education_max,Dist_60_Rural_Map@data$M2_education_max)
education_dropout_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_education_dropout, Dist_60_Rural_Map@data$M1_education_dropout,Dist_60_Rural_Map@data$M2_education_dropout)
health_chronic_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_health_chronic, Dist_60_Rural_Map@data$M1_health_chronic,Dist_60_Rural_Map@data$M2_health_chronic)
health_visit_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_health_visit, Dist_60_Rural_Map@data$M1_health_visit,Dist_60_Rural_Map@data$M2_health_visit)
employment_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_employment, Dist_60_Rural_Map@data$M1_employment,Dist_60_Rural_Map@data$M2_employment)
assets_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_assets, Dist_60_Rural_Map@data$M1_assets,Dist_60_Rural_Map@data$M2_assets)
services_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_services, Dist_60_Rural_Map@data$M1_services,Dist_60_Rural_Map@data$M2_services)
electricity_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_electricity, Dist_60_Rural_Map@data$M1_electricity,Dist_60_Rural_Map@data$M2_electricity)
cooking_fuel_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_cooking_fuel, Dist_60_Rural_Map@data$M1_cooking_fuel,Dist_60_Rural_Map@data$M2_cooking_fuel)
water_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_water, Dist_60_Rural_Map@data$M1_water,Dist_60_Rural_Map@data$M2_water)
toilet_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_toilet, Dist_60_Rural_Map@data$M1_toilet,Dist_60_Rural_Map@data$M2_toilet)
land_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_land, Dist_60_Rural_Map@data$M1_land,Dist_60_Rural_Map@data$M2_land)
livestock_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_livestock, Dist_60_Rural_Map@data$M1_livestock,Dist_60_Rural_Map@data$M2_livestock)
rural_equip_Rural = switch(mpi_selection, Dist_60_Rural_Map@data$M0_rural_equip, Dist_60_Rural_Map@data$M1_rural_equip,Dist_60_Rural_Map@data$M2_rural_equip)


# We need to select how these variables affect M0, M1 and M2 by selecting
# which index to look at 1 = Total, 2 = Urban, 3 = Rural
urban_rural_selection = 1

M0 = switch(urban_rural_selection, M0_Total, M0_Urban, M0_Rural)
M1 = switch(urban_rural_selection, M1_Total, M1_Urban, M1_Rural)
M2 = switch(urban_rural_selection, M2_Total, M2_Urban, M2_Rural)

index = switch(mpi_selection, M0, M1, M2)
education_max = switch(urban_rural_selection, education_max_Total, education_max_Urban, education_max_Rural)
education_dropout = switch(urban_rural_selection, education_dropout_Total, education_dropout_Urban, education_dropout_Rural)
health_chronic = switch(urban_rural_selection, health_chronic_Total, health_chronic_Urban, health_chronic_Rural)
health_visit = switch(urban_rural_selection, health_visit_Total, health_visit_Urban, health_visit_Rural)
employment = switch(urban_rural_selection, employment_Total, employment_Urban, employment_Rural)
assets = switch(urban_rural_selection, assets_Total, assets_Urban, assets_Rural)
services = switch(urban_rural_selection, services_Total, services_Urban, services_Rural)
electricity = switch(urban_rural_selection, electricity_Total, electricity_Urban, electricity_Rural)
cooking_fuel = switch(urban_rural_selection, cooking_fuel_Total, cooking_fuel_Urban, cooking_fuel_Rural)
water = switch(urban_rural_selection, water_Total, water_Urban, water_Rural)
toilet = switch(urban_rural_selection, toilet_Total, toilet_Urban, toilet_Rural)
land = switch(urban_rural_selection, land_Total, land_Urban, land_Rural)
livestock = switch(urban_rural_selection, livestock_Total, livestock_Urban, livestock_Rural)
rural_equip = switch(urban_rural_selection, rural_equip_Total, rural_equip_Urban, rural_equip_Rural)

index_labels <- get_label(Dist_60_Total_Map@data$ADM2_EN, paste("M<sub>", mpi_selection - 1, "</sub>"), index, switch(k_threshold,
                                                                                            National_2017$M0_k1[1],
                                                                                            National_2017$M0_k2[1],
                                                                                            National_2017$M0_k3[1],
                                                                                            National_2017$M0_k4[1],
                                                                                            National_2017$M0_k5[1],
                                                                                            National_2017$M0_k6[1],
                                                                                            National_2017$M0_k7[1],
                                                                                            National_2017$M0_k8[1],
                                                                                            National_2017$M0_k9[1]))

## MAPPING----------------------------------------------------------------------
# This is where the map gets plotted 
leaflet(
  options = leafletOptions(
    minZoom = 0, maxZoom= 18,
    drag = FALSE)) %>% addTiles() %>%
  setView(lng = 30, lat=-19, zoom=6) %>% 
  get_polygon(Dist_60_Map, pal, index, index_labels, "Poverty Index") %>%
  clearControls() %>%
  addLayersControl(
    baseGroups = c("Poverty Index"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(pal = pal, values = education_max, opacity = 0.7, title = switch(k_threshold,
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



