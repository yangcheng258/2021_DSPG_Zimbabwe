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
colnames(Dist_60_Rural_2017)[2] <- "NAME_2"

# To avoid overlap in data, three different maps are created to host the rural, 
# urban and total MPI Data and decompositions 
Dist_60_Total_Map = Dist_60_Map
Dist_60_Urban_Map = Dist_60_Map
Dist_60_Rural_Map = Dist_60_Map

# Merges the shapefiles with the data csv files 
Dist_60_Total_Map@data = merge(Dist_60_Total_Map@data, Dist_60_Total_2017, by = c("NAME_2"), sort = FALSE)
Dist_60_Urban_Map@data = merge(Dist_60_Urban_Map@data, Dist_60_Urban_2017, by = c("NAME_2"), sort = FALSE)
Dist_60_Rural_Map@data = merge(Dist_60_Rural_Map@data, Dist_60_Rural_2017, by = c("NAME_2"), sort = FALSE)

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

M2_Rural = switch(k_threshold,
                  Dist_60_Rural_Map@data$M2_k1,
                  Dist_60_Rural_Map@data$M2_k2,
                  Dist_60_Rural_Map@data$M2_k3,
                  Dist_60_Rural_Map@data$M2_k4,
                  Dist_60_Rural_Map@data$M2_k5,
                  Dist_60_Rural_Map@data$M2_k6,
                  Dist_60_Rural_Map@data$M2_k7,
                  Dist_60_Rural_Map@data$M2_k8,
                  Dist_60_Rural_Map@data$M2_k9)

# We need to select how these variables affect M0, M1 and M2 by selecting
# which index to look at 1 = M0, 2 = M1, 3 = M2
mpi_selection = 3

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

# This is the color palette used in the graphs
pal <- colorNumeric(
  palette = "inferno",
  domain = c(0, 1),
  reverse = TRUE)


index_labels <- get_label(Dist_60_Total_Map@data$NAME_2, paste0("M<sub>", mpi_selection - 1, "</sub>"), index, switch(k_threshold,
                                                                                National_2017$M0_k1[1],
                                                                                National_2017$M0_k2[1],
                                                                                National_2017$M0_k3[1],
                                                                                National_2017$M0_k4[1],
                                                                                National_2017$M0_k5[1],
                                                                                National_2017$M0_k6[1],
                                                                                National_2017$M0_k7[1],
                                                                                National_2017$M0_k8[1],
                                                                                National_2017$M0_k9[1]))
education_max_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Max. Education", education_max, switch(mpi_selection,
                                                                                                         National_2017$M0_education_max[1],
                                                                                                         National_2017$M1_education_max[1],
                                                                                                         National_2017$M2_education_max[1]))
education_dropout_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Education Dropout", education_dropout, switch(mpi_selection,
                                                                                                         National_2017$M0_education_dropout[1],
                                                                                                         National_2017$M1_education_dropout[1],
                                                                                                         National_2017$M2_education_dropout[1]))
health_chronic_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Chronic Illness", health_chronic, switch(mpi_selection,
                                                                                                         National_2017$M0_health_chronic[1],
                                                                                                         National_2017$M1_health_chronic[1],
                                                                                                         National_2017$M2_health_chronic[1]))
health_visit_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Lack of Health Visit", health_visit, switch(mpi_selection,
                                                                                                           National_2017$M0_health_visit[1],
                                                                                                           National_2017$M1_health_visit[1],
                                                                                                           National_2017$M2_health_visit[1]))
employment_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Unemployment", employment, switch(mpi_selection,
                                                                                                           National_2017$M0_employment[1],
                                                                                                           National_2017$M1_employment[1],
                                                                                                           National_2017$M2_employment[1]))
assets_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Household Assets", assets, switch(mpi_selection,
                                                                                                           National_2017$M0_assets[1],
                                                                                                           National_2017$M1_assets[1],
                                                                                                           National_2017$M2_assets[1]))
services_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Access to Services", services, switch(mpi_selection,
                                                                                                           National_2017$M0_services[1],
                                                                                                           National_2017$M1_services[1],
                                                                                                           National_2017$M2_services[1]))
electricity_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Lack of Electricity", electricity, switch(mpi_selection,
                                                                                                           National_2017$M0_electricity[1],
                                                                                                           National_2017$M1_electricity[1],
                                                                                                           National_2017$M2_electricity[1]))
cooking_fuel_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Poor Cooking Fuel", cooking_fuel, switch(mpi_selection,
                                                                                                           National_2017$M0_cooking_fuel[1],
                                                                                                           National_2017$M1_cooking_fuel[1],
                                                                                                           National_2017$M2_cooking_fuel[1]))
water_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Poor Water Source", water, switch(mpi_selection,
                                                                                                           National_2017$M0_water[1],
                                                                                                           National_2017$M1_water[1],
                                                                                                           National_2017$M2_water[1]))
toilet_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Lack of Toilet", toilet, switch(mpi_selection,
                                                                                                           National_2017$M0_toilet[1],
                                                                                                           National_2017$M1_toilet[1],
                                                                                                           National_2017$M2_toilet[1]))
land_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Lack of Land", land, switch(mpi_selection,
                                                                                                           National_2017$M0_land[1],
                                                                                                           National_2017$M1_land[1],
                                                                                                           National_2017$M2_land[1]))
livestock_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Lack of Livestock", livestock, switch(mpi_selection,
                                                                                                           National_2017$M0_livestock[1],
                                                                                                           National_2017$M1_livestock[1],
                                                                                                           National_2017$M2_livestock[1]))
rural_equip_labels <- get_label(Dist_60_Total_Map@data$NAME_2, "Lack of Rural Equipment", rural_equip, switch(mpi_selection,
                                                                                                           National_2017$M0_rural_equip[1],
                                                                                                           National_2017$M1_rural_equip[1],
                                                                                                           National_2017$M2_rural_equip[1]))

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
  get_polygon(Dist_60_Total_Map, pal, index, index_labels, "Poverty Index") %>%
  get_polygon(Dist_60_Total_Map, pal, education_max, education_max_labels, "Max. Education") %>%
  get_polygon(Dist_60_Total_Map, pal, education_dropout, education_dropout_labels, "Education Dropout") %>%
  get_polygon(Dist_60_Total_Map, pal, health_chronic, health_chronic_labels, "Chronic Illness") %>%
  get_polygon(Dist_60_Total_Map, pal, health_visit, health_visit_labels, "Lack of Health Visit") %>%
  get_polygon(Dist_60_Total_Map, pal, employment, employment_labels, "Unemployment") %>%
  get_polygon(Dist_60_Total_Map, pal, assets, assets_labels, "Lack of Household Assets") %>%
  get_polygon(Dist_60_Total_Map, pal, services, services_labels, "Lack of Access to Services") %>%
  get_polygon(Dist_60_Total_Map, pal, electricity, electricity_labels, "Lack of Electricity") %>%
  get_polygon(Dist_60_Total_Map, pal, cooking_fuel, cooking_fuel_labels, "Poor Cooking Fuel") %>%
  get_polygon(Dist_60_Total_Map, pal, water, water_labels, "Poor Water Source") %>%
  get_polygon(Dist_60_Total_Map, pal, toilet, toilet_labels, "Lack of Toilet") %>%
  get_polygon(Dist_60_Total_Map, pal, land, land_labels, "Lack of Land") %>%
  get_polygon(Dist_60_Total_Map, pal, livestock, livestock_labels, "Lack of Livestock") %>%
  get_polygon(Dist_60_Total_Map, pal, rural_equip, rural_equip_labels, "Lack of Rural Equipment") %>%
  clearControls() %>%
  addLayersControl(
  baseGroups = c("Poverty Index", 
                 "Max. Education", 
                 "Education Dropout", 
                 "Chronic Illness", 
                 "Lack of Health Visit", 
                 "Unemployment", 
                 "Lack of Household Assets", 
                 "Lack of Access to Services", 
                 "Lack of Electricity", 
                 "Poor Cooking Fuel",
                 "Poor Water Source",
                 "Lack of Toilet",
                 "Lack of Land",
                 "Lack of Livestock",
                 "Lack of Rural Equipment"),
  options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend(pal = pal, values = c(0, 1), opacity = 0.7, title = paste0("Legend (with k = ", k_threshold, ")"),
            na.label = "No Data",
            group = c("Poverty Index", "Max. Education"),
            position = "bottomleft") %>%
  htmlwidgets::prependContent(html_fix)
