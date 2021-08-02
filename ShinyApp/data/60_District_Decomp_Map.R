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

# Loads the shapefile
Dist_60_Map <- readOGR(dsn = paste0(getwd(),"/Shapefiles/60DistrictShapefiles"), layer="gadm36_ZWE_2")

# Loads the district data
Dist_60_Total_2017 = read.csv("MappingData/OriginalMPI/2017/2017_District.csv")
Dist_60_Urban_2017 = read.csv("MappingData/OriginalMPI/2017/2017_District_Urban.csv")
Dist_60_Rural_2017 = read.csv("MappingData/OriginalMPI/2017/2017_District_Rural.csv")

# Loads the national data
National_2017 = read.csv(file = "MappingData/OriginalMPI/2017/2017_National.csv")
National_Urban_2017 = read.csv(file = "MappingData/OriginalMPI/2017/2017_National_Urban.csv")
National_Rural_2017 = read.csv(file = "MappingData/OriginalMPI/2017/2017_National_Rural.csv")



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

# This is the level that the decomposition data is selected on. 
# 1 = g0, c0, 2 = g1, c1, 3 = g2, c2
level_selection = 1

# 1 = Total, 2 = Urban, 3 = Rural
UrbRurSelection = 1

map = switch(UrbRurSelection,
             Dist_60_Total_Map,
             Dist_60_Urban_Map,
             Dist_60_Rural_Map)
nat_data = switch(UrbRurSelection,
                  National_2017,
                  National_Urban_2017,
                  National_Rural_2017)


g_edu_max = switch(k_threshold,
                   switch(level_selection,  map@data$g0_edu_max_k1,map@data$g1_edu_max_k1,map@data$g2_edu_max_k1),
                   switch(level_selection,  map@data$g0_edu_max_k2,map@data$g1_edu_max_k2,map@data$g2_edu_max_k2),
                   switch(level_selection,  map@data$g0_edu_max_k3,map@data$g1_edu_max_k3,map@data$g2_edu_max_k3),
                   switch(level_selection,  map@data$g0_edu_max_k4,map@data$g1_edu_max_k4,map@data$g2_edu_max_k4),
                   switch(level_selection,  map@data$g0_edu_max_k5,map@data$g1_edu_max_k5,map@data$g2_edu_max_k5),
                   switch(level_selection,  map@data$g0_edu_max_k6,map@data$g1_edu_max_k6,map@data$g2_edu_max_k6),
                   switch(level_selection,  map@data$g0_edu_max_k7,map@data$g1_edu_max_k7,map@data$g2_edu_max_k7),
                   switch(level_selection,  map@data$g0_edu_max_k8,map@data$g1_edu_max_k8,map@data$g2_edu_max_k8),
                   switch(level_selection,  map@data$g0_edu_max_k9,map@data$g1_edu_max_k9,map@data$g2_edu_max_k9)
)
g_edu_dropout = switch(k_threshold,
                       switch(level_selection,  map@data$g0_edu_dropout_k1,map@data$g1_edu_dropout_k1,map@data$g2_edu_dropout_k1),
                       switch(level_selection,  map@data$g0_edu_dropout_k2,map@data$g1_edu_dropout_k2,map@data$g2_edu_dropout_k2),
                       switch(level_selection,  map@data$g0_edu_dropout_k3,map@data$g1_edu_dropout_k3,map@data$g2_edu_dropout_k3),
                       switch(level_selection,  map@data$g0_edu_dropout_k4,map@data$g1_edu_dropout_k4,map@data$g2_edu_dropout_k4),
                       switch(level_selection,  map@data$g0_edu_dropout_k5,map@data$g1_edu_dropout_k5,map@data$g2_edu_dropout_k5),
                       switch(level_selection,  map@data$g0_edu_dropout_k6,map@data$g1_edu_dropout_k6,map@data$g2_edu_dropout_k6),
                       switch(level_selection,  map@data$g0_edu_dropout_k7,map@data$g1_edu_dropout_k7,map@data$g2_edu_dropout_k7),
                       switch(level_selection,  map@data$g0_edu_dropout_k8,map@data$g1_edu_dropout_k8,map@data$g2_edu_dropout_k8),
                       switch(level_selection,  map@data$g0_edu_dropout_k9,map@data$g1_edu_dropout_k9,map@data$g2_edu_dropout_k9)
)

g_hea_chronic = switch(k_threshold,
                       switch(level_selection,  map@data$g0_hea_chronic_k1,map@data$g1_hea_chronic_k1,map@data$g2_hea_chronic_k1),
                       switch(level_selection,  map@data$g0_hea_chronic_k2,map@data$g1_hea_chronic_k2,map@data$g2_hea_chronic_k2),
                       switch(level_selection,  map@data$g0_hea_chronic_k3,map@data$g1_hea_chronic_k3,map@data$g2_hea_chronic_k3),
                       switch(level_selection,  map@data$g0_hea_chronic_k4,map@data$g1_hea_chronic_k4,map@data$g2_hea_chronic_k4),
                       switch(level_selection,  map@data$g0_hea_chronic_k5,map@data$g1_hea_chronic_k5,map@data$g2_hea_chronic_k5),
                       switch(level_selection,  map@data$g0_hea_chronic_k6,map@data$g1_hea_chronic_k6,map@data$g2_hea_chronic_k6),
                       switch(level_selection,  map@data$g0_hea_chronic_k7,map@data$g1_hea_chronic_k7,map@data$g2_hea_chronic_k7),
                       switch(level_selection,  map@data$g0_hea_chronic_k8,map@data$g1_hea_chronic_k8,map@data$g2_hea_chronic_k8),
                       switch(level_selection,  map@data$g0_hea_chronic_k9,map@data$g1_hea_chronic_k9,map@data$g2_hea_chronic_k9)
)

g_hea_visit = switch(k_threshold,
                     switch(level_selection,  map@data$g0_hea_visit_k1,map@data$g1_hea_visit_k1,map@data$g2_hea_visit_k1),
                     switch(level_selection,  map@data$g0_hea_visit_k2,map@data$g1_hea_visit_k2,map@data$g2_hea_visit_k2),
                     switch(level_selection,  map@data$g0_hea_visit_k3,map@data$g1_hea_visit_k3,map@data$g2_hea_visit_k3),
                     switch(level_selection,  map@data$g0_hea_visit_k4,map@data$g1_hea_visit_k4,map@data$g2_hea_visit_k4),
                     switch(level_selection,  map@data$g0_hea_visit_k5,map@data$g1_hea_visit_k5,map@data$g2_hea_visit_k5),
                     switch(level_selection,  map@data$g0_hea_visit_k6,map@data$g1_hea_visit_k6,map@data$g2_hea_visit_k6),
                     switch(level_selection,  map@data$g0_hea_visit_k7,map@data$g1_hea_visit_k7,map@data$g2_hea_visit_k7),
                     switch(level_selection,  map@data$g0_hea_visit_k8,map@data$g1_hea_visit_k8,map@data$g2_hea_visit_k8),
                     switch(level_selection,  map@data$g0_hea_visit_k9,map@data$g1_hea_visit_k9,map@data$g2_hea_visit_k9)
)

g_employment = switch(k_threshold,
                      switch(level_selection,  map@data$g0_employment_k1,map@data$g1_employment_k1,map@data$g2_employment_k1),
                      switch(level_selection,  map@data$g0_employment_k2,map@data$g1_employment_k2,map@data$g2_employment_k2),
                      switch(level_selection,  map@data$g0_employment_k3,map@data$g1_employment_k3,map@data$g2_employment_k3),
                      switch(level_selection,  map@data$g0_employment_k4,map@data$g1_employment_k4,map@data$g2_employment_k4),
                      switch(level_selection,  map@data$g0_employment_k5,map@data$g1_employment_k5,map@data$g2_employment_k5),
                      switch(level_selection,  map@data$g0_employment_k6,map@data$g1_employment_k6,map@data$g2_employment_k6),
                      switch(level_selection,  map@data$g0_employment_k7,map@data$g1_employment_k7,map@data$g2_employment_k7),
                      switch(level_selection,  map@data$g0_employment_k8,map@data$g1_employment_k8,map@data$g2_employment_k8),
                      switch(level_selection,  map@data$g0_employment_k9,map@data$g1_employment_k9,map@data$g2_employment_k9)
)

g_assets = switch(k_threshold,
                  switch(level_selection,  map@data$g0_assets_k1,map@data$g1_assets_k1,map@data$g2_assets_k1),
                  switch(level_selection,  map@data$g0_assets_k2,map@data$g1_assets_k2,map@data$g2_assets_k2),
                  switch(level_selection,  map@data$g0_assets_k3,map@data$g1_assets_k3,map@data$g2_assets_k3),
                  switch(level_selection,  map@data$g0_assets_k4,map@data$g1_assets_k4,map@data$g2_assets_k4),
                  switch(level_selection,  map@data$g0_assets_k5,map@data$g1_assets_k5,map@data$g2_assets_k5),
                  switch(level_selection,  map@data$g0_assets_k6,map@data$g1_assets_k6,map@data$g2_assets_k6),
                  switch(level_selection,  map@data$g0_assets_k7,map@data$g1_assets_k7,map@data$g2_assets_k7),
                  switch(level_selection,  map@data$g0_assets_k8,map@data$g1_assets_k8,map@data$g2_assets_k8),
                  switch(level_selection,  map@data$g0_assets_k9,map@data$g1_assets_k9,map@data$g2_assets_k9)
)

g_services = switch(k_threshold,
                    switch(level_selection,  map@data$g0_services_k1,map@data$g1_services_k1,map@data$g2_services_k1),
                    switch(level_selection,  map@data$g0_services_k2,map@data$g1_services_k2,map@data$g2_services_k2),
                    switch(level_selection,  map@data$g0_services_k3,map@data$g1_services_k3,map@data$g2_services_k3),
                    switch(level_selection,  map@data$g0_services_k4,map@data$g1_services_k4,map@data$g2_services_k4),
                    switch(level_selection,  map@data$g0_services_k5,map@data$g1_services_k5,map@data$g2_services_k5),
                    switch(level_selection,  map@data$g0_services_k6,map@data$g1_services_k6,map@data$g2_services_k6),
                    switch(level_selection,  map@data$g0_services_k7,map@data$g1_services_k7,map@data$g2_services_k7),
                    switch(level_selection,  map@data$g0_services_k8,map@data$g1_services_k8,map@data$g2_services_k8),
                    switch(level_selection,  map@data$g0_services_k9,map@data$g1_services_k9,map@data$g2_services_k9)
)

g_electricity = switch(k_threshold,
                       switch(level_selection,  map@data$g0_electricity_k1,map@data$g1_electricity_k1,map@data$g2_electricity_k1),
                       switch(level_selection,  map@data$g0_electricity_k2,map@data$g1_electricity_k2,map@data$g2_electricity_k2),
                       switch(level_selection,  map@data$g0_electricity_k3,map@data$g1_electricity_k3,map@data$g2_electricity_k3),
                       switch(level_selection,  map@data$g0_electricity_k4,map@data$g1_electricity_k4,map@data$g2_electricity_k4),
                       switch(level_selection,  map@data$g0_electricity_k5,map@data$g1_electricity_k5,map@data$g2_electricity_k5),
                       switch(level_selection,  map@data$g0_electricity_k6,map@data$g1_electricity_k6,map@data$g2_electricity_k6),
                       switch(level_selection,  map@data$g0_electricity_k7,map@data$g1_electricity_k7,map@data$g2_electricity_k7),
                       switch(level_selection,  map@data$g0_electricity_k8,map@data$g1_electricity_k8,map@data$g2_electricity_k8),
                       switch(level_selection,  map@data$g0_electricity_k9,map@data$g1_electricity_k9,map@data$g2_electricity_k9)
)

g_cooking_fuel = switch(k_threshold,
                        switch(level_selection,  map@data$g0_cooking_fuel_k1,map@data$g1_cooking_fuel_k1,map@data$g2_cooking_fuel_k1),
                        switch(level_selection,  map@data$g0_cooking_fuel_k2,map@data$g1_cooking_fuel_k2,map@data$g2_cooking_fuel_k2),
                        switch(level_selection,  map@data$g0_cooking_fuel_k3,map@data$g1_cooking_fuel_k3,map@data$g2_cooking_fuel_k3),
                        switch(level_selection,  map@data$g0_cooking_fuel_k4,map@data$g1_cooking_fuel_k4,map@data$g2_cooking_fuel_k4),
                        switch(level_selection,  map@data$g0_cooking_fuel_k5,map@data$g1_cooking_fuel_k5,map@data$g2_cooking_fuel_k5),
                        switch(level_selection,  map@data$g0_cooking_fuel_k6,map@data$g1_cooking_fuel_k6,map@data$g2_cooking_fuel_k6),
                        switch(level_selection,  map@data$g0_cooking_fuel_k7,map@data$g1_cooking_fuel_k7,map@data$g2_cooking_fuel_k7),
                        switch(level_selection,  map@data$g0_cooking_fuel_k8,map@data$g1_cooking_fuel_k8,map@data$g2_cooking_fuel_k8),
                        switch(level_selection,  map@data$g0_cooking_fuel_k9,map@data$g1_cooking_fuel_k9,map@data$g2_cooking_fuel_k9)
)

g_water = switch(k_threshold,
                 switch(level_selection,  map@data$g0_water_k1,map@data$g1_water_k1,map@data$g2_water_k1),
                 switch(level_selection,  map@data$g0_water_k2,map@data$g1_water_k2,map@data$g2_water_k2),
                 switch(level_selection,  map@data$g0_water_k3,map@data$g1_water_k3,map@data$g2_water_k3),
                 switch(level_selection,  map@data$g0_water_k4,map@data$g1_water_k4,map@data$g2_water_k4),
                 switch(level_selection,  map@data$g0_water_k5,map@data$g1_water_k5,map@data$g2_water_k5),
                 switch(level_selection,  map@data$g0_water_k6,map@data$g1_water_k6,map@data$g2_water_k6),
                 switch(level_selection,  map@data$g0_water_k7,map@data$g1_water_k7,map@data$g2_water_k7),
                 switch(level_selection,  map@data$g0_water_k8,map@data$g1_water_k8,map@data$g2_water_k8),
                 switch(level_selection,  map@data$g0_water_k9,map@data$g1_water_k9,map@data$g2_water_k9)
)

g_toilet = switch(k_threshold,
                  switch(level_selection,  map@data$g0_toilet_k1,map@data$g1_toilet_k1,map@data$g2_toilet_k1),
                  switch(level_selection,  map@data$g0_toilet_k2,map@data$g1_toilet_k2,map@data$g2_toilet_k2),
                  switch(level_selection,  map@data$g0_toilet_k3,map@data$g1_toilet_k3,map@data$g2_toilet_k3),
                  switch(level_selection,  map@data$g0_toilet_k4,map@data$g1_toilet_k4,map@data$g2_toilet_k4),
                  switch(level_selection,  map@data$g0_toilet_k5,map@data$g1_toilet_k5,map@data$g2_toilet_k5),
                  switch(level_selection,  map@data$g0_toilet_k6,map@data$g1_toilet_k6,map@data$g2_toilet_k6),
                  switch(level_selection,  map@data$g0_toilet_k7,map@data$g1_toilet_k7,map@data$g2_toilet_k7),
                  switch(level_selection,  map@data$g0_toilet_k8,map@data$g1_toilet_k8,map@data$g2_toilet_k8),
                  switch(level_selection,  map@data$g0_toilet_k9,map@data$g1_toilet_k9,map@data$g2_toilet_k9)
)

g_land = switch(k_threshold,
                switch(level_selection,  map@data$g0_land_k1,map@data$g1_land_k1,map@data$g2_land_k1),
                switch(level_selection,  map@data$g0_land_k2,map@data$g1_land_k2,map@data$g2_land_k2),
                switch(level_selection,  map@data$g0_land_k3,map@data$g1_land_k3,map@data$g2_land_k3),
                switch(level_selection,  map@data$g0_land_k4,map@data$g1_land_k4,map@data$g2_land_k4),
                switch(level_selection,  map@data$g0_land_k5,map@data$g1_land_k5,map@data$g2_land_k5),
                switch(level_selection,  map@data$g0_land_k6,map@data$g1_land_k6,map@data$g2_land_k6),
                switch(level_selection,  map@data$g0_land_k7,map@data$g1_land_k7,map@data$g2_land_k7),
                switch(level_selection,  map@data$g0_land_k8,map@data$g1_land_k8,map@data$g2_land_k8),
                switch(level_selection,  map@data$g0_land_k9,map@data$g1_land_k9,map@data$g2_land_k9)
)

g_livestock = switch(k_threshold,
                     switch(level_selection,  map@data$g0_livestock_k1,map@data$g1_livestock_k1,map@data$g2_livestock_k1),
                     switch(level_selection,  map@data$g0_livestock_k2,map@data$g1_livestock_k2,map@data$g2_livestock_k2),
                     switch(level_selection,  map@data$g0_livestock_k3,map@data$g1_livestock_k3,map@data$g2_livestock_k3),
                     switch(level_selection,  map@data$g0_livestock_k4,map@data$g1_livestock_k4,map@data$g2_livestock_k4),
                     switch(level_selection,  map@data$g0_livestock_k5,map@data$g1_livestock_k5,map@data$g2_livestock_k5),
                     switch(level_selection,  map@data$g0_livestock_k6,map@data$g1_livestock_k6,map@data$g2_livestock_k6),
                     switch(level_selection,  map@data$g0_livestock_k7,map@data$g1_livestock_k7,map@data$g2_livestock_k7),
                     switch(level_selection,  map@data$g0_livestock_k8,map@data$g1_livestock_k8,map@data$g2_livestock_k8),
                     switch(level_selection,  map@data$g0_livestock_k9,map@data$g1_livestock_k9,map@data$g2_livestock_k9)
)

g_rural_equip = switch(k_threshold,
                       switch(level_selection,  map@data$g0_rural_equip_k1,map@data$g1_rural_equip_k1,map@data$g2_rural_equip_k1),
                       switch(level_selection,  map@data$g0_rural_equip_k2,map@data$g1_rural_equip_k2,map@data$g2_rural_equip_k2),
                       switch(level_selection,  map@data$g0_rural_equip_k3,map@data$g1_rural_equip_k3,map@data$g2_rural_equip_k3),
                       switch(level_selection,  map@data$g0_rural_equip_k4,map@data$g1_rural_equip_k4,map@data$g2_rural_equip_k4),
                       switch(level_selection,  map@data$g0_rural_equip_k5,map@data$g1_rural_equip_k5,map@data$g2_rural_equip_k5),
                       switch(level_selection,  map@data$g0_rural_equip_k6,map@data$g1_rural_equip_k6,map@data$g2_rural_equip_k6),
                       switch(level_selection,  map@data$g0_rural_equip_k7,map@data$g1_rural_equip_k7,map@data$g2_rural_equip_k7),
                       switch(level_selection,  map@data$g0_rural_equip_k8,map@data$g1_rural_equip_k8,map@data$g2_rural_equip_k8),
                       switch(level_selection,  map@data$g0_rural_equip_k9,map@data$g1_rural_equip_k9,map@data$g2_rural_equip_k9)
)





c_edu_max = switch(k_threshold,
                   switch(level_selection,  map@data$c0_edu_max_k1,map@data$c1_edu_max_k1,map@data$c2_edu_max_k1),
                   switch(level_selection,  map@data$c0_edu_max_k2,map@data$c1_edu_max_k2,map@data$c2_edu_max_k2),
                   switch(level_selection,  map@data$c0_edu_max_k3,map@data$c1_edu_max_k3,map@data$c2_edu_max_k3),
                   switch(level_selection,  map@data$c0_edu_max_k4,map@data$c1_edu_max_k4,map@data$c2_edu_max_k4),
                   switch(level_selection,  map@data$c0_edu_max_k5,map@data$c1_edu_max_k5,map@data$c2_edu_max_k5),
                   switch(level_selection,  map@data$c0_edu_max_k6,map@data$c1_edu_max_k6,map@data$c2_edu_max_k6),
                   switch(level_selection,  map@data$c0_edu_max_k7,map@data$c1_edu_max_k7,map@data$c2_edu_max_k7),
                   switch(level_selection,  map@data$c0_edu_max_k8,map@data$c1_edu_max_k8,map@data$c2_edu_max_k8),
                   switch(level_selection,  map@data$c0_edu_max_k9,map@data$c1_edu_max_k9,map@data$c2_edu_max_k9)
)
c_edu_dropout = switch(k_threshold,
                       switch(level_selection,  map@data$c0_edu_dropout_k1,map@data$c1_edu_dropout_k1,map@data$c2_edu_dropout_k1),
                       switch(level_selection,  map@data$c0_edu_dropout_k2,map@data$c1_edu_dropout_k2,map@data$c2_edu_dropout_k2),
                       switch(level_selection,  map@data$c0_edu_dropout_k3,map@data$c1_edu_dropout_k3,map@data$c2_edu_dropout_k3),
                       switch(level_selection,  map@data$c0_edu_dropout_k4,map@data$c1_edu_dropout_k4,map@data$c2_edu_dropout_k4),
                       switch(level_selection,  map@data$c0_edu_dropout_k5,map@data$c1_edu_dropout_k5,map@data$c2_edu_dropout_k5),
                       switch(level_selection,  map@data$c0_edu_dropout_k6,map@data$c1_edu_dropout_k6,map@data$c2_edu_dropout_k6),
                       switch(level_selection,  map@data$c0_edu_dropout_k7,map@data$c1_edu_dropout_k7,map@data$c2_edu_dropout_k7),
                       switch(level_selection,  map@data$c0_edu_dropout_k8,map@data$c1_edu_dropout_k8,map@data$c2_edu_dropout_k8),
                       switch(level_selection,  map@data$c0_edu_dropout_k9,map@data$c1_edu_dropout_k9,map@data$c2_edu_dropout_k9)
)

c_hea_chronic = switch(k_threshold,
                       switch(level_selection,  map@data$c0_hea_chronic_k1,map@data$c1_hea_chronic_k1,map@data$c2_hea_chronic_k1),
                       switch(level_selection,  map@data$c0_hea_chronic_k2,map@data$c1_hea_chronic_k2,map@data$c2_hea_chronic_k2),
                       switch(level_selection,  map@data$c0_hea_chronic_k3,map@data$c1_hea_chronic_k3,map@data$c2_hea_chronic_k3),
                       switch(level_selection,  map@data$c0_hea_chronic_k4,map@data$c1_hea_chronic_k4,map@data$c2_hea_chronic_k4),
                       switch(level_selection,  map@data$c0_hea_chronic_k5,map@data$c1_hea_chronic_k5,map@data$c2_hea_chronic_k5),
                       switch(level_selection,  map@data$c0_hea_chronic_k6,map@data$c1_hea_chronic_k6,map@data$c2_hea_chronic_k6),
                       switch(level_selection,  map@data$c0_hea_chronic_k7,map@data$c1_hea_chronic_k7,map@data$c2_hea_chronic_k7),
                       switch(level_selection,  map@data$c0_hea_chronic_k8,map@data$c1_hea_chronic_k8,map@data$c2_hea_chronic_k8),
                       switch(level_selection,  map@data$c0_hea_chronic_k9,map@data$c1_hea_chronic_k9,map@data$c2_hea_chronic_k9)
)

c_hea_visit = switch(k_threshold,
                     switch(level_selection,  map@data$c0_hea_visit_k1,map@data$c1_hea_visit_k1,map@data$c2_hea_visit_k1),
                     switch(level_selection,  map@data$c0_hea_visit_k2,map@data$c1_hea_visit_k2,map@data$c2_hea_visit_k2),
                     switch(level_selection,  map@data$c0_hea_visit_k3,map@data$c1_hea_visit_k3,map@data$c2_hea_visit_k3),
                     switch(level_selection,  map@data$c0_hea_visit_k4,map@data$c1_hea_visit_k4,map@data$c2_hea_visit_k4),
                     switch(level_selection,  map@data$c0_hea_visit_k5,map@data$c1_hea_visit_k5,map@data$c2_hea_visit_k5),
                     switch(level_selection,  map@data$c0_hea_visit_k6,map@data$c1_hea_visit_k6,map@data$c2_hea_visit_k6),
                     switch(level_selection,  map@data$c0_hea_visit_k7,map@data$c1_hea_visit_k7,map@data$c2_hea_visit_k7),
                     switch(level_selection,  map@data$c0_hea_visit_k8,map@data$c1_hea_visit_k8,map@data$c2_hea_visit_k8),
                     switch(level_selection,  map@data$c0_hea_visit_k9,map@data$c1_hea_visit_k9,map@data$c2_hea_visit_k9)
)

c_employment = switch(k_threshold,
                      switch(level_selection,  map@data$c0_employment_k1,map@data$c1_employment_k1,map@data$c2_employment_k1),
                      switch(level_selection,  map@data$c0_employment_k2,map@data$c1_employment_k2,map@data$c2_employment_k2),
                      switch(level_selection,  map@data$c0_employment_k3,map@data$c1_employment_k3,map@data$c2_employment_k3),
                      switch(level_selection,  map@data$c0_employment_k4,map@data$c1_employment_k4,map@data$c2_employment_k4),
                      switch(level_selection,  map@data$c0_employment_k5,map@data$c1_employment_k5,map@data$c2_employment_k5),
                      switch(level_selection,  map@data$c0_employment_k6,map@data$c1_employment_k6,map@data$c2_employment_k6),
                      switch(level_selection,  map@data$c0_employment_k7,map@data$c1_employment_k7,map@data$c2_employment_k7),
                      switch(level_selection,  map@data$c0_employment_k8,map@data$c1_employment_k8,map@data$c2_employment_k8),
                      switch(level_selection,  map@data$c0_employment_k9,map@data$c1_employment_k9,map@data$c2_employment_k9)
)

c_assets = switch(k_threshold,
                  switch(level_selection,  map@data$c0_assets_k1,map@data$c1_assets_k1,map@data$c2_assets_k1),
                  switch(level_selection,  map@data$c0_assets_k2,map@data$c1_assets_k2,map@data$c2_assets_k2),
                  switch(level_selection,  map@data$c0_assets_k3,map@data$c1_assets_k3,map@data$c2_assets_k3),
                  switch(level_selection,  map@data$c0_assets_k4,map@data$c1_assets_k4,map@data$c2_assets_k4),
                  switch(level_selection,  map@data$c0_assets_k5,map@data$c1_assets_k5,map@data$c2_assets_k5),
                  switch(level_selection,  map@data$c0_assets_k6,map@data$c1_assets_k6,map@data$c2_assets_k6),
                  switch(level_selection,  map@data$c0_assets_k7,map@data$c1_assets_k7,map@data$c2_assets_k7),
                  switch(level_selection,  map@data$c0_assets_k8,map@data$c1_assets_k8,map@data$c2_assets_k8),
                  switch(level_selection,  map@data$c0_assets_k9,map@data$c1_assets_k9,map@data$c2_assets_k9)
)

c_services = switch(k_threshold,
                    switch(level_selection,  map@data$c0_services_k1,map@data$c1_services_k1,map@data$c2_services_k1),
                    switch(level_selection,  map@data$c0_services_k2,map@data$c1_services_k2,map@data$c2_services_k2),
                    switch(level_selection,  map@data$c0_services_k3,map@data$c1_services_k3,map@data$c2_services_k3),
                    switch(level_selection,  map@data$c0_services_k4,map@data$c1_services_k4,map@data$c2_services_k4),
                    switch(level_selection,  map@data$c0_services_k5,map@data$c1_services_k5,map@data$c2_services_k5),
                    switch(level_selection,  map@data$c0_services_k6,map@data$c1_services_k6,map@data$c2_services_k6),
                    switch(level_selection,  map@data$c0_services_k7,map@data$c1_services_k7,map@data$c2_services_k7),
                    switch(level_selection,  map@data$c0_services_k8,map@data$c1_services_k8,map@data$c2_services_k8),
                    switch(level_selection,  map@data$c0_services_k9,map@data$c1_services_k9,map@data$c2_services_k9)
)

c_electricity = switch(k_threshold,
                       switch(level_selection,  map@data$c0_electricity_k1,map@data$c1_electricity_k1,map@data$c2_electricity_k1),
                       switch(level_selection,  map@data$c0_electricity_k2,map@data$c1_electricity_k2,map@data$c2_electricity_k2),
                       switch(level_selection,  map@data$c0_electricity_k3,map@data$c1_electricity_k3,map@data$c2_electricity_k3),
                       switch(level_selection,  map@data$c0_electricity_k4,map@data$c1_electricity_k4,map@data$c2_electricity_k4),
                       switch(level_selection,  map@data$c0_electricity_k5,map@data$c1_electricity_k5,map@data$c2_electricity_k5),
                       switch(level_selection,  map@data$c0_electricity_k6,map@data$c1_electricity_k6,map@data$c2_electricity_k6),
                       switch(level_selection,  map@data$c0_electricity_k7,map@data$c1_electricity_k7,map@data$c2_electricity_k7),
                       switch(level_selection,  map@data$c0_electricity_k8,map@data$c1_electricity_k8,map@data$c2_electricity_k8),
                       switch(level_selection,  map@data$c0_electricity_k9,map@data$c1_electricity_k9,map@data$c2_electricity_k9)
)

c_cooking_fuel = switch(k_threshold,
                        switch(level_selection,  map@data$c0_cooking_fuel_k1,map@data$c1_cooking_fuel_k1,map@data$c2_cooking_fuel_k1),
                        switch(level_selection,  map@data$c0_cooking_fuel_k2,map@data$c1_cooking_fuel_k2,map@data$c2_cooking_fuel_k2),
                        switch(level_selection,  map@data$c0_cooking_fuel_k3,map@data$c1_cooking_fuel_k3,map@data$c2_cooking_fuel_k3),
                        switch(level_selection,  map@data$c0_cooking_fuel_k4,map@data$c1_cooking_fuel_k4,map@data$c2_cooking_fuel_k4),
                        switch(level_selection,  map@data$c0_cooking_fuel_k5,map@data$c1_cooking_fuel_k5,map@data$c2_cooking_fuel_k5),
                        switch(level_selection,  map@data$c0_cooking_fuel_k6,map@data$c1_cooking_fuel_k6,map@data$c2_cooking_fuel_k6),
                        switch(level_selection,  map@data$c0_cooking_fuel_k7,map@data$c1_cooking_fuel_k7,map@data$c2_cooking_fuel_k7),
                        switch(level_selection,  map@data$c0_cooking_fuel_k8,map@data$c1_cooking_fuel_k8,map@data$c2_cooking_fuel_k8),
                        switch(level_selection,  map@data$c0_cooking_fuel_k9,map@data$c1_cooking_fuel_k9,map@data$c2_cooking_fuel_k9)
)

c_water = switch(k_threshold,
                 switch(level_selection,  map@data$c0_water_k1,map@data$c1_water_k1,map@data$c2_water_k1),
                 switch(level_selection,  map@data$c0_water_k2,map@data$c1_water_k2,map@data$c2_water_k2),
                 switch(level_selection,  map@data$c0_water_k3,map@data$c1_water_k3,map@data$c2_water_k3),
                 switch(level_selection,  map@data$c0_water_k4,map@data$c1_water_k4,map@data$c2_water_k4),
                 switch(level_selection,  map@data$c0_water_k5,map@data$c1_water_k5,map@data$c2_water_k5),
                 switch(level_selection,  map@data$c0_water_k6,map@data$c1_water_k6,map@data$c2_water_k6),
                 switch(level_selection,  map@data$c0_water_k7,map@data$c1_water_k7,map@data$c2_water_k7),
                 switch(level_selection,  map@data$c0_water_k8,map@data$c1_water_k8,map@data$c2_water_k8),
                 switch(level_selection,  map@data$c0_water_k9,map@data$c1_water_k9,map@data$c2_water_k9)
)

c_toilet = switch(k_threshold,
                  switch(level_selection,  map@data$c0_toilet_k1,map@data$c1_toilet_k1,map@data$c2_toilet_k1),
                  switch(level_selection,  map@data$c0_toilet_k2,map@data$c1_toilet_k2,map@data$c2_toilet_k2),
                  switch(level_selection,  map@data$c0_toilet_k3,map@data$c1_toilet_k3,map@data$c2_toilet_k3),
                  switch(level_selection,  map@data$c0_toilet_k4,map@data$c1_toilet_k4,map@data$c2_toilet_k4),
                  switch(level_selection,  map@data$c0_toilet_k5,map@data$c1_toilet_k5,map@data$c2_toilet_k5),
                  switch(level_selection,  map@data$c0_toilet_k6,map@data$c1_toilet_k6,map@data$c2_toilet_k6),
                  switch(level_selection,  map@data$c0_toilet_k7,map@data$c1_toilet_k7,map@data$c2_toilet_k7),
                  switch(level_selection,  map@data$c0_toilet_k8,map@data$c1_toilet_k8,map@data$c2_toilet_k8),
                  switch(level_selection,  map@data$c0_toilet_k9,map@data$c1_toilet_k9,map@data$c2_toilet_k9)
)

c_land = switch(k_threshold,
                switch(level_selection,  map@data$c0_land_k1,map@data$c1_land_k1,map@data$c2_land_k1),
                switch(level_selection,  map@data$c0_land_k2,map@data$c1_land_k2,map@data$c2_land_k2),
                switch(level_selection,  map@data$c0_land_k3,map@data$c1_land_k3,map@data$c2_land_k3),
                switch(level_selection,  map@data$c0_land_k4,map@data$c1_land_k4,map@data$c2_land_k4),
                switch(level_selection,  map@data$c0_land_k5,map@data$c1_land_k5,map@data$c2_land_k5),
                switch(level_selection,  map@data$c0_land_k6,map@data$c1_land_k6,map@data$c2_land_k6),
                switch(level_selection,  map@data$c0_land_k7,map@data$c1_land_k7,map@data$c2_land_k7),
                switch(level_selection,  map@data$c0_land_k8,map@data$c1_land_k8,map@data$c2_land_k8),
                switch(level_selection,  map@data$c0_land_k9,map@data$c1_land_k9,map@data$c2_land_k9)
)

c_livestock = switch(k_threshold,
                     switch(level_selection,  map@data$c0_livestock_k1,map@data$c1_livestock_k1,map@data$c2_livestock_k1),
                     switch(level_selection,  map@data$c0_livestock_k2,map@data$c1_livestock_k2,map@data$c2_livestock_k2),
                     switch(level_selection,  map@data$c0_livestock_k3,map@data$c1_livestock_k3,map@data$c2_livestock_k3),
                     switch(level_selection,  map@data$c0_livestock_k4,map@data$c1_livestock_k4,map@data$c2_livestock_k4),
                     switch(level_selection,  map@data$c0_livestock_k5,map@data$c1_livestock_k5,map@data$c2_livestock_k5),
                     switch(level_selection,  map@data$c0_livestock_k6,map@data$c1_livestock_k6,map@data$c2_livestock_k6),
                     switch(level_selection,  map@data$c0_livestock_k7,map@data$c1_livestock_k7,map@data$c2_livestock_k7),
                     switch(level_selection,  map@data$c0_livestock_k8,map@data$c1_livestock_k8,map@data$c2_livestock_k8),
                     switch(level_selection,  map@data$c0_livestock_k9,map@data$c1_livestock_k9,map@data$c2_livestock_k9)
)

c_rural_equip = switch(k_threshold,
                       switch(level_selection,  map@data$c0_rural_equip_k1,map@data$c1_rural_equip_k1,map@data$c2_rural_equip_k1),
                       switch(level_selection,  map@data$c0_rural_equip_k2,map@data$c1_rural_equip_k2,map@data$c2_rural_equip_k2),
                       switch(level_selection,  map@data$c0_rural_equip_k3,map@data$c1_rural_equip_k3,map@data$c2_rural_equip_k3),
                       switch(level_selection,  map@data$c0_rural_equip_k4,map@data$c1_rural_equip_k4,map@data$c2_rural_equip_k4),
                       switch(level_selection,  map@data$c0_rural_equip_k5,map@data$c1_rural_equip_k5,map@data$c2_rural_equip_k5),
                       switch(level_selection,  map@data$c0_rural_equip_k6,map@data$c1_rural_equip_k6,map@data$c2_rural_equip_k6),
                       switch(level_selection,  map@data$c0_rural_equip_k7,map@data$c1_rural_equip_k7,map@data$c2_rural_equip_k7),
                       switch(level_selection,  map@data$c0_rural_equip_k8,map@data$c1_rural_equip_k8,map@data$c2_rural_equip_k8),
                       switch(level_selection,  map@data$c0_rural_equip_k9,map@data$c1_rural_equip_k9,map@data$c2_rural_equip_k9)
)



n_c_edu_max = switch(k_threshold,
                     switch(level_selection,  nat_data$c0_edu_max_k1,nat_data$c1_edu_max_k1,nat_data$c2_edu_max_k1),
                     switch(level_selection,  nat_data$c0_edu_max_k2,nat_data$c1_edu_max_k2,nat_data$c2_edu_max_k2),
                     switch(level_selection,  nat_data$c0_edu_max_k3,nat_data$c1_edu_max_k3,nat_data$c2_edu_max_k3),
                     switch(level_selection,  nat_data$c0_edu_max_k4,nat_data$c1_edu_max_k4,nat_data$c2_edu_max_k4),
                     switch(level_selection,  nat_data$c0_edu_max_k5,nat_data$c1_edu_max_k5,nat_data$c2_edu_max_k5),
                     switch(level_selection,  nat_data$c0_edu_max_k6,nat_data$c1_edu_max_k6,nat_data$c2_edu_max_k6),
                     switch(level_selection,  nat_data$c0_edu_max_k7,nat_data$c1_edu_max_k7,nat_data$c2_edu_max_k7),
                     switch(level_selection,  nat_data$c0_edu_max_k8,nat_data$c1_edu_max_k8,nat_data$c2_edu_max_k8),
                     switch(level_selection,  nat_data$c0_edu_max_k9,nat_data$c1_edu_max_k9,nat_data$c2_edu_max_k9)
)
n_c_edu_dropout = switch(k_threshold,
                         switch(level_selection,  nat_data$c0_edu_dropout_k1,nat_data$c1_edu_dropout_k1,nat_data$c2_edu_dropout_k1),
                         switch(level_selection,  nat_data$c0_edu_dropout_k2,nat_data$c1_edu_dropout_k2,nat_data$c2_edu_dropout_k2),
                         switch(level_selection,  nat_data$c0_edu_dropout_k3,nat_data$c1_edu_dropout_k3,nat_data$c2_edu_dropout_k3),
                         switch(level_selection,  nat_data$c0_edu_dropout_k4,nat_data$c1_edu_dropout_k4,nat_data$c2_edu_dropout_k4),
                         switch(level_selection,  nat_data$c0_edu_dropout_k5,nat_data$c1_edu_dropout_k5,nat_data$c2_edu_dropout_k5),
                         switch(level_selection,  nat_data$c0_edu_dropout_k6,nat_data$c1_edu_dropout_k6,nat_data$c2_edu_dropout_k6),
                         switch(level_selection,  nat_data$c0_edu_dropout_k7,nat_data$c1_edu_dropout_k7,nat_data$c2_edu_dropout_k7),
                         switch(level_selection,  nat_data$c0_edu_dropout_k8,nat_data$c1_edu_dropout_k8,nat_data$c2_edu_dropout_k8),
                         switch(level_selection,  nat_data$c0_edu_dropout_k9,nat_data$c1_edu_dropout_k9,nat_data$c2_edu_dropout_k9)
)

n_c_hea_chronic = switch(k_threshold,
                         switch(level_selection,  nat_data$c0_hea_chronic_k1,nat_data$c1_hea_chronic_k1,nat_data$c2_hea_chronic_k1),
                         switch(level_selection,  nat_data$c0_hea_chronic_k2,nat_data$c1_hea_chronic_k2,nat_data$c2_hea_chronic_k2),
                         switch(level_selection,  nat_data$c0_hea_chronic_k3,nat_data$c1_hea_chronic_k3,nat_data$c2_hea_chronic_k3),
                         switch(level_selection,  nat_data$c0_hea_chronic_k4,nat_data$c1_hea_chronic_k4,nat_data$c2_hea_chronic_k4),
                         switch(level_selection,  nat_data$c0_hea_chronic_k5,nat_data$c1_hea_chronic_k5,nat_data$c2_hea_chronic_k5),
                         switch(level_selection,  nat_data$c0_hea_chronic_k6,nat_data$c1_hea_chronic_k6,nat_data$c2_hea_chronic_k6),
                         switch(level_selection,  nat_data$c0_hea_chronic_k7,nat_data$c1_hea_chronic_k7,nat_data$c2_hea_chronic_k7),
                         switch(level_selection,  nat_data$c0_hea_chronic_k8,nat_data$c1_hea_chronic_k8,nat_data$c2_hea_chronic_k8),
                         switch(level_selection,  nat_data$c0_hea_chronic_k9,nat_data$c1_hea_chronic_k9,nat_data$c2_hea_chronic_k9)
)

n_c_hea_visit = switch(k_threshold,
                       switch(level_selection,  nat_data$c0_hea_visit_k1,nat_data$c1_hea_visit_k1,nat_data$c2_hea_visit_k1),
                       switch(level_selection,  nat_data$c0_hea_visit_k2,nat_data$c1_hea_visit_k2,nat_data$c2_hea_visit_k2),
                       switch(level_selection,  nat_data$c0_hea_visit_k3,nat_data$c1_hea_visit_k3,nat_data$c2_hea_visit_k3),
                       switch(level_selection,  nat_data$c0_hea_visit_k4,nat_data$c1_hea_visit_k4,nat_data$c2_hea_visit_k4),
                       switch(level_selection,  nat_data$c0_hea_visit_k5,nat_data$c1_hea_visit_k5,nat_data$c2_hea_visit_k5),
                       switch(level_selection,  nat_data$c0_hea_visit_k6,nat_data$c1_hea_visit_k6,nat_data$c2_hea_visit_k6),
                       switch(level_selection,  nat_data$c0_hea_visit_k7,nat_data$c1_hea_visit_k7,nat_data$c2_hea_visit_k7),
                       switch(level_selection,  nat_data$c0_hea_visit_k8,nat_data$c1_hea_visit_k8,nat_data$c2_hea_visit_k8),
                       switch(level_selection,  nat_data$c0_hea_visit_k9,nat_data$c1_hea_visit_k9,nat_data$c2_hea_visit_k9)
)

n_c_employment = switch(k_threshold,
                        switch(level_selection,  nat_data$c0_employment_k1,nat_data$c1_employment_k1,nat_data$c2_employment_k1),
                        switch(level_selection,  nat_data$c0_employment_k2,nat_data$c1_employment_k2,nat_data$c2_employment_k2),
                        switch(level_selection,  nat_data$c0_employment_k3,nat_data$c1_employment_k3,nat_data$c2_employment_k3),
                        switch(level_selection,  nat_data$c0_employment_k4,nat_data$c1_employment_k4,nat_data$c2_employment_k4),
                        switch(level_selection,  nat_data$c0_employment_k5,nat_data$c1_employment_k5,nat_data$c2_employment_k5),
                        switch(level_selection,  nat_data$c0_employment_k6,nat_data$c1_employment_k6,nat_data$c2_employment_k6),
                        switch(level_selection,  nat_data$c0_employment_k7,nat_data$c1_employment_k7,nat_data$c2_employment_k7),
                        switch(level_selection,  nat_data$c0_employment_k8,nat_data$c1_employment_k8,nat_data$c2_employment_k8),
                        switch(level_selection,  nat_data$c0_employment_k9,nat_data$c1_employment_k9,nat_data$c2_employment_k9)
)

n_c_assets = switch(k_threshold,
                    switch(level_selection,  nat_data$c0_assets_k1,nat_data$c1_assets_k1,nat_data$c2_assets_k1),
                    switch(level_selection,  nat_data$c0_assets_k2,nat_data$c1_assets_k2,nat_data$c2_assets_k2),
                    switch(level_selection,  nat_data$c0_assets_k3,nat_data$c1_assets_k3,nat_data$c2_assets_k3),
                    switch(level_selection,  nat_data$c0_assets_k4,nat_data$c1_assets_k4,nat_data$c2_assets_k4),
                    switch(level_selection,  nat_data$c0_assets_k5,nat_data$c1_assets_k5,nat_data$c2_assets_k5),
                    switch(level_selection,  nat_data$c0_assets_k6,nat_data$c1_assets_k6,nat_data$c2_assets_k6),
                    switch(level_selection,  nat_data$c0_assets_k7,nat_data$c1_assets_k7,nat_data$c2_assets_k7),
                    switch(level_selection,  nat_data$c0_assets_k8,nat_data$c1_assets_k8,nat_data$c2_assets_k8),
                    switch(level_selection,  nat_data$c0_assets_k9,nat_data$c1_assets_k9,nat_data$c2_assets_k9)
)

n_c_services = switch(k_threshold,
                      switch(level_selection,  nat_data$c0_services_k1,nat_data$c1_services_k1,nat_data$c2_services_k1),
                      switch(level_selection,  nat_data$c0_services_k2,nat_data$c1_services_k2,nat_data$c2_services_k2),
                      switch(level_selection,  nat_data$c0_services_k3,nat_data$c1_services_k3,nat_data$c2_services_k3),
                      switch(level_selection,  nat_data$c0_services_k4,nat_data$c1_services_k4,nat_data$c2_services_k4),
                      switch(level_selection,  nat_data$c0_services_k5,nat_data$c1_services_k5,nat_data$c2_services_k5),
                      switch(level_selection,  nat_data$c0_services_k6,nat_data$c1_services_k6,nat_data$c2_services_k6),
                      switch(level_selection,  nat_data$c0_services_k7,nat_data$c1_services_k7,nat_data$c2_services_k7),
                      switch(level_selection,  nat_data$c0_services_k8,nat_data$c1_services_k8,nat_data$c2_services_k8),
                      switch(level_selection,  nat_data$c0_services_k9,nat_data$c1_services_k9,nat_data$c2_services_k9)
)

n_c_electricity = switch(k_threshold,
                         switch(level_selection,  nat_data$c0_electricity_k1,nat_data$c1_electricity_k1,nat_data$c2_electricity_k1),
                         switch(level_selection,  nat_data$c0_electricity_k2,nat_data$c1_electricity_k2,nat_data$c2_electricity_k2),
                         switch(level_selection,  nat_data$c0_electricity_k3,nat_data$c1_electricity_k3,nat_data$c2_electricity_k3),
                         switch(level_selection,  nat_data$c0_electricity_k4,nat_data$c1_electricity_k4,nat_data$c2_electricity_k4),
                         switch(level_selection,  nat_data$c0_electricity_k5,nat_data$c1_electricity_k5,nat_data$c2_electricity_k5),
                         switch(level_selection,  nat_data$c0_electricity_k6,nat_data$c1_electricity_k6,nat_data$c2_electricity_k6),
                         switch(level_selection,  nat_data$c0_electricity_k7,nat_data$c1_electricity_k7,nat_data$c2_electricity_k7),
                         switch(level_selection,  nat_data$c0_electricity_k8,nat_data$c1_electricity_k8,nat_data$c2_electricity_k8),
                         switch(level_selection,  nat_data$c0_electricity_k9,nat_data$c1_electricity_k9,nat_data$c2_electricity_k9)
)

n_c_cooking_fuel = switch(k_threshold,
                          switch(level_selection,  nat_data$c0_cooking_fuel_k1,nat_data$c1_cooking_fuel_k1,nat_data$c2_cooking_fuel_k1),
                          switch(level_selection,  nat_data$c0_cooking_fuel_k2,nat_data$c1_cooking_fuel_k2,nat_data$c2_cooking_fuel_k2),
                          switch(level_selection,  nat_data$c0_cooking_fuel_k3,nat_data$c1_cooking_fuel_k3,nat_data$c2_cooking_fuel_k3),
                          switch(level_selection,  nat_data$c0_cooking_fuel_k4,nat_data$c1_cooking_fuel_k4,nat_data$c2_cooking_fuel_k4),
                          switch(level_selection,  nat_data$c0_cooking_fuel_k5,nat_data$c1_cooking_fuel_k5,nat_data$c2_cooking_fuel_k5),
                          switch(level_selection,  nat_data$c0_cooking_fuel_k6,nat_data$c1_cooking_fuel_k6,nat_data$c2_cooking_fuel_k6),
                          switch(level_selection,  nat_data$c0_cooking_fuel_k7,nat_data$c1_cooking_fuel_k7,nat_data$c2_cooking_fuel_k7),
                          switch(level_selection,  nat_data$c0_cooking_fuel_k8,nat_data$c1_cooking_fuel_k8,nat_data$c2_cooking_fuel_k8),
                          switch(level_selection,  nat_data$c0_cooking_fuel_k9,nat_data$c1_cooking_fuel_k9,nat_data$c2_cooking_fuel_k9)
)

n_c_water = switch(k_threshold,
                   switch(level_selection,  nat_data$c0_water_k1,nat_data$c1_water_k1,nat_data$c2_water_k1),
                   switch(level_selection,  nat_data$c0_water_k2,nat_data$c1_water_k2,nat_data$c2_water_k2),
                   switch(level_selection,  nat_data$c0_water_k3,nat_data$c1_water_k3,nat_data$c2_water_k3),
                   switch(level_selection,  nat_data$c0_water_k4,nat_data$c1_water_k4,nat_data$c2_water_k4),
                   switch(level_selection,  nat_data$c0_water_k5,nat_data$c1_water_k5,nat_data$c2_water_k5),
                   switch(level_selection,  nat_data$c0_water_k6,nat_data$c1_water_k6,nat_data$c2_water_k6),
                   switch(level_selection,  nat_data$c0_water_k7,nat_data$c1_water_k7,nat_data$c2_water_k7),
                   switch(level_selection,  nat_data$c0_water_k8,nat_data$c1_water_k8,nat_data$c2_water_k8),
                   switch(level_selection,  nat_data$c0_water_k9,nat_data$c1_water_k9,nat_data$c2_water_k9)
)

n_c_toilet = switch(k_threshold,
                    switch(level_selection,  nat_data$c0_toilet_k1,nat_data$c1_toilet_k1,nat_data$c2_toilet_k1),
                    switch(level_selection,  nat_data$c0_toilet_k2,nat_data$c1_toilet_k2,nat_data$c2_toilet_k2),
                    switch(level_selection,  nat_data$c0_toilet_k3,nat_data$c1_toilet_k3,nat_data$c2_toilet_k3),
                    switch(level_selection,  nat_data$c0_toilet_k4,nat_data$c1_toilet_k4,nat_data$c2_toilet_k4),
                    switch(level_selection,  nat_data$c0_toilet_k5,nat_data$c1_toilet_k5,nat_data$c2_toilet_k5),
                    switch(level_selection,  nat_data$c0_toilet_k6,nat_data$c1_toilet_k6,nat_data$c2_toilet_k6),
                    switch(level_selection,  nat_data$c0_toilet_k7,nat_data$c1_toilet_k7,nat_data$c2_toilet_k7),
                    switch(level_selection,  nat_data$c0_toilet_k8,nat_data$c1_toilet_k8,nat_data$c2_toilet_k8),
                    switch(level_selection,  nat_data$c0_toilet_k9,nat_data$c1_toilet_k9,nat_data$c2_toilet_k9)
)

n_c_land = switch(k_threshold,
                  switch(level_selection,  nat_data$c0_land_k1,nat_data$c1_land_k1,nat_data$c2_land_k1),
                  switch(level_selection,  nat_data$c0_land_k2,nat_data$c1_land_k2,nat_data$c2_land_k2),
                  switch(level_selection,  nat_data$c0_land_k3,nat_data$c1_land_k3,nat_data$c2_land_k3),
                  switch(level_selection,  nat_data$c0_land_k4,nat_data$c1_land_k4,nat_data$c2_land_k4),
                  switch(level_selection,  nat_data$c0_land_k5,nat_data$c1_land_k5,nat_data$c2_land_k5),
                  switch(level_selection,  nat_data$c0_land_k6,nat_data$c1_land_k6,nat_data$c2_land_k6),
                  switch(level_selection,  nat_data$c0_land_k7,nat_data$c1_land_k7,nat_data$c2_land_k7),
                  switch(level_selection,  nat_data$c0_land_k8,nat_data$c1_land_k8,nat_data$c2_land_k8),
                  switch(level_selection,  nat_data$c0_land_k9,nat_data$c1_land_k9,nat_data$c2_land_k9)
)

n_c_livestock = switch(k_threshold,
                       switch(level_selection,  nat_data$c0_livestock_k1,nat_data$c1_livestock_k1,nat_data$c2_livestock_k1),
                       switch(level_selection,  nat_data$c0_livestock_k2,nat_data$c1_livestock_k2,nat_data$c2_livestock_k2),
                       switch(level_selection,  nat_data$c0_livestock_k3,nat_data$c1_livestock_k3,nat_data$c2_livestock_k3),
                       switch(level_selection,  nat_data$c0_livestock_k4,nat_data$c1_livestock_k4,nat_data$c2_livestock_k4),
                       switch(level_selection,  nat_data$c0_livestock_k5,nat_data$c1_livestock_k5,nat_data$c2_livestock_k5),
                       switch(level_selection,  nat_data$c0_livestock_k6,nat_data$c1_livestock_k6,nat_data$c2_livestock_k6),
                       switch(level_selection,  nat_data$c0_livestock_k7,nat_data$c1_livestock_k7,nat_data$c2_livestock_k7),
                       switch(level_selection,  nat_data$c0_livestock_k8,nat_data$c1_livestock_k8,nat_data$c2_livestock_k8),
                       switch(level_selection,  nat_data$c0_livestock_k9,nat_data$c1_livestock_k9,nat_data$c2_livestock_k9)
)

n_c_rural_equip = switch(k_threshold,
                         switch(level_selection,  nat_data$c0_rural_equip_k1,nat_data$c1_rural_equip_k1,nat_data$c2_rural_equip_k1),
                         switch(level_selection,  nat_data$c0_rural_equip_k2,nat_data$c1_rural_equip_k2,nat_data$c2_rural_equip_k2),
                         switch(level_selection,  nat_data$c0_rural_equip_k3,nat_data$c1_rural_equip_k3,nat_data$c2_rural_equip_k3),
                         switch(level_selection,  nat_data$c0_rural_equip_k4,nat_data$c1_rural_equip_k4,nat_data$c2_rural_equip_k4),
                         switch(level_selection,  nat_data$c0_rural_equip_k5,nat_data$c1_rural_equip_k5,nat_data$c2_rural_equip_k5),
                         switch(level_selection,  nat_data$c0_rural_equip_k6,nat_data$c1_rural_equip_k6,nat_data$c2_rural_equip_k6),
                         switch(level_selection,  nat_data$c0_rural_equip_k7,nat_data$c1_rural_equip_k7,nat_data$c2_rural_equip_k7),
                         switch(level_selection,  nat_data$c0_rural_equip_k8,nat_data$c1_rural_equip_k8,nat_data$c2_rural_equip_k8),
                         switch(level_selection,  nat_data$c0_rural_equip_k9,nat_data$c1_rural_equip_k9,nat_data$c2_rural_equip_k9)
)

n_g_edu_max = switch(k_threshold,
                     switch(level_selection,  nat_data$g0_edu_max_k1,nat_data$g1_edu_max_k1,nat_data$g2_edu_max_k1),
                     switch(level_selection,  nat_data$g0_edu_max_k2,nat_data$g1_edu_max_k2,nat_data$g2_edu_max_k2),
                     switch(level_selection,  nat_data$g0_edu_max_k3,nat_data$g1_edu_max_k3,nat_data$g2_edu_max_k3),
                     switch(level_selection,  nat_data$g0_edu_max_k4,nat_data$g1_edu_max_k4,nat_data$g2_edu_max_k4),
                     switch(level_selection,  nat_data$g0_edu_max_k5,nat_data$g1_edu_max_k5,nat_data$g2_edu_max_k5),
                     switch(level_selection,  nat_data$g0_edu_max_k6,nat_data$g1_edu_max_k6,nat_data$g2_edu_max_k6),
                     switch(level_selection,  nat_data$g0_edu_max_k7,nat_data$g1_edu_max_k7,nat_data$g2_edu_max_k7),
                     switch(level_selection,  nat_data$g0_edu_max_k8,nat_data$g1_edu_max_k8,nat_data$g2_edu_max_k8),
                     switch(level_selection,  nat_data$g0_edu_max_k9,nat_data$g1_edu_max_k9,nat_data$g2_edu_max_k9)
)
n_g_edu_dropout = switch(k_threshold,
                         switch(level_selection,  nat_data$g0_edu_dropout_k1,nat_data$g1_edu_dropout_k1,nat_data$g2_edu_dropout_k1),
                         switch(level_selection,  nat_data$g0_edu_dropout_k2,nat_data$g1_edu_dropout_k2,nat_data$g2_edu_dropout_k2),
                         switch(level_selection,  nat_data$g0_edu_dropout_k3,nat_data$g1_edu_dropout_k3,nat_data$g2_edu_dropout_k3),
                         switch(level_selection,  nat_data$g0_edu_dropout_k4,nat_data$g1_edu_dropout_k4,nat_data$g2_edu_dropout_k4),
                         switch(level_selection,  nat_data$g0_edu_dropout_k5,nat_data$g1_edu_dropout_k5,nat_data$g2_edu_dropout_k5),
                         switch(level_selection,  nat_data$g0_edu_dropout_k6,nat_data$g1_edu_dropout_k6,nat_data$g2_edu_dropout_k6),
                         switch(level_selection,  nat_data$g0_edu_dropout_k7,nat_data$g1_edu_dropout_k7,nat_data$g2_edu_dropout_k7),
                         switch(level_selection,  nat_data$g0_edu_dropout_k8,nat_data$g1_edu_dropout_k8,nat_data$g2_edu_dropout_k8),
                         switch(level_selection,  nat_data$g0_edu_dropout_k9,nat_data$g1_edu_dropout_k9,nat_data$g2_edu_dropout_k9)
)

n_g_hea_chronic = switch(k_threshold,
                         switch(level_selection,  nat_data$g0_hea_chronic_k1,nat_data$g1_hea_chronic_k1,nat_data$g2_hea_chronic_k1),
                         switch(level_selection,  nat_data$g0_hea_chronic_k2,nat_data$g1_hea_chronic_k2,nat_data$g2_hea_chronic_k2),
                         switch(level_selection,  nat_data$g0_hea_chronic_k3,nat_data$g1_hea_chronic_k3,nat_data$g2_hea_chronic_k3),
                         switch(level_selection,  nat_data$g0_hea_chronic_k4,nat_data$g1_hea_chronic_k4,nat_data$g2_hea_chronic_k4),
                         switch(level_selection,  nat_data$g0_hea_chronic_k5,nat_data$g1_hea_chronic_k5,nat_data$g2_hea_chronic_k5),
                         switch(level_selection,  nat_data$g0_hea_chronic_k6,nat_data$g1_hea_chronic_k6,nat_data$g2_hea_chronic_k6),
                         switch(level_selection,  nat_data$g0_hea_chronic_k7,nat_data$g1_hea_chronic_k7,nat_data$g2_hea_chronic_k7),
                         switch(level_selection,  nat_data$g0_hea_chronic_k8,nat_data$g1_hea_chronic_k8,nat_data$g2_hea_chronic_k8),
                         switch(level_selection,  nat_data$g0_hea_chronic_k9,nat_data$g1_hea_chronic_k9,nat_data$g2_hea_chronic_k9)
)

n_g_hea_visit = switch(k_threshold,
                       switch(level_selection,  nat_data$g0_hea_visit_k1,nat_data$g1_hea_visit_k1,nat_data$g2_hea_visit_k1),
                       switch(level_selection,  nat_data$g0_hea_visit_k2,nat_data$g1_hea_visit_k2,nat_data$g2_hea_visit_k2),
                       switch(level_selection,  nat_data$g0_hea_visit_k3,nat_data$g1_hea_visit_k3,nat_data$g2_hea_visit_k3),
                       switch(level_selection,  nat_data$g0_hea_visit_k4,nat_data$g1_hea_visit_k4,nat_data$g2_hea_visit_k4),
                       switch(level_selection,  nat_data$g0_hea_visit_k5,nat_data$g1_hea_visit_k5,nat_data$g2_hea_visit_k5),
                       switch(level_selection,  nat_data$g0_hea_visit_k6,nat_data$g1_hea_visit_k6,nat_data$g2_hea_visit_k6),
                       switch(level_selection,  nat_data$g0_hea_visit_k7,nat_data$g1_hea_visit_k7,nat_data$g2_hea_visit_k7),
                       switch(level_selection,  nat_data$g0_hea_visit_k8,nat_data$g1_hea_visit_k8,nat_data$g2_hea_visit_k8),
                       switch(level_selection,  nat_data$g0_hea_visit_k9,nat_data$g1_hea_visit_k9,nat_data$g2_hea_visit_k9)
)

n_g_employment = switch(k_threshold,
                        switch(level_selection,  nat_data$g0_employment_k1,nat_data$g1_employment_k1,nat_data$g2_employment_k1),
                        switch(level_selection,  nat_data$g0_employment_k2,nat_data$g1_employment_k2,nat_data$g2_employment_k2),
                        switch(level_selection,  nat_data$g0_employment_k3,nat_data$g1_employment_k3,nat_data$g2_employment_k3),
                        switch(level_selection,  nat_data$g0_employment_k4,nat_data$g1_employment_k4,nat_data$g2_employment_k4),
                        switch(level_selection,  nat_data$g0_employment_k5,nat_data$g1_employment_k5,nat_data$g2_employment_k5),
                        switch(level_selection,  nat_data$g0_employment_k6,nat_data$g1_employment_k6,nat_data$g2_employment_k6),
                        switch(level_selection,  nat_data$g0_employment_k7,nat_data$g1_employment_k7,nat_data$g2_employment_k7),
                        switch(level_selection,  nat_data$g0_employment_k8,nat_data$g1_employment_k8,nat_data$g2_employment_k8),
                        switch(level_selection,  nat_data$g0_employment_k9,nat_data$g1_employment_k9,nat_data$g2_employment_k9)
)

n_g_assets = switch(k_threshold,
                    switch(level_selection,  nat_data$g0_assets_k1,nat_data$g1_assets_k1,nat_data$g2_assets_k1),
                    switch(level_selection,  nat_data$g0_assets_k2,nat_data$g1_assets_k2,nat_data$g2_assets_k2),
                    switch(level_selection,  nat_data$g0_assets_k3,nat_data$g1_assets_k3,nat_data$g2_assets_k3),
                    switch(level_selection,  nat_data$g0_assets_k4,nat_data$g1_assets_k4,nat_data$g2_assets_k4),
                    switch(level_selection,  nat_data$g0_assets_k5,nat_data$g1_assets_k5,nat_data$g2_assets_k5),
                    switch(level_selection,  nat_data$g0_assets_k6,nat_data$g1_assets_k6,nat_data$g2_assets_k6),
                    switch(level_selection,  nat_data$g0_assets_k7,nat_data$g1_assets_k7,nat_data$g2_assets_k7),
                    switch(level_selection,  nat_data$g0_assets_k8,nat_data$g1_assets_k8,nat_data$g2_assets_k8),
                    switch(level_selection,  nat_data$g0_assets_k9,nat_data$g1_assets_k9,nat_data$g2_assets_k9)
)

n_g_services = switch(k_threshold,
                      switch(level_selection,  nat_data$g0_services_k1,nat_data$g1_services_k1,nat_data$g2_services_k1),
                      switch(level_selection,  nat_data$g0_services_k2,nat_data$g1_services_k2,nat_data$g2_services_k2),
                      switch(level_selection,  nat_data$g0_services_k3,nat_data$g1_services_k3,nat_data$g2_services_k3),
                      switch(level_selection,  nat_data$g0_services_k4,nat_data$g1_services_k4,nat_data$g2_services_k4),
                      switch(level_selection,  nat_data$g0_services_k5,nat_data$g1_services_k5,nat_data$g2_services_k5),
                      switch(level_selection,  nat_data$g0_services_k6,nat_data$g1_services_k6,nat_data$g2_services_k6),
                      switch(level_selection,  nat_data$g0_services_k7,nat_data$g1_services_k7,nat_data$g2_services_k7),
                      switch(level_selection,  nat_data$g0_services_k8,nat_data$g1_services_k8,nat_data$g2_services_k8),
                      switch(level_selection,  nat_data$g0_services_k9,nat_data$g1_services_k9,nat_data$g2_services_k9)
)

n_g_electricity = switch(k_threshold,
                         switch(level_selection,  nat_data$g0_electricity_k1,nat_data$g1_electricity_k1,nat_data$g2_electricity_k1),
                         switch(level_selection,  nat_data$g0_electricity_k2,nat_data$g1_electricity_k2,nat_data$g2_electricity_k2),
                         switch(level_selection,  nat_data$g0_electricity_k3,nat_data$g1_electricity_k3,nat_data$g2_electricity_k3),
                         switch(level_selection,  nat_data$g0_electricity_k4,nat_data$g1_electricity_k4,nat_data$g2_electricity_k4),
                         switch(level_selection,  nat_data$g0_electricity_k5,nat_data$g1_electricity_k5,nat_data$g2_electricity_k5),
                         switch(level_selection,  nat_data$g0_electricity_k6,nat_data$g1_electricity_k6,nat_data$g2_electricity_k6),
                         switch(level_selection,  nat_data$g0_electricity_k7,nat_data$g1_electricity_k7,nat_data$g2_electricity_k7),
                         switch(level_selection,  nat_data$g0_electricity_k8,nat_data$g1_electricity_k8,nat_data$g2_electricity_k8),
                         switch(level_selection,  nat_data$g0_electricity_k9,nat_data$g1_electricity_k9,nat_data$g2_electricity_k9)
)

n_g_cooking_fuel = switch(k_threshold,
                          switch(level_selection,  nat_data$g0_cooking_fuel_k1,nat_data$g1_cooking_fuel_k1,nat_data$g2_cooking_fuel_k1),
                          switch(level_selection,  nat_data$g0_cooking_fuel_k2,nat_data$g1_cooking_fuel_k2,nat_data$g2_cooking_fuel_k2),
                          switch(level_selection,  nat_data$g0_cooking_fuel_k3,nat_data$g1_cooking_fuel_k3,nat_data$g2_cooking_fuel_k3),
                          switch(level_selection,  nat_data$g0_cooking_fuel_k4,nat_data$g1_cooking_fuel_k4,nat_data$g2_cooking_fuel_k4),
                          switch(level_selection,  nat_data$g0_cooking_fuel_k5,nat_data$g1_cooking_fuel_k5,nat_data$g2_cooking_fuel_k5),
                          switch(level_selection,  nat_data$g0_cooking_fuel_k6,nat_data$g1_cooking_fuel_k6,nat_data$g2_cooking_fuel_k6),
                          switch(level_selection,  nat_data$g0_cooking_fuel_k7,nat_data$g1_cooking_fuel_k7,nat_data$g2_cooking_fuel_k7),
                          switch(level_selection,  nat_data$g0_cooking_fuel_k8,nat_data$g1_cooking_fuel_k8,nat_data$g2_cooking_fuel_k8),
                          switch(level_selection,  nat_data$g0_cooking_fuel_k9,nat_data$g1_cooking_fuel_k9,nat_data$g2_cooking_fuel_k9)
)

n_g_water = switch(k_threshold,
                   switch(level_selection,  nat_data$g0_water_k1,nat_data$g1_water_k1,nat_data$g2_water_k1),
                   switch(level_selection,  nat_data$g0_water_k2,nat_data$g1_water_k2,nat_data$g2_water_k2),
                   switch(level_selection,  nat_data$g0_water_k3,nat_data$g1_water_k3,nat_data$g2_water_k3),
                   switch(level_selection,  nat_data$g0_water_k4,nat_data$g1_water_k4,nat_data$g2_water_k4),
                   switch(level_selection,  nat_data$g0_water_k5,nat_data$g1_water_k5,nat_data$g2_water_k5),
                   switch(level_selection,  nat_data$g0_water_k6,nat_data$g1_water_k6,nat_data$g2_water_k6),
                   switch(level_selection,  nat_data$g0_water_k7,nat_data$g1_water_k7,nat_data$g2_water_k7),
                   switch(level_selection,  nat_data$g0_water_k8,nat_data$g1_water_k8,nat_data$g2_water_k8),
                   switch(level_selection,  nat_data$g0_water_k9,nat_data$g1_water_k9,nat_data$g2_water_k9)
)

n_g_toilet = switch(k_threshold,
                    switch(level_selection,  nat_data$g0_toilet_k1,nat_data$g1_toilet_k1,nat_data$g2_toilet_k1),
                    switch(level_selection,  nat_data$g0_toilet_k2,nat_data$g1_toilet_k2,nat_data$g2_toilet_k2),
                    switch(level_selection,  nat_data$g0_toilet_k3,nat_data$g1_toilet_k3,nat_data$g2_toilet_k3),
                    switch(level_selection,  nat_data$g0_toilet_k4,nat_data$g1_toilet_k4,nat_data$g2_toilet_k4),
                    switch(level_selection,  nat_data$g0_toilet_k5,nat_data$g1_toilet_k5,nat_data$g2_toilet_k5),
                    switch(level_selection,  nat_data$g0_toilet_k6,nat_data$g1_toilet_k6,nat_data$g2_toilet_k6),
                    switch(level_selection,  nat_data$g0_toilet_k7,nat_data$g1_toilet_k7,nat_data$g2_toilet_k7),
                    switch(level_selection,  nat_data$g0_toilet_k8,nat_data$g1_toilet_k8,nat_data$g2_toilet_k8),
                    switch(level_selection,  nat_data$g0_toilet_k9,nat_data$g1_toilet_k9,nat_data$g2_toilet_k9)
)

n_g_land = switch(k_threshold,
                  switch(level_selection,  nat_data$g0_land_k1,nat_data$g1_land_k1,nat_data$g2_land_k1),
                  switch(level_selection,  nat_data$g0_land_k2,nat_data$g1_land_k2,nat_data$g2_land_k2),
                  switch(level_selection,  nat_data$g0_land_k3,nat_data$g1_land_k3,nat_data$g2_land_k3),
                  switch(level_selection,  nat_data$g0_land_k4,nat_data$g1_land_k4,nat_data$g2_land_k4),
                  switch(level_selection,  nat_data$g0_land_k5,nat_data$g1_land_k5,nat_data$g2_land_k5),
                  switch(level_selection,  nat_data$g0_land_k6,nat_data$g1_land_k6,nat_data$g2_land_k6),
                  switch(level_selection,  nat_data$g0_land_k7,nat_data$g1_land_k7,nat_data$g2_land_k7),
                  switch(level_selection,  nat_data$g0_land_k8,nat_data$g1_land_k8,nat_data$g2_land_k8),
                  switch(level_selection,  nat_data$g0_land_k9,nat_data$g1_land_k9,nat_data$g2_land_k9)
)

n_g_livestock = switch(k_threshold,
                       switch(level_selection,  nat_data$g0_livestock_k1,nat_data$g1_livestock_k1,nat_data$g2_livestock_k1),
                       switch(level_selection,  nat_data$g0_livestock_k2,nat_data$g1_livestock_k2,nat_data$g2_livestock_k2),
                       switch(level_selection,  nat_data$g0_livestock_k3,nat_data$g1_livestock_k3,nat_data$g2_livestock_k3),
                       switch(level_selection,  nat_data$g0_livestock_k4,nat_data$g1_livestock_k4,nat_data$g2_livestock_k4),
                       switch(level_selection,  nat_data$g0_livestock_k5,nat_data$g1_livestock_k5,nat_data$g2_livestock_k5),
                       switch(level_selection,  nat_data$g0_livestock_k6,nat_data$g1_livestock_k6,nat_data$g2_livestock_k6),
                       switch(level_selection,  nat_data$g0_livestock_k7,nat_data$g1_livestock_k7,nat_data$g2_livestock_k7),
                       switch(level_selection,  nat_data$g0_livestock_k8,nat_data$g1_livestock_k8,nat_data$g2_livestock_k8),
                       switch(level_selection,  nat_data$g0_livestock_k9,nat_data$g1_livestock_k9,nat_data$g2_livestock_k9)
)

n_g_rural_equip = switch(k_threshold,
                         switch(level_selection,  nat_data$g0_rural_equip_k1,nat_data$g1_rural_equip_k1,nat_data$g2_rural_equip_k1),
                         switch(level_selection,  nat_data$g0_rural_equip_k2,nat_data$g1_rural_equip_k2,nat_data$g2_rural_equip_k2),
                         switch(level_selection,  nat_data$g0_rural_equip_k3,nat_data$g1_rural_equip_k3,nat_data$g2_rural_equip_k3),
                         switch(level_selection,  nat_data$g0_rural_equip_k4,nat_data$g1_rural_equip_k4,nat_data$g2_rural_equip_k4),
                         switch(level_selection,  nat_data$g0_rural_equip_k5,nat_data$g1_rural_equip_k5,nat_data$g2_rural_equip_k5),
                         switch(level_selection,  nat_data$g0_rural_equip_k6,nat_data$g1_rural_equip_k6,nat_data$g2_rural_equip_k6),
                         switch(level_selection,  nat_data$g0_rural_equip_k7,nat_data$g1_rural_equip_k7,nat_data$g2_rural_equip_k7),
                         switch(level_selection,  nat_data$g0_rural_equip_k8,nat_data$g1_rural_equip_k8,nat_data$g2_rural_equip_k8),
                         switch(level_selection,  nat_data$g0_rural_equip_k9,nat_data$g1_rural_equip_k9,nat_data$g2_rural_equip_k9)
)

# 1 = c (percent contribution), 2 = g (gap)
c_g_selection = 2

edu_max = switch(c_g_selection, c_edu_max, g_edu_max)
edu_dropout = switch(c_g_selection, c_edu_dropout, g_edu_dropout)
hea_chronic = switch(c_g_selection, c_hea_chronic, g_hea_chronic)
hea_visit = switch(c_g_selection, c_hea_visit, g_hea_visit)
employment = switch(c_g_selection, c_employment, g_employment)
assets = switch(c_g_selection, c_assets, g_assets)
services = switch(c_g_selection, c_services, g_services)
electricity = switch(c_g_selection, c_electricity, g_electricity)
cooking_fuel = switch(c_g_selection, c_cooking_fuel, g_cooking_fuel)
water = switch(c_g_selection, c_water, g_water)
toilet = switch(c_g_selection, c_toilet, g_toilet)
land = switch(c_g_selection, c_land, g_land)
livestock = switch(c_g_selection, c_livestock, g_livestock)
rural_equip = switch(c_g_selection, c_rural_equip, g_rural_equip)

n_edu_max = switch(c_g_selection, n_c_edu_max, n_g_edu_max)
n_edu_dropout = switch(c_g_selection, n_c_edu_dropout, n_g_edu_dropout)
n_hea_chronic = switch(c_g_selection, n_c_hea_chronic, n_g_hea_chronic)
n_hea_visit = switch(c_g_selection, n_c_hea_visit, n_g_hea_visit)
n_employment = switch(c_g_selection, n_c_employment, n_g_employment)
n_assets = switch(c_g_selection, n_c_assets, n_g_assets)
n_services = switch(c_g_selection, n_c_services, n_g_services)
n_electricity = switch(c_g_selection, n_c_electricity, n_g_electricity)
n_cooking_fuel = switch(c_g_selection, n_c_cooking_fuel, n_g_cooking_fuel)
n_water = switch(c_g_selection, n_c_water, n_g_water)
n_toilet = switch(c_g_selection, n_c_toilet, n_g_toilet)
n_land = switch(c_g_selection, n_c_land, n_g_land)
n_livestock = switch(c_g_selection, n_c_livestock, n_g_livestock)
n_rural_equip = switch(c_g_selection, n_c_rural_equip, n_g_rural_equip)

edu_max_labels <- get_label(map@data$NAME_2, "Max. Education", edu_max, n_edu_max)
edu_dropout_labels <- get_label(map@data$NAME_2, "Education Dropout", edu_dropout, n_edu_dropout)
hea_chronic_labels <- get_label(map@data$NAME_2, "Chronic Illness", hea_chronic, n_hea_chronic)
hea_visit_labels <- get_label(map@data$NAME_2, "Lack of Health Visit", hea_visit, n_hea_visit)
employment_labels <- get_label(map@data$NAME_2, "Unemployment", employment, n_employment)
assets_labels <- get_label(map@data$NAME_2, "Household Assets", assets, n_assets)
services_labels <- get_label(map@data$NAME_2, "Access to Services", services, n_services)
electricity_labels <- get_label(map@data$NAME_2, "Lack of Electricity", electricity, n_electricity)
cooking_fuel_labels <- get_label(map@data$NAME_2, "Poor Cooking Fuel", cooking_fuel, n_cooking_fuel)
water_labels <- get_label(map@data$NAME_2, "Poor Water Source", water, n_water)
toilet_labels <- get_label(map@data$NAME_2, "Lack of Toilet", toilet, n_toilet)
land_labels <- get_label(map@data$NAME_2, "Lack of Land", land, n_land)
livestock_labels <- get_label(map@data$NAME_2, "Lack of Livestock", livestock, n_livestock)
rural_equip_labels <- get_label(map@data$NAME_2, "Lack of Rural Equipment", rural_equip, n_rural_equip)


pal <- colorNumeric(
  palette = "viridis",
  domain = switch(c_g_selection,
                  c(0, .6),
                  c(0, 1)),
  reverse = TRUE)

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
  get_polygon(map, pal, edu_max, edu_max_labels, "Max. Education") %>%
  get_polygon(map, pal, edu_dropout, edu_dropout_labels, "Education Dropout") %>%
  get_polygon(map, pal, hea_chronic, hea_chronic_labels, "Chronic Illness") %>%
  get_polygon(map, pal, hea_visit, hea_visit_labels, "Lack of Health Visit") %>%
  get_polygon(map, pal, employment, employment_labels, "Unemployment") %>%
  get_polygon(map, pal, assets, assets_labels, "Lack of Household Assets") %>%
  get_polygon(map, pal, services, services_labels, "Lack of Access to Services") %>%
  get_polygon(map, pal, electricity, electricity_labels, "Lack of Electricity") %>%
  get_polygon(map, pal, cooking_fuel, cooking_fuel_labels, "Poor Cooking Fuel") %>%
  get_polygon(map, pal, water, water_labels, "Poor Water Source") %>%
  get_polygon(map, pal, toilet, toilet_labels, "Lack of Toilet") %>%
  get_polygon(map, pal, land, land_labels, "Lack of Land") %>%
  get_polygon(map, pal, livestock, livestock_labels, "Lack of Livestock") %>%
  get_polygon(map, pal, rural_equip, rural_equip_labels, "Lack of Rural Equipment") %>%
  clearControls() %>%
  addLayersControl(
    baseGroups = c("Max. Education", 
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
  addLegend(pal = pal, values = c(0, 0.6), opacity = 0.7, title = paste0("Legend (with k = ", k_threshold, ")"),
            na.label = "No Data",
            group = c("Poverty Index", "Max. Education"),
            position = "bottomleft") %>%
  htmlwidgets::prependContent(html_fix)

