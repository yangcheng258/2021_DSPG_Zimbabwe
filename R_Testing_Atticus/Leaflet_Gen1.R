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
ZimMap <-  readOGR(dsn = paste0(getwd(),"/zwe_admbnda_adm2_zimstat_ocha_20180911"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")

# Loading the MPI data at the district level
MPIData = read.csv(file = 'MappingData.csv')


## DATA CLEANING ---------------------
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
ZimMap@data[["ADM2_EN"]] <- names

# Rename the district to match the shapefile
colnames(MPIData)[1] <- "ADM2_EN"


## LEAFLET MAP --------------------------

# Merges the MPI data with the shapefile data while preserving the order of 
# the shapefile names to match up with the polygons
ZimMap@data = merge(ZimMap@data, MPIData, by = c("ADM2_EN"), sort = FALSE)

pal <- colorNumeric(
  palette = "inferno",
  domain = ZimMap@data$M0_k3,
  reverse = TRUE)

labels <- sprintf(
    "<strong>%s</strong>
    <strong><br/>M<sub>0</sub>:</strong> %g
    <strong><br/>M<sub>1</sub>:</strong> %g
    <strong><br/>M<sub>2</sub>:</strong> %g",
    ZimMap@data$ADM2_EN, ZimMap@data$M0_k3, ZimMap@data$M1_k3, ZimMap@data$M2_k3
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

