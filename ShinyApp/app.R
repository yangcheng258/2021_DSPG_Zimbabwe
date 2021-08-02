
#Yang's seeting WD
# setwd("G:/My Drive/PhD/Internship/Zimbabwe/03_Git/2021_DSPG_Zimbabwe/ShinyApp")


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
library(ggplot2)
library(plotly)
library(ggrepel)
library(hrbrthemes)
library(rmapshaper)
gpclibPermit()
#rsconnect::configureApp("ShinyApp", account = "ecsusan-vt-2020-shiny", size="xxlarge")


## FORMATTING-------------------------------------------------------------------
prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2021 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic'); 
           }
           "

## LOADING DATA-----------------------------------------------------------------
# MAPS
MAP_2017_91_T_o <- ms_simplify(readOGR(dsn = "./data/shapefiles/91DistrictShapefiles", layer="zwe_admbnda_adm2_zimstat_ocha_20180911"))

MAP_2017_60_T_o <- ms_simplify(readOGR(dsn = "./data/shapefiles/60DistrictShapefiles", layer="gadm36_ZWE_2"))

MAP_2017_10_T_o <- ms_simplify(readOGR(dsn = "./data/shapefiles/ProvinceShapefiles", layer="zwe_admbnda_adm1_zimstat_ocha_20180911"))

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


## CAPTIONS---------------------------------------------------------------------

slider_caption = "Adjust the poverty line or cutoff threshold for an individual to be considered poor."
urban_rural_caption = "Choose between displaying data from all households, only urban households or only rural households"
level_caption = "Choose which poverty index to display the relevant data for"
c_g_caption = "Choose whether to display the percentage contribution to the specified poverty index or the raw calculation
of the gap for each component of the multidimensional poverty index.  Note: Users have to multiply by 100 to get the percentage contribution to MPI. E.g. 0.001*100 = 1%."


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
  metric <- metric %>% round(digits = 3)
  national_metric <- national_metric%>% round(digits = 3)
  label <- sprintf(
    paste0("<strong>%s</strong><br/>
    <strong>" , metric_name , ":</strong> %g<br/>
    <strong>National " , metric_name , ":</strong> %g"),
    name_data, round(metric,digits = 3), round(national_metric,digits = 3)) %>% lapply(htmltools::HTML)
  return(label)
}

# create function to make the scatterplots
create_scatter <- function(names, x_data, y_data, x_label, y_label, title) {
  
  M0_Comparison = data.frame(names, x_data, y_data)
  #print(x_data)
  #print(y_data)
  x_data <- round(x_data,digits = 3)
  y_data <- round(y_data,digits = 3)
  # replace NA with 0
  #M0_Comparison <- mutate_if(M0_Comparison, is.numeric, ~replace(., is.na(.), 0))
  
  colnames(M0_Comparison)[1] = "Name"
 
  
  return (plot_ly(M0_Comparison, x = ~x_data, y = ~y_data, text=~Name) %>%
   
               
            add_markers()%>%
            layout(shapes=list(type='line', x0=0, x1=0.6, y0=0, y1=0.6,name=" "), 
                   showlegend = FALSE,
                   xaxis=list(range = c(0,0.56),title = x_label), 
                   yaxis=list(range = c(0,0.56),title=y_label) )  
  )
  
  
}

## Ranking Function

# UI -------------------------------------------------------------

ui <- navbarPage(title = "Zimbabwe",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
#                  tags$script(HTML("var header = $('.navbar > .Zimbabwe');
# header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"github.png\" alt=\"alt\" style=\"float:right;width:33px;height:41px;padding-top:10px;\"> </a>`</div>');
#     console.log(header)")
#                  ),
                 ## Tab Overview -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   br(""),
                                   h1(strong("Using PICES Data to Visualize District Level Multidimensional Poverty in Zimbabwe")),
                                   # fluidRow(style = "margin: 2px;",
                                   #          img(src = "Zimbabwe_Flag.png", height="100", width="200", alt="Image", style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000;")),
                                   h4("Data Science for the Public Good Program"),
                                   h4("Virginia Tech"),
                                   h4("Department of Agriculture and Applied Economics")
                                   
                          ),
                          
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Project Overview"), align = "center"),
                                          p("Prior research suggests that poverty in Zimbabwe has increased since the period of crisis began at the turn of the millennium. According to the latest World Bank (2020) estimates, due to the longstanding economic crisis and disruptions following the COVID-19 pandemic, 49% of Zimbabwe’s population was in extreme poverty in 2020. Our stakeholders seek solutions to the economic situation. They would like more granular information presented in creative ways that allow the user to glean the multidimensional and temporal aspects of poverty in Zimbabwe. The recent availability of household surveys for public use has opened the possibility of using the data to inform evidence-based policy."),
                                          p("This project uses data from the Poverty, Income, Consumption, Expenditure Survey (PICES) to provide granular information on poverty in Zimbabwe. We created multidimensional poverty indices (MPI) at the", strong(" district and province level"), " and decomposed them into components that focus on ", strong("education, health, employment, housing conditions, living conditions, assets, agricultural assets, and access to services."),   
                                            "We provide interactive tools that allow the user to visualize and study each component and understand their contribution to the MPI. We constructed these measures for two waves of data in 2011 and 2017 to show the changes in poverty over time and across regions in Zimbabwe.  The composition and decomposition of MPI in this project provide evidence-based policy recommendations and interventions for poverty reduction. ")),
                                   column(4,
                                          h2(strong("Introduction to Zimbabwe"), align = "center"),
                                          p("Nestled in the Southeastern tip of Africa, Zimbabwe neighbors South Africa, Mozambique, Zambia, and Botswana. Zimbabwe gained independence from Great Britain in 1980 and was ruled by Prime Minister and eventually President Robert MUGABE until his resignation in 2017. Presently, President Emmerson Mnangagwa holds office. 
                                            The country is home to roughly 14,830,000 inhabitants, 10% of whom live in the capital city of Harare. Although large agglomerations exist in other major urban areas including Bulawayo and Chitungwiza, population distribution is relatively even otherwise. Zimbabwe’s central government is responsible for regulating 
                                            its 10 provinces and 59 further subdivided districts. Zimbabwe’'s terrain consists mostly of plateau upon which forests thrive and arable land is plenty. Because of this, 67.5% of the labor force works in agriculture growing sugar cane, tobacco, fruits, and vegetables among other things. Another 7.3% of the labor force 
                                            takes advantage of the Zimbabwe’s rich natural resources and participates in the industry sector mining and exporting coal, gold, platinum copper, and other metals as well as manufacturing wood products, cement, chemicals, fertilizer, and food. Despite being relatively well-educated and extremely literate, the population 
                                            suffers from both unemployment and severe underemployment in which individuals are overqualified for the jobs they have or are not given adequate work hours. In combination with ubiquitous low wages, this creates an obstacle for economic growth. Monetary poverty measures in 2017 revealed roughly 63% of Zimbabwean households 
                                            lived in poverty. This is reflected in income inequality, overall low standards of living, malnourishment, low life expectancy, high rates of infant/maternal mortality, and difficulty accessing health and education resources.")),
                                   
                                   column(4,
                                          h2(strong("Recent History"), align = "center"),
                                          p("After gaining independence in 1980, there was widespread hope that the economic and labor exploitation Africans suffered at the hands of an imperial Great Britain would diminish. While initial trends were encouraging, this hope dwindled as a multitude of factors sent the Zimbabwean economy into decline. Most prominent among 
                                            these factors was inconsistent policy prescriptions resulting in vague and evolving strategies on combatting poverty. An initial scientific socialist policy was applied between 1980 and 1990 to address poverty but was ineffective and thus abandoned due to financial downturn and prolonged drought that 
                                            forced agricultural workers into the cities where they faced even greater poverty due to unemployment. In an attempt to revamp the economy, Zimbabwe sought help from the International Monetary Fund (IMF) and the World Bank (WB) which meant an adoption of a more capitalistic policy. The costs of necessities including food, water, and 
                                            education went up as a result, harming and expanding the already existing poor population. The late 1990’s and 2000’s brought ever greater poverty and financial distress to Zimbabwe as a continuing government budget deficit mixed with a fiscal policy focused on increasing the amount of money in circulation resulted in hyperinflation. 
                                            In turn, this increased the economic crisis as foreign investment dropped and Zimbabwean currency crashed. During this time, unemployment skyrocketed and a massive informal sector of the economy emerged. In 2009, Zimbabwe adopted the U.S. dollar along with a handful of other currencies. Though this move somewhat stabilized the 
                                            economy at first, a 2013 shift in government rendered these efforts futile. By 2017, inflation increased significantly as did the overall economic crisis and poverty."))
                                   
                                   
                                
                                   
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 ## Tab data and methodology ----------------------------------------------------
                 navbarMenu("Data & Methodology", 
                            
                            tabPanel("Data", 
                                     fluidPage(
                                       h3(strong("Description of the PICES DATA")),
                                       withMathJax(),  
                                       p("The data come from two nationally representative household surveys, called the PICES, conducted by ZIMSTAT: first, from June 2011 to May 2012, and second, from January to December 2017. The PICES surveys are well suited to construct multidimensional poverty indices because they include information at the household and individual levels, and they are collected repeatedly. The surveys were conducted in the eight provinces of Zimbabwe and in the cities of Harare and Bulawayo. The number of usable observations (households) is 29,748 in 2011–2012 (23,843 rural and 5,905 urban) and 31,193 in 2017 (25,525 rural and 5668 urban). Survey weights and household size are employed to obtain national, provincial, and rural-urban representation. Both survey instruments are virtually identical across the two waves. They include information on household demographics, education, employment, healthcare, migration, housing characteristics, assets ownership, access to services, and agricultural activities."),
                                       h3(strong("Description of the Variables/Components")),
                                       img(src = "variables.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "80%"),
                                       withMathJax(), 
                                       p("To construct the multidimensional poverty index based on the Alkire-Foster method, we consider eight   poverty dimensions consisting of 14 variables relevant to identifying poverty status. The first dimension, education, consists of two variables – Max Education and Education Dropout. The Max Education variable refers to nobody in the household having completed primary school. We assess the sensitivity of the MPI by broadening these measures to nobody in the household having completed secondary school. The Education Dropout variable is an indicator variable for whether the household has a child aged 7-11 who is not enrolled in school. The education dimension receives the greatest weight in the MPI (2 out of 9.5), along with the two health variables that make up the second health dimension (2 out of 9.5). These two variables are Chronic Illness, referring to the presence of a chronically ill individual within the household, and Lack of Health Visit, which refers to a household member who has been sick in the past 30 days without receiving a necessary healthcare."),
                                       p("Unemployment, defined as one member of the household having been unemployed as their main occupation in the last 12 months, is given a weight of 1 for urban households and 0 for rural households since unemployment is less common and is more difficult to identify in rural areas.  "),
                                       p("For housing conditions, two variables are considered: lack of access to electricity and no toilet (in rural areas) or no flush toilet (for urban areas with more developed sanitation). Weights of 0.5 are given to rural residence Lack of Electricity and Lack of Toilet indicators underlying the dimension. In urban areas, where lack of electricity indicates a greater state of deprivation, a weight of one is attributed to electricity. In contrast, the lack of a toilet retains a weight of 0.5."),
                                       p("Two variables reflect living conditions: Poor Water Source and Poor Cooking Fuel, with a weight of 0.5 for each. Rural households are considered to be deprived if their main water source is an unprotected well, a river, or another unprotected source, or if the water source is 1 km away or farther. In urban areas with more developed water infrastructure, deprivation is defined as not having access to piped water or communal water on-premises (which affects only a small number of households). In rural and urban areas, households are deprived if they use wood or ’other’ (not electricity, paraffin, gas, coal) as cooking fuel.  "),
                                       p("Lack of Household Assets is given a dimension weight of 1 in both rural and urban areas. The stock of household assets is measured by a physical asset index (PAI) and an asset deprivation (D) threshold as follows:  "),
                                       
                                       
                                       p(" \\(PAI = 2 * motor vehicle + motorcycle + bicycle + television + radio + fridge + landline phone\\)   ", align = "center"),
                                       p("\\( D = 1 \\)", " if ","\\(PAI < 2 \\)", align = "center"),
                                       p("For rural households, agricultural assets are essential indicators of wellbeing and agricultural activity capabilities. The dimension weight is 1.5, with three component variables usually given a weight of 0.5 each. The first variable, Lack of Land, uses a threshold of 0.25 hectares, which is sufficiently low to represent effective deprivation of landholding in rural Zimbabwe. The second variable on livestock is measured in Tropical Livestock Units (TLU), an indicator of wealth that can be used to insulate households from negative idiosyncratic and covariateshocks. A TLU deprivation threshold of 1 indicates  Lack of Livestock. The third variable is the Lack of Rural Equipment. An agricultural equipment index (AEI) is created as follows:  "),
                                       p("\\( AEI = plough + wheelbarrow + scotchcart + tractor + griding mill \\)", align = "center"),
                                       p("\\( D = 1 \\)", " if ","\\(AEI < 1 \\)", align = "center"),
                                       p("The agricultural asset dimension is not included for households in urban areas.  "),
                                       p("The final dimension of wellbeing – with a weight of 1 – is Lack of Access to Services, where remoteness indicates deprivation. Households are considered deprived if they are far from two or more of seven recorded services in the data. The distance thresholds employed are 5 km for a primary school, 15 km for a secondary school, 15 km for a hospital, 5 km for shops, 6 km for a hammer mill, 15 km for a post office, and 5 km for a bus stop, respectively. These distance thresholds are halved in urban areas, where services tend to be closer, but distance still represents a barrier to access."),
                                      p("**Note: The livestock data were not available in the 2011-12 wave, which limited our ability to compare the change in livestock dimension across time. To account for this, we have assigned the Lack of Livestock variable a weight of zero and divided the weight proportionally between the other two agricultural asset variables. We use this adjusted index to compare the MPI for 2011 and 2017."),
                                       h3(strong("Sensitivity Check")),
                                       p("Our stakeholders believe that given the country’s high level of literacy, a higher education threshold would more accurately represent education (or the lack thereof) in the Zimbabwean context. To understand how sensitive the MPI measures are to a change in definition, we construct an adjusted MPI. The adjusted MPI assumes that a household is deprived if no one in the household has attained a secondary school education. This is an expansion of the original definition in Alkire-Foster MPI, which sets the threshold at primary school."),
                                      p("We present this sensitivity analysis in the MPI Mapping tab. Select the education-adjusted MPI to compare with the original Alkire-Foster MPI results with the lower education threshold.")
                                     )
                                     
                                     
                                     
                                     
                            ),
                            tabPanel("Methodology", 
                                     fluidPage(
                                      box(
                                         withMathJax(),
                                         title = h3(strong("MPI Methodology")),
                                         width = 12,
                                         em(h4("A brief overview of the Mathematics behind the Multidimensional Poverty Index")), tags$br(),
                                         p("The aggregate methodology for determining the multidimensional poverty 
       indices proposed by Alkine and Foster in 2011 involve a matrix with \\(n\\) 
       rows and \\(d\\) columns, where \\(n\\) is the number of people within the 
       state and \\(d\\) is the number of dimensions for assessing poverty. There 
       are three main measurements denoted on the \\(M\\) scale: \\(M_{0}, M_{1}\\) and \\(M_{2}\\).
       The A-F method employed in this study contains eight dimensions of poverty. 
       Within each dimension, there are one or two variables that indicate whether 
       an individual is deprived in that area. Each variable has a specific
       weight associated with it depending on its contribution to overall poverty
       and how it pertains to rural and urban communities differently. For a given 
       individual, the total number of deprivations are added up and if he or she falls
       above a given threshold, \\(k\\), then that individual is considered poor. 
       Having multiple dimensions of poverty allows us to decompose the original 
       measure into its individual variables to identify which are contributing 
       most to the overall index of poverty."),
                                         tags$br(),
                                         p("The \\(M_{0}\\) index is known as the Adjusted Headcount Ratio. The simple headcount
       ratio is simply the number of individuals considered to be poor divided by
       the entire population. The \\(M_{0}\\) index adjusts for the multidimensionality
       of the algorithm by multiplying the simple headcount ratio, \\(H\\), by the 
       average deprivation share, \\(A\\). This metric can be thought of as a more
       accurate measure of the simple headcount ratio."),
                                         tags$br(),
                                         p("The \\(M_{1}\\) index is known as the Adjusted Poverty Gap. This examines the distance
       between the prescribed threshold, \\(k\\), and an individual","'","s true number of 
       deprivations. This helps examine the subset of poor individuals to efficiently
       assess which individuals are the poorest in the country."),
                                         tags$br(),
                                         p("The \\(M_{2}\\) index is known as the Adjusted Poverty Severity. This is
       simply the square of the distance between a poor individual and the poverty
       threshold, \\(k\\). The advantage of using this metric is that it weights
       poorer individuals who fall farther from the poverty line more heavily to 
       provide a more obvious descriptor for the poorest people in a given area."),
                                         tags$br()
                                         ),
                                       box(
                                         width = 6,
                                         h5(strong("Headcount Ratio")),
                                         h3("\\(H = \\frac{n_{poor}}{n_{pop}}\\)"),
                                         tags$br(),
                                         h5(strong("Average Deprivation Share")),
                                         h3("\\(A = \\frac{n_{deprivations}}{n_{potential}}\\)"),
                                         tags$br(),
                                         h5(strong("Deprivation Threshold")),
                                         h5(em("\\(k\\) = Threshold (If an index is above threshold, k, then the individual is considered poor)")),
                                         tags$br(),
                                         h5(strong("Dimensional Aggregation")),
                                         h4("\\(D_{total} = \\sum_{i=1}^{d}\\sum_{j=1}^{v_{d}} w_{i, j}\\)"),
                                         em(p("\\(d = \\) Number of Dimensions")),
                                         em(p("\\(v_{d} = \\) Number of variables for a Specific Dimension")),
                                         em(p("\\(w_{i,j} = \\) Weight of a Specific Variable for a Specific Dimension"))
                                         
                                         
                                       ),
                                      box(
                                         width = 6,
                                         h5(strong("Poverty Index")),
                                         h4("\\(M_{0}= H * A\\)"),
                                         tags$br(),
                                         h5(strong("Adjusted Poverty Gap")),
                                         h4("\\(M_{1} = μ(g^{1}(k))\\)"),
                                         h4("\\(g^{1}_{i} = k - \\frac{\\sum deprivations}{\\sum possible\\ deprivations}\\)   if   \\(g^{1}_{i} > 0\\)"),
                                         h4("Else \\(g^{1}_{i} = 0\\)"),
                                         tags$br(),
                                         h5(strong("Adjusted Poverty Severity")),
                                         h4("\\(M_{2} = μ(g^{2}(k))\\)"),
                                         h4("\\(g^{2}_{i} = [k - \\frac{\\sum deprivations}{\\sum possible\\ deprivations}]^{2}\\) if \\(g^{2}_{i} > 0\\)"),
                                         h4("Else \\(g^{2}_{i} = 0\\)")
                                         
                                       )
                                     )
                    )
                 ),
                 
                 ## Tab MPIPING MPI --------------------------------------------------------------------
                 navbarMenu("MPI Mapping", 
                             tabPanel("91 District MPI Map",
                                                 
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "91 District MPI Map of Zimbabwe",
                                         
                                         withSpinner(leafletOutput("Dist_91_MPI_Map",height = 520)),
                                         width = 8,
                                         height = 600
                                       ),
                                       
                                       box(
                                         width = 4,
                                         withMathJax(),
                                         title = "Description",
                                         p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:
                                      \\(M_{0}\\), \\(M_{1}\\), and \\(M_{2}\\)."), 
                                         tags$ul(  
                                           tags$li("\\(M_{0}\\) is the ",strong("adjusted headcount ratio")," designed by",a(href="https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/","Sabina Alkire and James Foster",target="_blank"),
                                                   " and considers all of the dimensions described in the methodology section."),
                                           tags$li("\\(M_{1}\\)
                                      is the ",strong("adjusted poverty gap")," an index to show how far below the poor people are from the poverty line."),
                                           tags$li("\\(M_{2}\\) is the ",strong("square of the adjusted poverty gap."),"By squaring the poverty gaps, this measure puts a higher weight on those who are farther away from the poverty line. Thus, this index measures severity of poverty.")
                                           
                                         ),
                                         p("To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      adjust the slider below the graph.")
                                       )),
                                     fluidRow(
                                       br()
                                     ),
                                     fluidRow(  
                                     box(
                                         sliderInput("slider_91_MPI", strong("k-Threshold Value"), 1, 9, 3),
                                         width = 4,
                                         footer = slider_caption
                                       ),
                                       box(
                                         width = 4,
                                         radioButtons("UrbRurSelection_MPI_91",strong("Select Urban/Rural Filter"), 
                                                      choiceNames = c("All",
                                                                      "Urban",
                                                                      "Rural"),
                                                      choiceValues = c(1, 2, 3)),
                                         footer = urban_rural_caption
                                       ),
                                       box(
                                         width = 4,
                                         radioButtons("SensitivitySelection_91", strong("Select Which MPI to Use:"),
                                                      choiceNames = c("Original MPI",
                                                                      "Education-Adjusted MPI"),
                                                      choiceValues = c(1, 2))
                                         
                                       )),
                                     
                                     fluidRow(
                                       br()
                                     ),
                                       fluidRow(
                                     box(
                                         withMathJax(),
                                         title = strong("Descriptive Analysis of 91 Districts"),
                                         width = 12,
                                         p("\\(M_{0}\\)"),
                                         p("Looking at the original poverty index and focusing on the \\(M_{0}\\) index, we can see that for low k-threshold values, a large portion of the population can be considered multidimensionally poor. Additionally, urban districts and urban households tend to have lower \\(M_{0}\\) scores than their rural counterparts. As we increase the k-threshold values, thereby increasing the criteria to be labeled multidimensionally poor, fewer people across the country can be identified as such. The greater Harare and Bulawayo areas have low \\(M_{0}\\) values for low k-thresholds. Still, their \\(M_{0}\\) values for higher k-thresholds are above the national average, implying that while those districts are better on average, some of the most poverty-stricken households reside within their bounds (particularly the Epworth district)."),
                                         em("Sensitivity Analysis: "),
                                         p("When we consider how sensitive the MPI is to a change in the education variable to the highest level of attainment being less than a secondary school education, we see that, on average, the (adjusted-headcount) \\(M_{0}\\) scores are higher. More individuals are now more likely to be considered education-deprived than the original \\(M_{0}\\)  index. Similar trends exist for the adjusted \\(M_{0}\\) index as the original \\(M_{0}\\) index – fewer households are considered multidimensionally poor as we increase k. Urban districts & households tend to have lower \\(M_{0}\\) scores, and some of the households most vulnerable to multidimensional poverty are present around the biggest cities. "),
                                         
                                         
                                         p("\\(M_{1}\\)"),
                                         p("When we focus on the depth of poverty (\\(M_{1}\\) index ), if the k-thresholdvalues are low, poverty throughout much of Zimbabwe can be considered deep.  A majority of \\(M_{1}\\) values exceed the national \\(M_{1}\\) value. Similar to the \\(M_{0}\\) trends, urban districts tend to have lower \\(M_{1}\\) values than rural districts, implying deeper poverty in rural districts. Although the number of districts portraying deep poverty generally decreases as k-threshold values increase, this is not the case for rural districts neighboring Harare, including Bindura, Goromonzi, and Marondera. These areas maintain high \\(M_{1}\\) values as k-threshold values increase, as do a cluster of districts in the country’s southeastern region."),
                                         em("Sensitivity Analysis: "),
                                         p("Deeper poverty is seen when the level of education is changed from primary school to secondary school. As k-threshold values increase, \\(M_{1}\\) values predictably decrease on average. Rural \\(M_{1}\\) values are, on average, higher than urban \\(M_{1}\\) values, with few exceptions in Umguza, Bubi, and Mutaza."),
                                         p(" Districts with \\(M_{1}\\) values do not follow this pattern but maintain relatively high \\(M_{1}\\) values are in southwestern Zimbabwe."),
                                         
                                         p("\\(M_{2}\\)"),
                                         p("A look at the \\(M_{2}\\) values of the original index reveals much of the same. Low k-threshold values render high rates of poverty severity across a large proportion of Zimbabwe’s population. As k-threshold values increase, \\(M_{2}\\) values fall throughout most of the country but remain substantially high in the western portion of the country and around Harare, implying a greater number of impoverished households are further away from the poverty line than other impoverished households in these regions. If we distinguish between urban and rural, we can see that urban districts tend to have less severe poverty than rural districts, excluding the urban aggregates in Umguza, Bubi, and Mutasa. "),
                                         em("Sensitivity Analysis: "),
                                         p("As is the case with the \\(M_{0}\\) and \\(M_{1}\\) indexes, a look at the Education-adjusted MPI shows an increase in \\(M_{2}\\) values across the board. It is reasonable to conclude that adding the population that has not completed secondary school to the education variable is significant as it consistently results in higher MPI values. Taken together, our findings suggest that an expanded education threshold captures a substantial part of the population deprived in the education dimension as \\(M_{0}\\), \\(M_{1}\\), and \\(M_{2}\\) values all increase substantially. Interestingly, when selected for urban districts, the cities of Harare and Bulawayo show relatively high \\(M_{2}\\) values as the k-threshold value increases, raising the possibility of disproportionate education deprivation in the two major urban landscapes of Zimbabwe. "),
                                         p("")
                                       )
                                     )
                                     ),
                            tabPanel( "60 District MPI Map",
                                      # tabName = "60_Dist",
                                      fluidPage(
                                        fluidRow(
                                        box(
                                          title = "60 District MPI Map of Zimbabwe",
                                          withSpinner(leafletOutput("Dist_60_MPI_Map",height = 520)),
                                          width = 8,
                                          height = 600
                                        ),
                                        box(
                                          width = 4,
                                          withMathJax(),
                                          title = "Description",
                                          p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:
                                      \\(M_{0}\\), \\(M_{1}\\), and \\(M_{2}\\)."), 
                                          tags$ul(  
                                            tags$li("\\(M_{0}\\) is the ",strong("adjusted headcount ratio")," designed by",a(href="https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/","Sabina Alkire and James Foster",target="_blank"),
                                                    " and considers all of the dimensions described in the methodology section."),
                                            tags$li("\\(M_{1}\\)
                                      is the ",strong("adjusted poverty gap")," an index to show how far below the poor people are from the poverty line."),
                                            tags$li("\\(M_{2}\\) is the ",strong("square of the adjusted poverty gap."),"By squaring the poverty gaps, this measure puts a higher weight on those who are farther away from the poverty line. Thus, this index measures severity of poverty.")
                                            
                                          ),
                                          p("To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      adjust the slider below the graph.")
                                        )),
                                        fluidRow(
                                          br()
                                        ),
                                        fluidRow(
                                        box(
                                          sliderInput("slider_60_MPI", strong("k-Threshold Value"), 1, 9, 3),
                                          width = 4,
                                          footer = slider_caption
                                        ),
                                        box(width = 4,
                                          radioButtons("UrbRurSelection_MPI_60", strong("Select Urban/Rural Filter"), 
                                                       choiceNames = c("All",
                                                                       "Urban",
                                                                       "Rural"),
                                                       choiceValues = c(1, 2, 3)),
                                          footer = urban_rural_caption
                                        ),
                                        box(width = 4,
                                          radioButtons("SensitivitySelection_60", strong("Select Which MPI to Use:"),
                                                       choiceNames = c("Original MPI",
                                                                       "Education-Adjusted MPI"),
                                                       choiceValues = c(1, 2))
                                          
                                        )),
                                        fluidRow(
                                          br()
                                        ),
                                        box(
                                          withMathJax(),
                                          title = strong("Descriptive Analysis of 60 Distirct"),
                                          width = 12,
                                          p("\\(M_{0}\\)"),
                                          p("When viewing \\(M_{0}\\) values at the 60-district level, it is clear that a majority of Zimbabwe’s districts can be categorized as multidimensionally poor as most exceed the national \\(M_{0}\\) value. As k-threshold values increase, making the criteria for poverty more severe, most districts exhibit very low \\(M_{0}\\) values while a cluster of districts (Lupane, Nkayi, Tsholotsho, Bulilima, Mangwe, and Matobo) in the western part of the country maintain high \\(M_{0}\\) values. The region northeast of Harare holds similar \\(M_{0}\\) values at a high k-threshold. This trend indicates high rural poverty incidence at two intersections, one between Matabeleland North and Matabeleland South and Mashonaland Central and Mashonaland East. When defined by aggregated urban households, most districts fall below the national \\(M_{0}\\) value, indicating less poverty incidence in urban areas than in their rural counterparts and the nation at large. "),
                                          em("Sensitivity Analysis: "),
                                          p("Looking at the \\(M_{0}\\) measure when MPI is adjusted for the sensitivity analysis, we see district \\(M_{0}\\) values increase consistently throughout Zimbabwe. As k-threshold values increase, the majority of districts move out of multidimensional poverty. The southwest region and the northeast region surrounding Harare are exceptions. The districts of Bindura and Marondera exhibit high \\(M_{0}\\) values at high k-thresholds, suggesting a very high poverty incidence. When we compare urban and rural households, it becomes clear that the rural regions have a higher prevalence of poverty than their rural counterparts. ."),
                                          p("\\(M_{1}\\)"),
                                          p("When we look at \\(M_{1}\\) values for the 60-districts map, we see that the majority of Zimbabwe exhibits deep multidimensional poverty. Many many of the districts have \\(M_{1}\\) values higher than the national average. As k-threshold values increase, \\(M_{1}\\) values tend to decrease, although outliers can be found in the northwestern region of the country as well as the region surrounding Harare. Distinguishing between urban and rural reveals that urban areas tend to have more shallow poverty than the national average as well as their rural counterparts. "),
                                          em("Sensitivity Analysis: "),
                                          p("The adjusted \\(M_{1}\\) value is higher, suggesting deeper levels of poverty than the original dimension. As the k-threshold values are increased, the number of households that exist far above the poverty line decrease. The urban and rural disparities described above remain."),
                                          p("\\(M_{2}\\)"),
                                          p("Poverty severity, reflected by the \\(M_{2}\\) value, indicates moderate poverty severity throughout Zimbabwe that tends to decrease as k-threshold values increase. Urban areas consistently have lower \\(M_{2}\\) values and thus less severe poverty than the national average and rural areas. Poverty severity remains a problem in both Bulawayo and Harare. Rural areas consistently have \\(M_{2}\\) values higher than urban areas and the nation, revealing the presence of relatively severe multidimensional poverty in northwest regions.  "),
                                          em("Sensitivity Analysis: "),
                                          p("The severity of poverty throughout Zimbabwe increases slightly as the education dimension is expanded to account for secondary schooling. When urban households are selected at high k-threshold values, Harare and Bulawayo become the only two districts with \\(M_{2}\\) values not equal to zero, implying greater severity due to lack of secondary education in these districts than elsewhere. Rural districts consistently exhibit higher \\(M_{2}\\) values than urban districts and have more variation in \\(M_{2}\\) value when k-threshold values are adjusted."),
                                          p("")
                                        )
                                      )
                                      
                            ),
                            
                            ## province MPI-------------
                            tabPanel("Province MPI Map",
                                     # tabName = "Prov",
                                     fluidPage(
                                       fluidRow(
                                       box(
                                         title = "Province-Level MPI Map of Zimbabwe",
                                         withSpinner(leafletOutput("Prov_MPI_Map",height = 520)),
                                         width = 8,
                                         height = 600
                                       ),
                                       box(
                                         width = 4,
                                         withMathJax(),
                                         title = "Description",
                                         p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:
                                      \\(M_{0}\\), \\(M_{1}\\), and \\(M_{2}\\)."), 
                                         tags$ul(  
                                           tags$li("\\(M_{0}\\) is the ",strong("adjusted headcount ratio")," designed by",a(href="https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/","Sabina Alkire and James Foster",target="_blank"),
                                                   " and considers all of the dimensions described in the methodology section."),
                                           tags$li("\\(M_{1}\\)
                                      is the ",strong("adjusted poverty gap")," an index to show how far below the poor people are from the poverty line."),
                                           tags$li("\\(M_{2}\\) is the ",strong("square of the adjusted poverty gap."),"By squaring the poverty gaps, this measure puts a higher weight on those who are farther away from the poverty line. Thus, this index measures severity of poverty.")
                                           
                                         ),
                                         p("To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      adjust the slider below the graph.")
                                       )),
                                       fluidRow(
                                         br()
                                       ),
                                       fluidRow(
                                       box(
                                         sliderInput("slider_10_MPI", strong("k-Threshold Value"), 1, 9, 3),
                                         width = 4,
                                         footer = slider_caption
                                       ),
                                       box(width = 4,
                                         radioButtons("UrbRurSelection_MPI_10", strong("Select Urban/Rural Filter"), 
                                                      choiceNames = c("All",
                                                                      "Urban",
                                                                      "Rural"),
                                                      choiceValues = c(1, 2, 3)),
                                         footer = urban_rural_caption
                                       ),
                                       box(width = 4,
                                         radioButtons("SensitivitySelection_10", strong("Select Which MPI to Use:"),
                                                      choiceNames = c("Original MPI",
                                                                      "Education-Adjusted MPI"),
                                                      choiceValues = c(1, 2))
                                         
                                       )
                                       ),
                                       fluidRow(
                                         br()
                                       ),
                                       box(
                                         withMathJax(),
                                         title = strong("Descriptive Analysis of Province"),
                                         width = 12,
                                         
                                         em("\\(M_{0}\\)"),
                                         p("While province-level MPI measures can provide insight into regional poverty trends, this analysis is limited in its ability to highlight variations inside each province. Nonetheless, measures of poverty incidence show slight deviation across different provinces. However, the city provinces of Bulawayo and Harare stand out from the others insofar as they exhibit lower province-level \\(M_{0}\\) values than the other provinces. As the k-threshold value increases, province-level \\(M_{0}\\) values decrease. Interestingly, Harare’s \\(M_{0}\\) value exceeds those of the other provinces at a high k-threshold, signaling the existence of multidimensional poverty even at high poverty thresholds. When urban households are selected, \\(M_{0}\\) values decrease throughout all provinces, especially Masvingo and Mashonaland West. A switch to rural households results in increased \\(M_{0}\\) values across the board. "),
                                         em("Sensitivity Analysis: "),
                                         p("Provinces reflect substantially higher \\(M_{0}\\) values following ongoing trends when the education adjustment is made. Mashonaland Central, Mashonaland East, Matabeleland North, and Matabeleland South are significantly impacted by this adjustment and show \\(M_{0}\\) values nearing 0.5. Increases in the k-threshold show decreased \\(M_{0}\\) values throughout, although Harare and Mashonaland East have relatively high values. The urban and rural split reveals disparity among rural households when compared to their urban counterparts. "),
                                         em("\\(M_{1}\\): "),
                                         p("For \\(M_{1}\\), the map above shows that moderate province-level \\(M_{1}\\) values are evenly dispersed throughout Zimbabwe with slight disparities in Matabeleland North and Matabeleland South. An increase in the k-threshold decreases \\(M_{1}\\) values at a greater rate in the southeastern region of Zimbabwe than in the western and northeastern portions. Urban households in urban provinces tend to have lower \\(M_{1}\\) values than rural households in provinces with lower population density. "),
                                         em("Sensitivity Analysis: "),
                                         p("Adjusting for the increased education threshold results in uniformly higher \\(M_{1}\\) values in all provinces, again with a slight disparity in Matabeleland North and Matabeleland South. When provinces are divided by population density, urban households tend to have lower \\(M_{1}\\) values than rural households. "),
                                         em("\\(M_{2}\\)"),
                                         p("For \\(M_{2}\\), the poverty severity measure, the maps show consistent values throughout Zimbabwe’s provinces. Midlands, Mashonaland West, Manicaland, and the city provinces reflect lower poverty severity than other provinces. Increasing the k-threshold value reveals relatively high \\(M_{2}\\) values in the city provinces and extends into the surrounding provinces. Poverty severity in rural households is higher than it is at the overall province level and the urban household level. "),
                                         em("Sensitivity Analysis: "),
                                         p("The sensitivity analysis on the education component shows that the \\(M_{2}\\) values increase across all provinces as more households become deprived in the education dimension. With this selection, rural households exhibit higher \\(M_{2}\\) values than their urban counterparts.")
                                       )
                                     )
                            )
                            
                            
                 ),
                 # Tab MPI Ranking-----------------
                 navbarMenu("MPI Rankings",
                            ### 91 DIstricts ranking--------
                            tabPanel("91 District Rankings",
                                     
                                     tabsetPanel(
                                       tabPanel("M0",
                                                fluidRow(
                                                  box(width = 8,
                                                      withSpinner(plotlyOutput("M0_dis_91_ranking", height = 950))
                                                  ),
                                                  box(withMathJax(),
                                                      width = 4,
                                                      title = "Description",
                                                      p("This graphic portrays a ranked ordering of the \\(M_{0}\\) values of 91 districts. Displaying the data this way reveals where each district stands in relationship with one another and, 
                                                      when used to supplement the map, helps make clear geographically clustered poverty. To view the \\(M_{0}\\) value of a specific district, hover over the respective bar. To view the ranked 
                                                     order of each district at different k-thresholds, adjust the slider to the desired value.")),
                                                  box(sliderInput("M0_k_91_threshold", strong("k-Threshold"), 1, 9, 3),
                                                      width = 4,
                                                      footer = slider_caption)
                                                )),
                                       
                                       tabPanel("M1",
                                                fluidRow(
                                                  box(width = 8,
                                                      withSpinner(plotlyOutput("M1_dis_91_ranking", height = 950))
                                                  ),
                                                  box(withMathJax(),
                                                      width = 4,
                                                      title = "Description",
                                                      p("This graphic portrays a ranked ordering of the \\(M_{1}\\) values of each district. Displaying the data this way reveals where each district stands in relationship with one another and, 
                                                      when used to supplement the map, helps make clear geographically clustered poverty. To view the \\(M_{1}\\) value of a specific district, hover over the respective bar. To view the ranked 
                                                     order of each district at different k-thresholds, adjust the slider to the desired value.")),
                                                  box(sliderInput("M1_k_91_threshold", strong("k-Threshold"), 1, 9, 3),
                                                      width = 4,
                                                      footer = slider_caption)
                                                )),
                                       
                                       tabPanel("M2",
                                                fluidRow(
                                                  box(width = 8,
                                                      withSpinner(plotlyOutput("M2_dis_91_ranking", height = 950))
                                                  ),
                                                  box(withMathJax(),
                                                      width = 4,
                                                      title = "Description",
                                                      p("This graphic portrays a ranked ordering of the \\(M_{2}\\) values of each district. Displaying the data this way reveals where each district stands in relationship with one another and, 
                                                      when used to supplement the map, helps make clear geographically clustered poverty. To view the \\(M_{2}\\) value of a specific district, hover over the respective bar. To view the ranked 
                                                     order of each district at different k-thresholds, adjust the slider to the desired value.")),
                                                  box(sliderInput("M2_k_91_threshold", strong("k-Threshold"), 1, 9, 3),
                                                      width = 4,
                                                      footer = slider_caption)
                                                ))
                                     )),
                            
                            
                            
                            ### DIstricts ranking--------
                            tabPanel("60 District Rankings",
                                     #tabName = "rank_60",
                                     tabsetPanel(
                                       tabPanel("M0",
                                                fluidRow(
                                                  box(width = 8,
                                                      withSpinner(plotlyOutput("M0_ranking", height = 950))
                                                  ),
                                                  box(withMathJax(),
                                                      width = 4,
                                                      title = "Description",
                                                      p("This graphic portrays a ranked ordering of the \\(M_{0}\\) values of each district. Displaying the data this way reveals where each district stands in relationship with one another and, 
                                                      when used to supplement the map, helps make clear geographically clustered poverty. To view the \\(M_{0}\\) value of a specific district, hover over the respective bar. To view the ranked 
                                                     order of each district at different k-thresholds, adjust the slider to the desired value.")),
                                                  box(sliderInput("M0_k_threshold", strong("k-Threshold"), 1, 9, 3),
                                                      width = 4,
                                                      footer = slider_caption)
                                                )),
                                       
                                       tabPanel("M1",
                                                fluidRow(
                                                  box(width = 8,
                                                      withSpinner(plotlyOutput("M1_ranking", height = 950))
                                                  ),
                                                  box(withMathJax(),
                                                      width = 4,
                                                      title = "Description",
                                                      p("This graphic portrays a ranked ordering of the \\(M_{1}\\) values of each district. Displaying the data this way reveals where each district stands in relationship with one another and, 
                                                      when used to supplement the map, helps make clear geographically clustered poverty. To view the \\(M_{1}\\) value of a specific district, hover over the respective bar. To view the ranked 
                                                     order of each district at different k-thresholds, adjust the slider to the desired value.")),
                                                  box(sliderInput("M1_k_threshold", strong("k-Threshold"), 1, 9, 3),
                                                      width = 4,
                                                      footer = slider_caption)
                                                )),
                                       
                                       tabPanel("M2",
                                                fluidRow(
                                                  box(width = 8,
                                                      withSpinner(plotlyOutput("M2_ranking", height = 950))
                                                  ),
                                                  box(withMathJax(),
                                                      width = 4,
                                                      title = "Description",
                                                      p("This graphic portrays a ranked ordering of the \\(M_{2}\\) values of each district. Displaying the data this way reveals where each district stands in relationship with one another and, 
                                                      when used to supplement the map, helps make clear geographically clustered poverty. To view the \\(M_{2}\\) value of a specific district, hover over the respective bar. To view the ranked 
                                                     order of each district at different k-thresholds, adjust the slider to the desired value.")),
                                                  box(sliderInput("M2_k_threshold", strong("k-Threshold"), 1, 9, 3),
                                                      width = 4,
                                                      footer = slider_caption)
                                                ))
                                     )),
                            ### Province ranking-------------
                            tabPanel("Province Rankings",
                                     tabsetPanel(
                                       tabPanel("M0",
                                                fluidRow(
                                                  box(width = 8,
                                                      withSpinner(plotlyOutput("M0_prov_ranking", height = 950))
                                                  ),
                                                  box(withMathJax(),
                                                      width = 4,
                                                      title = "Description",
                                                      p("This graphic portrays a ranked ordering of the \\(M_{0}\\) values of each province. Displaying the data this way reveals where each province stands in relationship with one another and, 
                                                      when used to supplement the map, helps make clear geographically clustered poverty. To view the \\(M_{0}\\) value of a specific province, hover over the respective bar. To view the ranked 
                                                     order of each province at different k-thresholds, adjust the slider to the desired value.")),
                                                  box(sliderInput("M0_prov_k", strong("k-Threshold"), 1, 9, 3),
                                                      width = 4,
                                                      footer = slider_caption)
                                                )),
                                       
                                       tabPanel("M1",
                                                fluidRow(
                                                  box(width = 8,
                                                      withSpinner(plotlyOutput("M1_prov_ranking", height = 950))
                                                  ),
                                                  box(withMathJax(),
                                                      width = 4,
                                                      title = "Description",
                                                      p("This graphic portrays a ranked ordering of the \\(M_{1}\\) values of each province. Displaying the data this way reveals where each province stands in relationship with one another and, 
                                                      when used to supplement the map, helps make clear geographically clustered poverty. To view the \\(M_{1}\\) value of a specific province, hover over the respective bar. To view the ranked 
                                                     order of each province at different k-thresholds, adjust the slider to the desired value.")),
                                                  box(sliderInput("M1_prov_k", strong("k-Threshold"), 1, 9, 3),
                                                      width = 4,
                                                      footer = slider_caption)
                                                )),
                                       tabPanel("M2",
                                                fluidRow(
                                                  box(width = 8,
                                                      withSpinner(plotlyOutput("M2_prov_ranking", height = 950))
                                                  ),
                                                  box(withMathJax(),
                                                      width = 4,
                                                      title = "Description",
                                                      p("This graphic portrays a ranked ordering of the \\(M_{2}\\) values of each province. Displaying the data this way reveals where each province stands in relationship with one another and, 
                                                      when used to supplement the map, helps make clear geographically clustered poverty. To view the \\(M_{2}\\) value of a specific province, hover over the respective bar. To view the ranked 
                                                     order of each province at different k-thresholds, adjust the slider to the desired value.")),
                                                  box(sliderInput("M2_prov_k", strong("k-Threshold"), 1, 9, 3),
                                                      width = 4,
                                                      footer = slider_caption)
                                                ))
                                     )
                                     )
                            
                            
                            ),
                 
                 ## Tab Decomposition------------------
                 navbarMenu("MPI Decomposition",
                     
                            tabPanel(
                              "91 District MPI Map",
                               fluidPage(
                                fluidRow(
                                box(
                                  title = "91 District Decomposition Map of Zimbabwe",
                                  withSpinner(leafletOutput("Dist_91_Decomp_Map",height = 520)),
                                  p(strong("Note: The map always resets to Max Education.")),
                                  width = 8,
                                  height = 600
                                ),
                                tags$br(),
                                tags$br(),
                                
                                box(
                                  withMathJax(),
                                  title = "Description",
                                  p("This page presents the unidimensional indices for the individual components that make up the MPI.  The dropdown menu allows the user to display the unidimensional index (i.e., the health, education, or asset component) or to display the component’s contribution to the multidimensional index. This can be shown at the district or province level.   The user can display these values for rural or urban areas and with different k-threshold values.  "), 
                                  
                                  p("Note: for our district-level analysis, a grey-filled area with an NA means that no districts fulfill the criteria chosen. For example, this would apply if there are high k-thresholds and no households exist at this level, or for the regional analysis if there are no urban or rural households.These results are presented for the incidence (\\(M_{0}\\)), gap (\\(M_{1}\\)), and severity of poverty (\\(M_{2}\\)). "),
                                  width = 4
                                )),
                                
                                fluidRow(
                                  br()
                                ),
                                  fluidRow(
                                    box(width = 6,
                                  sliderInput("slider_91_Decomp", strong("\n k-Threshold Value"), 1, 9, 3),
                                  footer = slider_caption
                                ),
                              
                                box(width = 6,
                                  radioButtons("UrbRurSelection_Decomp_91", strong("\n Select Urban/Rural Filter"), 
                                               choiceNames = c("All",
                                                               "Urban",
                                                               "Rural"),
                                               choiceValues = c(1, 2, 3)),
                                  footer = urban_rural_caption
                                )),
                                fluidRow(
                                  br()
                                ),
                                fluidRow(
                                box(width = 6,
                                  withMathJax(),
                                  radioButtons("LevelSelection_Decomp_91",  strong("\n S elect Poverty Index to Examine"), 
                                               choiceNames = c("Adj. Headcount Ratio \\(M_{0}\\)",
                                                               "Adj. Poverty Gap \\(M_{1}\\)",
                                                               "Adj. Poverty Severity \\(M_{2}\\)"),
                                               choiceValues = c(1, 2, 3)),
                                  footer = level_caption
                                ),
                                
                                box(width = 6,
                                  withMathJax(),
                                  radioButtons("c_g_Decomp_91",  strong("\n Select Method to Examine the Component "), 
                                               choiceNames = c("Percent Contribution to MPI",
                                                               "Unidimensional Poverty Indices "),
                                               choiceValues = c(1, 2)),
                                  footer = c_g_caption
                                )),
                                fluidRow(
                                  br()
                                ),
                                box(
                                  withMathJax(),
                                  title = strong("Descriptive Analysis"),
                                  width = 12,
                                 
                                  p("The percent contribution and poverty gap measures are directly dependent on the k-threshold that is specified. When we increase the threshold of k, we are effectively increasing the criteria to be labeled multidimensionally poor. A trend that we notice is that households/regions that are considered multidimensionally poor when the k-threshold is set high are more heavily concentrated in urban areas like Bulawayo and Harare. Therefore, when we look at the \\(M_{0}\\) scores for high k values, we view an index score skewed towards urban households. When the k value is low, fewer households will have been ruled out as multidimensionally poor, so more of the initial population (composed largely of rural households) will influence the \\(M_{0}\\) score."),
                                  p("In our interface, selecting the adjusted-headcount ratio (\\(M_{0}\\)) and focusing first on the percent contribution of the individual variables, we can see that the three most significant contributors to the \\(M_{0}\\) index for most k-threshold values are the Lack of Household Assets, Lack of Access to Services, & Chronic Illness variables. As we increase the k-threshold, we can see that the Lack of Health Visit  , which was not as prevalent, to begin with, becomes much more so as the criteria to be labeled multidimensionally poor increase. This is because this variable is more relevant to the urban poor, which is why it does not display as contributing to the \\(M_{0}\\) scores significantly, to begin with. The opposite could be said about something like the Poor Cooking Fuel variable, which is prevalent in our \\(M_{0}\\) scores, to begin with, but less so as we increase the k-threshold, and several rural households start to be excluded as being multidimensionally poor."),
                                  p("For each variable, we can see that the unidimensional index is the greatest for Lack of Household Assets, Lack of Access to Services, & Chronic Illness. These variables are contributing the most to the \\(M_{0}\\) scores for most k-thresholds. The index for Lack of Health Visit, which contributes more to the \\(M_{0}\\) score for higher values of k, has more striking poverty gaps as k reaches its maximum of 9. Poor Cooking Fuel, which contributes more to our \\(M_{0}\\) scores at low values of k, has more striking poverty gaps at low k thresholds. Given the Alkire-Foster method that assigns weight to each component of poverty the variables with the highest unidimensional indices contribute the most to the \\(M_{0}\\) score for a given k-threshold."),
                                  p("The \\(M_{1}\\) and \\(M_{2}\\) measures exhibit very similar trends in terms of their components as discussed for \\(M_{0}\\). These can be explored as well.  "),
                                  p(""))
                              )),
                            
                            tabPanel("60 District MPI Map",
                                     # Everything has to be put in a row or column
                                     fluidPage(
                                       fluidRow(
                                       box(
                                         title = "60 District Decomposition Map of Zimbabwe",
                                         withSpinner(leafletOutput("Dist_60_Decomp_Map",height = 520)),
                                         p(strong("Note: The map always resets to Max Education.")),
                                         width = 8,
                                         height = 600
                                       ),
                                       box(
                                         withMathJax(),
                                         title = "Description",
                                         p("This page presents the unidimensional indices for the individual components that make up the MPI.  The dropdown menu allows the user to display the unidimensional index (i.e., the health, education, or asset component) or to display the component’s contribution to the multidimensional index. This can be shown at the district or province level.   The user can display these values for rural or urban areas and with different k-threshold values.  "), 
                                         
                                         p("Note: for our district-level analysis, a grey-filled area with an NA means that no districts fulfill the criteria chosen. For example, this would apply if there are high k-thresholds and no households exist at this level, or for the regional analysis if there are no urban or rural households.These results are presented for the incidence (\\(M_{0}\\)), gap (\\(M_{1}\\)), and severity of poverty (\\(M_{2}\\)). "),
                                         width = 4
                                       )),
                                       fluidRow(
                                         br()
                                       ),
                                       fluidRow(
                                       box(width = 6,
                                         sliderInput("slider_60_Decomp", strong("k-Threshold Value"), 1, 9, 3),
                                         footer = slider_caption
                                       ),
                                       box(width = 6,
                                         radioButtons("UrbRurSelection_Decomp_60", strong("Select Urban/Rural Filter"), 
                                                      choiceNames = c("All",
                                                                      "Urban",
                                                                      "Rural"),
                                                      choiceValues = c(1, 2, 3)),
                                         footer = urban_rural_caption
                                       )),
                                       fluidRow(
                                         br(),
                                         br()
                                       ),
                                       box(width = 6,
                                         withMathJax(),
                                         radioButtons("LevelSelection_Decomp_60", strong("Select Poverty Index to Examine"), 
                                                      choiceNames = c("Adj. Headcount Ratio \\(M_{0}\\)",
                                                                      "Adj. Poverty Gap \\(M_{1}\\)",
                                                                      "Adj. Poverty Severity \\(M_{2}\\)"),
                                                      choiceValues = c(1, 2, 3)),
                                         footer = level_caption
                                       ),
                                       box(width = 6,
                                         withMathJax(),
                                         radioButtons("c_g_Decomp_60", strong("Select Method to Examine the Component"), 
                                                      choiceNames = c("Percent Contribution to MPI",
                                                                      "Unidimensional Poverty Indices "),
                                                      choiceValues = c(1, 2)),
                                         footer = c_g_caption
                                       ),
                                       
                                       fluidRow(
                                         br(),
                                         br()
                                       ),
                                       box(
                                         withMathJax(),
                                         title = strong("Descriptive Analysis"),
                                         width = 12,
                                         
                                         p("The percent contribution and poverty gap measures are directly dependent on the k-threshold that is specified. When we increase the threshold of k, we are effectively increasing the criteria to be labeled multidimensionally poor. A trend that we notice is that households/regions that are considered multidimensionally poor when the k-threshold is set high are more heavily concentrated in urban areas like Bulawayo and Harare. Therefore, when we look at the \\(M_{0}\\) scores for high k values, we view an index score skewed towards urban households. When the k value is low, fewer households will have been ruled out as multidimensionally poor, so more of the initial population (composed largely of rural households) will influence the \\(M_{0}\\) score."),
                                         p("In our interface, selecting the adjusted-headcount ratio (\\(M_{0}\\)) and focusing first on the percent contribution of the individual variables, we can see that the three most significant contributors to the \\(M_{0}\\) index for most k-threshold values are the Lack of Household Assets, Lack of Access to Services, & Chronic Illness variables. As we increase the k-threshold, we can see that the Lack of Health Visit  , which was not as prevalent, to begin with, becomes much more so as the criteria to be labeled multidimensionally poor increase. This is because this variable is more relevant to the urban poor, which is why it does not display as contributing to the \\(M_{0}\\) scores significantly, to begin with. The opposite could be said about something like the Poor Cooking Fuel variable, which is prevalent in our \\(M_{0}\\) scores, to begin with, but less so as we increase the k-threshold, and several rural households start to be excluded as being multidimensionally poor."),
                                         p("For each variable, we can see that the unidimensional index is the greatest for Lack of Household Assets, Lack of Access to Services, & Chronic Illness. These variables are contributing the most to the \\(M_{0}\\) scores for most k-thresholds. The index for Lack of Health Visit, which contributes more to the \\(M_{0}\\) score for higher values of k, has more striking poverty gaps as k reaches its maximum of 9. Poor Cooking Fuel, which contributes more to our \\(M_{0}\\) scores at low values of k, has more striking poverty gaps at low k thresholds. Given the Alkire-Foster method that assigns weight to each component of poverty the variables with the highest unidimensional indices contribute the most to the \\(M_{0}\\) score for a given k-threshold."),
                                         
                                         p(""))
                                     )),
                            tabPanel(
                              "Province MPI Map",
                              # Everything has to be put in a row or column
                              fluidPage(
                                box(
                                  title = "Province Decomposition Map of Zimbabwe",
                                  withSpinner(leafletOutput("Prov_Decomp_Map",height = 520)),
                                  p(strong("Note: The map always resets to Max Education.")),
                                  width = 8,
                                  height = 600
                                ),
                                box(
                                  withMathJax(),
                                  title = "Description",
                                  p("This page presents the unidimensional indices for the individual components that make up the MPI.  The dropdown menu allows the user to display the unidimensional index (i.e., the health, education, or asset component) or to display the component’s contribution to the multidimensional index. This can be shown at the district or province level.   The user can display these values for rural or urban areas and with different k-threshold values.  "), 
                                  
                                  p("Note: for our district-level analysis, a grey-filled area with an NA means that no districts fulfill the criteria chosen. For example, this would apply if there are high k-thresholds and no households exist at this level, or for the regional analysis if there are no urban or rural households.These results are presented for the incidence (\\(M_{0}\\)), gap (\\(M_{1}\\)), and severity of poverty (\\(M_{2}\\)). "),
                                  width = 4
                                ),
                                fluidRow(
                                  br(),
                                
                                
                                box(width = 6,
                                  sliderInput("slider_Prov_Decomp", strong("k-Threshold Value"), 1, 9, 3),
                                  footer = slider_caption
                                ),
                                box(width = 6,
                                  radioButtons("UrbRurSelection_Decomp_Prov", strong("Select Urban/Rural Filter"), 
                                               choiceNames = c("All",
                                                               "Urban",
                                                               "Rural"),
                                               choiceValues = c(1, 2, 3)),
                                  footer = urban_rural_caption
                                )
                                ),
                                fluidRow(
                                  br(),
                                  br(),
                                
                                box(width = 6,
                                  withMathJax(),
                                  radioButtons("LevelSelection_Decomp_Prov", strong("Select Poverty Index to Examine"), 
                                               choiceNames = c("Adj. Headcount Ratio \\(M_{0}\\)",
                                                               "Adj. Poverty Gap \\(M_{1}\\)",
                                                               "Adj. Poverty Severity \\(M_{2}\\)"),
                                               choiceValues = c(1, 2, 3)),
                                  footer = level_caption
                                ),
                                box(width = 6,
                                  withMathJax(),
                                  radioButtons("c_g_Decomp_Prov", strong("Select Method to Examine the Component"), 
                                               choiceNames = c("Percent Contribution to MPI",
                                                               "Unidimensional Poverty Indices "),
                                               choiceValues = c(1, 2)),
                                  footer = c_g_caption
                                )),
                                fluidRow(
                                  br()
                                ),
                                box(
                                  withMathJax(),
                                  title = strong("Descriptive Analysis"),
                                  width = 12,
                                 
                                  p("The percent contribution and poverty gap measures are directly dependent on the k-threshold that is specified. When we increase the threshold of k, we are effectively increasing the criteria to be labeled multidimensionally poor. A trend that we notice is that households/regions that are considered multidimensionally poor when the k-threshold is set high are more heavily concentrated in urban areas like Bulawayo and Harare. Therefore, when we look at the \\(M_{0}\\) scores for high k values, we view an index score skewed towards urban households. When the k value is low, fewer households will have been ruled out as multidimensionally poor, so more of the initial population (composed largely of rural households) will influence the \\(M_{0}\\) score."),
                                  p("In our interface, selecting the adjusted-headcount ratio (\\(M_{0}\\)) and focusing first on the percent contribution of the individual variables, we can see that the three most significant contributors to the \\(M_{0}\\) index for most k-threshold values are the Lack of Household Assets, Lack of Access to Services, & Chronic Illness variables. As we increase the k-threshold, we can see that the Lack of Health Visit  , which was not as prevalent, to begin with, becomes much more so as the criteria to be labeled multidimensionally poor increase. This is because this variable is more relevant to the urban poor, which is why it does not display as contributing to the \\(M_{0}\\) scores significantly, to begin with. The opposite could be said about something like the Poor Cooking Fuel variable, which is prevalent in our \\(M_{0}\\) scores, to begin with, but less so as we increase the k-threshold, and several rural households start to be excluded as being multidimensionally poor."),
                                  p("For each variable, we can see that the unidimensional index is the greatest for Lack of Household Assets, Lack of Access to Services, & Chronic Illness. These variables are contributing the most to the \\(M_{0}\\) scores for most k-thresholds. The index for Lack of Health Visit, which contributes more to the \\(M_{0}\\) score for higher values of k, has more striking poverty gaps as k reaches its maximum of 9. Poor Cooking Fuel, which contributes more to our \\(M_{0}\\) scores at low values of k, has more striking poverty gaps at low k thresholds. Given the Alkire-Foster method that assigns weight to each component of poverty the variables with the highest unidimensional indices contribute the most to the \\(M_{0}\\) score for a given k-threshold."),
                                   p()
                                  )
                              ))
                 ),
                 ## Tab Temporal Comparison--------------------------------------
                 tabPanel("MPI: From 2011 to 2017", 
                          
                          # dashboardPage(
                          #   skin = 'black',
                          #   dashboardHeader(
                          #     title = 'MPI: from 2011 to 2017'
                          #   ),
                          #   dashboardSidebar(
                          #     sidebarMenu(
                          #       menuItem(
                          #         "\\(M_0 \\) Comparison Map",
                          #         tabName = 'M0_Comp',
                          #         selected = TRUE
                          #       ),
                          #       menuItem(
                          #         "\\(M_1 \\) Comparison",
                          #         tabName = "M1_Comp"
                          #       ),
                          #       menuItem(
                          #         "\\(M_2 \\) Comparison Map",
                          #         tabName = 'M2_Comp'
                          #       )
                          #     )
                          #   ),
                          # dashboardBody(
                          
                          tabsetPanel(
                            tabPanel("\\(M_0 \\) Comparison Map",
                                     # tabName = "M0_Comp",
                                     # Everything has to be put in a row or column
                                     fluidPage(
                                       box(
                                         title = "Comparison of \\(M_0 \\) in Zimbabwe",
                                         withSpinner(leafletOutput("M0_Comparison_Map",height = 520)),
                                         width = 6,
                                         height = 600
                                       ),
                                       box(
                                         withMathJax(),
                                         title = "Comparison of \\(M_0 \\) for 2011 and 2017",
                                         #withSpinner(plotlyOutput("M0_Scatterplot"))
                                         withSpinner(plotlyOutput("M0_Scatterplot",height = 520)),
                                         width = 6,
                                         height = 600
                                       ),
                                       fluidRow(
                                         br()
                                       ),
                                       box(width = 4,
                                         sliderInput("slider_M0_Comparison", strong("k-Threshold Value"), 1, 9, 3),
                                         footer = slider_caption
                                       ),
                                       box(width = 4,
                                         radioButtons("UrbRurSelection_M0", strong("Select Urban/Rural Filter"), 
                                                      choiceNames = c("All",
                                                                      "Urban",
                                                                      "Rural"),
                                                      choiceValues = c(1, 2, 3)),
                                         footer = urban_rural_caption
                                       ),
                                       box(width = 4,
                                         radioButtons("RegionSelection_M0", strong("Select Index Level to Display"), 
                                                      choiceNames = c("Districts",
                                                                      "Provinces"),
                                                      choiceValues = c(1, 2)),
                                         footer = urban_rural_caption
                                       ),
                                       fluidRow(
                                         br()
                                       ),
                                       box(
                                         withMathJax(),
                                         title = strong("Descriptive Analysis"),
                                         width = 12,
                                         
                                        p("The above maps display the change in the \\(M_{0}\\) index from 2011 to 2017. Positive values represent increases in the adjusted headcount of the multidimensionally-poor for a given province/district. An alternative way to compare the same district across time is to use a scatterplot displayed in the graph beside the map. The x-axis shows the province/district’s \\(M_{0}\\) score for 2011, whereas the y-axis shows the \\(M_{0}\\) score of the same province/district for 2017. Should the province/district appear above the 45-degree line, this implies that the province/district has an increased \\(M_{0}\\) score, implying that the headcount of the multidimensionally-poor has increased. Conversely, if the province/district appears below the 45-degree line, this implies that the province/district has a decreased \\(M_{0}\\) score, implying that the headcount of multidimensionally-poor individuals has decreased. Finally, being on the 45-degree line means that there has been no change in the headcount of multidimensionally-poor from 2011 to 2017."),
                                        p("At the district level, we can see that the majority of districts fall above the 45-degree line, implying a greater number of multidimensionally-poor households in most districts in 2017 compared to 2011. As we increase the k value, more districts fall below that 45-degree line. In other words, as the criteria to be labeledthreshold of multidimensionally poor poverty increases, more districts will not have increased inappear to have reduced their headcount of multidimensional poverty. The majority ofMost districts remain over the 45-degree line for low k-thresholds. Many remain over the 45-degree line for high k-thresholds, which tells us that multidimensional poverty has increased in the country, on average, over the specified 6-year period. This overall trend is applicable for urban and rural households, which we can visualize by clicking the respective button in the filter section. "),
                                        p("If we instead focus on the MPI for provinces, we can see that the same trend exists. With low k-values, all of the provinces find themselves having increased their \\(M_{0}\\) scores. As the k-threshold increases, a few of the provinces start to fall under the 45-degree line. This is consistent when looking uniquely at either urban or rural households as well. It is worth noting that Bulawayo and Harare remain above the 45-degree line for all k-values, suggesting that multidimensional poverty has increased for the entire population of those provinces/districts. "),
                                        p("Similar patterns are observed for the \\(M_{1}\\) and \\(M_{2}\\) measures and can be explored through the dashboard presented here."),
                                       
                                       ),
                                       p("")
                                     )
                            ),
                            tabPanel("\\(M_1 \\) Comparison Map",
                                     # Everything has to be put in a row or column
                                     fluidPage(
                                       box(
                                         title = "Comparison of \\(M_1 \\) in Zimbabwe",
                                         withSpinner(leafletOutput("M1_Comparison_Map",height = 520)),
                                         width = 6,
                                         height = 600
                                       ),
                                       box(
                                         withMathJax(),
                                         title = "Comparison of \\(M_1 \\) for 2011 and 2017",
                                         withSpinner(plotlyOutput("M1_Scatterplot",height = 520)),
                                         width = 6,
                                         height = 600
                                       ),
                                       fluidRow(
                                         br()
                                       ),
                                       box(width = 4,
                                         sliderInput("slider_M1_Comparison", strong("k-Threshold Value"), 1, 9, 3),
                                         footer = slider_caption
                                       ),
                                       box(width=4,
                                         radioButtons("UrbRurSelection_M1", strong("Select Urban/Rural Filter"), 
                                                      choiceNames = c("All",
                                                                      "Urban",
                                                                      "Rural"),
                                                      choiceValues = c(1, 2, 3)),
                                         footer = urban_rural_caption
                                       ),
                                       box(width=4,
                                         radioButtons("RegionSelection_M1", strong("Select Index Level to Display"), 
                                                      choiceNames = c("Districts",
                                                                      "Provinces"),
                                                      choiceValues = c(1, 2)),
                                         footer = urban_rural_caption
                                       ),
                                       fluidRow(
                                         br()
                                       ),
                                       box(
                                         withMathJax(),
                                         title = strong("Descriptive Analysis"),
                                         width = 12,
                                         p("The above maps display the change in the \\(M_{0}\\) index from 2011 to 2017. Positive values represent increases in the adjusted headcount of the multidimensionally-poor for a given province/district. An alternative way to compare the same district across time is to use a scatterplot displayed in the graph beside the map. The x-axis shows the province/district’s \\(M_{0}\\) score for 2011, whereas the y-axis shows the \\(M_{0}\\) score of the same province/district for 2017. Should the province/district appear above the 45-degree line, this implies that the province/district has an increased \\(M_{0}\\) score, implying that the headcount of the multidimensionally-poor has increased. Conversely, if the province/district appears below the 45-degree line, this implies that the province/district has a decreased \\(M_{0}\\) score, implying that the headcount of multidimensionally-poor individuals has decreased. "),
                                         p("At the district level, we can see that the majority of districts fall above the 45-degree line, implying a greater number of multidimensionally-poor households in most districts in 2017 compared to 2011. As we increase the k value, more districts fall below that 45-degree line. In other words, as the criteria to be labeled multidimensionally poor increases, more districts will not have increased in their headcount of multidimensional poverty. The majority of districts remain over the line for low k-thresholds. Many remain over the line for high k-thresholds, which tells us that multidimensional poverty has increased in the country, on average, over the specified 6-year period. This overall trend is applicable for urban and rural households, which we can visualize by clicking the respective button in the filter section. "),
                                         p("If we instead focus on the MPI for provinces, we can see that the same trend exists. With low k-values, all of the provinces find themselves having increased in their \\(M_{0}\\) scores. As the k-threshold increases, a few of the provinces start to fall under the 45-degree line. This is consistent when looking uniquely at either urban or rural households as well. It is worth noting that Bulawayo and Harare remain above the 45-degree line for all k-values, suggesting that multidimensional poverty has increased for the entire population of those provinces/districts. "),
                                         p("Similar patterns are observed for the \\(M_{1}\\) and \\(M_{2}\\) measures and can be explored through the dashboard presented here."),
                                        
                                           p(""),
                                         p("")
                                       ),
                                       p("")
                                     )
                            ),
                            tabPanel(  "\\(M_2 \\) Comparison Map",
                                       # Everything has to be put in a row or column
                                       fluidPage(
                                         box(
                                           title = "Comparison of \\(M_2 \\) in Zimbabwe",
                                           withSpinner(leafletOutput("M2_Comparison_Map",height = 520)),
                                           width = 6,
                                           height = 600
                                         ),
                                         box(
                                           withMathJax(),
                                           title = "Comparison of \\(M_2 \\) for 2011 and 2017",
                                           withSpinner(plotlyOutput("M2_Scatterplot",height = 520)),
                                           width = 6,
                                           height = 600
                                         ),
                                         fluidRow(
                                           br()
                                         ),
                                         box(width=4,
                                           sliderInput("slider_M2_Comparison", strong("k-Threshold Value"), 1, 9, 3),
                                           footer = slider_caption
                                         ),
                                         box(width=4,
                                           radioButtons("UrbRurSelection_M2", strong("Select Urban/Rural Filter"), 
                                                        choiceNames = c("All",
                                                                        "Urban",
                                                                        "Rural"),
                                                        choiceValues = c(1, 2, 3)),
                                           footer = urban_rural_caption
                                         ),
                                         box(width=4,
                                           radioButtons("RegionSelection_M2", strong("Select Index Level to Display"), 
                                                        choiceNames = c("Districts",
                                                                        "Provinces"),
                                                        choiceValues = c(1, 2)),
                                           footer = urban_rural_caption
                                         ),
                                         fluidRow(
                                           br()
                                         ),
                                         box(
                                           withMathJax(),
                                           title = strong("Descriptive Analysis"),
                                           width = 12,
                                           p("The above maps display the change in the \\(M_{0}\\) index from 2011 to 2017. Positive values represent increases in the adjusted headcount of the multidimensionally-poor for a given province/district. An alternative way to compare the same district across time is to use a scatterplot displayed in the graph beside the map. The x-axis shows the province/district’s \\(M_{0}\\) score for 2011, whereas the y-axis shows the \\(M_{0}\\) score of the same province/district for 2017. Should the province/district appear above the 45-degree line, this implies that the province/district has an increased \\(M_{0}\\) score, implying that the headcount of the multidimensionally-poor has increased. Conversely, if the province/district appears below the 45-degree line, this implies that the province/district has a decreased \\(M_{0}\\) score, implying that the headcount of multidimensionally-poor individuals has decreased. "),
                                           p("At the district level, we can see that the majority of districts fall above the 45-degree line, implying a greater number of multidimensionally-poor households in most districts in 2017 compared to 2011. As we increase the k value, more districts fall below that 45-degree line. In other words, as the criteria to be labeled multidimensionally poor increases, more districts will not have increased in their headcount of multidimensional poverty. The majority of districts remain over the line for low k-thresholds. Many remain over the line for high k-thresholds, which tells us that multidimensional poverty has increased in the country, on average, over the specified 6-year period. This overall trend is applicable for urban and rural households, which we can visualize by clicking the respective button in the filter section. "),
                                           p("If we instead focus on the MPI for provinces, we can see that the same trend exists. With low k-values, all of the provinces find themselves having increased in their \\(M_{0}\\) scores. As the k-threshold increases, a few of the provinces start to fall under the 45-degree line. This is consistent when looking uniquely at either urban or rural households as well. It is worth noting that Bulawayo and Harare remain above the 45-degree line for all k-values, suggesting that multidimensional poverty has increased for the entire population of those provinces/districts. "),
                                           p("Similar patterns are observed for the \\(M_{1}\\) and \\(M_{2}\\) measures and can be explored through the dashboard presented here."),
                                           
                                             p("")),
                                         p("")
                                       )
                            )
                          )
                 ),
                 
                 ## Takeaways------------------
                 tabPanel("Takeaways",
                    
                          fluidPage(style = "margin-left: 100px; margin-right: 100px;",
                            h1(strong("Takeaways"),align="center"),
                            p("The analysis presented here provides an interactive way to decompose a multidimensional poverty index along many dimensions. We display the MPIs in maps and rankings, allowing users to assess multidimensional poverty by region (10 provinces/60 districts/91 districts), by population (all/urban/rural households), and by their preferred definition of poverty (k-threshold). We allow users to decompose the MPIs into their 14 components, allowing users to look at the unidimensional poverty indices of the individual components and their contribution to the multidimensional indices. Finally, we offer users the ability to view the change in multidimensional poverty between the two most recent waves of PICES surveys (2011-2017). "),
                            p("From our analyses, we see that when defining multidimensional poverty according to low thresholds, major urban areas such as Bulawayo and Harare are, on average, better off than their rural counterparts. Despite this, as we increase the threshold of multidimensional poverty, we see that these urban areas host the most vulnerable populations. We notice that specific components contribute more extensively to the national indices (i.e., Chronic Illness, Lack of Access to Services, Lack of Household Assets), whereas others contribute more to urban households (Lack of Health Visit) or rural households (Poor Cooking Fuel). Our sensitivity check shows that by setting a higher poverty line for a key component like Max Education, thereby considering more households deprived in that dimension, multidimensional poverty increases accordingly. Finally, the multidimensional poverty indices constructed here show that poverty has increased across most districts in Zimbabwe from 2011 to 2017. "),
                            p("We note that trends within our analyses are not unanimous. Individual districts do not necessarily follow national trends. The interactive dashboard presented here, providing users the ability to disaggregate the analysis from the province to the district level, allows for a more refined assessment of the trends in the individual districts."),
                            p(""),
                          )
                                
                 ),
                 
                 
                 
                 
                 ## Tab DSPG Team------------------------------------------------
                 tabPanel("Our Team", 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h1(strong("Team"), align = "center"),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good"), align = "center"),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural and Applied Economics'), 
                                     "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges 
                                               around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to 
                                               determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, 
                                               how to apply, and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "team-yang.png", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-sambath.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-atticus.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-matt.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("", style = "padding-top:10px;"),
                                          p(a(href = 'https://www.linkedin.com/in/yang-cheng-200118191/', 'Yang Cheng', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = 'https://www.linkedin.com/in/sambath-jayapregasham-097803127/', 'Sambath Jayapregasham', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = 'https://www.linkedin.com/in/atticus-rex-717581191/', 'Atticus Rex', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics);"),
                                          p( a(href = 'https://www.linkedin.com/in/matthew-burkholder-297b9119a/', 'Matthew Burkholder', target = '_blank'), "(Virginia Tech, Philosophy, Politics, & Economics)."),
                                          p("", style = "padding-top:10px;")
                                          
                                   ),
                                   column(6, align = "center",
                                          h4(strong("Virginia Tech Faculty Members")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "faculty-chen.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-gupta.jpg", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-alwang.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          p("", style = "padding-top:10px;"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/chen-susan.html", 'Dr. Susan Chen', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/gupta-anubhab.html", 'Dr. Anubhab Gupta', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/alwang-jeffrey.html", 'Dr. Jeffrey Alwang', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics)."),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Project Stakeholders"), align = "center"),
                                   p(a(href="https://www.linkedin.com/in/dhiraj-sharma-aa029024/?originalSubdomain=np","Dhiraj Sharma",target='_blank')," (World Bank); "),
                                   p("Grown Chirongwe",a(href="https://www.zimstat.co.zw/","(Zimbabwe National Statistics Agency)",target="_blank")),
                                   
                                   p(em("Disclaim: "),("This project is an academic exercise conducted by VT-Data Science for the Public Good. The findings, interpretations, and conclusions expressed here do not necessarily reflect the views of the World Bank or the Zimbabwe Statistical Agency."))
                                   
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Acknowledgement"), align = "center"),
                                   p("We would like to thank ",a(href="https://www.linkedin.com/in/quentin-stoeffler-7913a035/?originalSubdomain=tr","Dr. Quentin Stoeffler",target='_blank')," for providing us with code of the paper", a(href="https://www.researchgate.net/profile/Jeffrey-Alwang/publication/283241726_Multidimensional_Poverty_in_Crisis_Lessons_from_Zimbabwe/links/56b8978a08ae44bb330d32f2/Multidimensional-Poverty-in-Crisis-Lessons-from-Zimbabwe.pdf","Multidimensional Poverty in Crisis: Lessons from Zimbabwe",target='_blank'),". We also thank ZimStat for providing 2011 and 2017 PICES data for this project.")
                                   
                          )
                 ),
                 inverse = T)
                 

                 




# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  output$allgrctable <- renderTable({
    table <- read.csv("data/isochrones/tables/grc_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 3)
  
  
  output$Dist_91_MPI_Map <- renderLeaflet({
    k_threshold = input$slider_91_MPI
    UrbRurSelection = strtoi(input$UrbRurSelection_MPI_91)
    SensitivitySelection = strtoi(input$SensitivitySelection_91)
    
    map = switch(SensitivitySelection, switch(UrbRurSelection,
                                              MAP_2017_91_T_o,
                                              MAP_2017_91_U_o,
                                              MAP_2017_91_R_o), switch(UrbRurSelection,
                                                                       MAP_2017_91_T_n,
                                                                       MAP_2017_91_U_n,
                                                                       MAP_2017_91_R_n))
    
    M0 = switch(k_threshold,
                map@data$M0_k1,
                map@data$M0_k2,
                map@data$M0_k3,
                map@data$M0_k4,
                map@data$M0_k5,
                map@data$M0_k6,
                map@data$M0_k7,
                map@data$M0_k8,
                map@data$M0_k9)
    
    M1 = switch(k_threshold,
                map@data$M1_k1,
                map@data$M1_k2,
                map@data$M1_k3,
                map@data$M1_k4,
                map@data$M1_k5,
                map@data$M1_k6,
                map@data$M1_k7,
                map@data$M1_k8,
                map@data$M1_k9)
    
    M2 = switch(k_threshold,
                map@data$M2_k1,
                map@data$M2_k2,
                map@data$M2_k3,
                map@data$M2_k4,
                map@data$M2_k5,
                map@data$M2_k6,
                map@data$M2_k7,
                map@data$M2_k8,
                map@data$M2_k9)
    
    # This is the color palette used in the graphs
    pal <- colorNumeric(
      palette = "viridis",
      domain = c(0, max(M0, na.rm = TRUE)),
      reverse = TRUE)
    
    # This creates labels for M0, M1 and M2 
    M0_labels <- get_label(MAP_2017_91_T_o@data$ADM2_EN, "M<sub>0</sub>", M0, switch(input$slider_91_MPI,
                                                                                     MPI_2017_1_T_o$M0_k1[1],
                                                                                     MPI_2017_1_T_o$M0_k2[1],
                                                                                     MPI_2017_1_T_o$M0_k3[1],
                                                                                     MPI_2017_1_T_o$M0_k4[1],
                                                                                     MPI_2017_1_T_o$M0_k5[1],
                                                                                     MPI_2017_1_T_o$M0_k6[1],
                                                                                     MPI_2017_1_T_o$M0_k7[1],
                                                                                     MPI_2017_1_T_o$M0_k8[1],
                                                                                     MPI_2017_1_T_o$M0_k9[1]))
    
    M1_labels <- get_label(MAP_2017_91_T_o@data$ADM2_EN, "M<sub>1</sub>", M1, switch(input$slider_91_MPI,
                                                                                     MPI_2017_1_T_o$M1_k1[1],
                                                                                     MPI_2017_1_T_o$M1_k2[1],
                                                                                     MPI_2017_1_T_o$M1_k3[1],
                                                                                     MPI_2017_1_T_o$M1_k4[1],
                                                                                     MPI_2017_1_T_o$M1_k5[1],
                                                                                     MPI_2017_1_T_o$M1_k6[1],
                                                                                     MPI_2017_1_T_o$M1_k7[1],
                                                                                     MPI_2017_1_T_o$M1_k8[1],
                                                                                     MPI_2017_1_T_o$M1_k9[1]))
    
    M2_labels <- get_label(MAP_2017_91_T_o@data$ADM2_EN, "M<sub>2</sub>", M2, switch(input$slider_91_MPI,
                                                                                     MPI_2017_1_T_o$M2_k1[1],
                                                                                     MPI_2017_1_T_o$M2_k2[1],
                                                                                     MPI_2017_1_T_o$M2_k3[1],
                                                                                     MPI_2017_1_T_o$M2_k4[1],
                                                                                     MPI_2017_1_T_o$M2_k5[1],
                                                                                     MPI_2017_1_T_o$M2_k6[1],
                                                                                     MPI_2017_1_T_o$M2_k7[1],
                                                                                     MPI_2017_1_T_o$M2_k8[1],
                                                                                     MPI_2017_1_T_o$M2_k9[1]))
    
    ## MAPPING MPI 2017 91districs----------------------------------------------------------------------
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
      get_polygon(MAP_2017_91_T_o, pal, M0, M0_labels, "M0") %>%
      get_polygon(MAP_2017_91_T_o, pal, M1, M1_labels, "M1") %>%
      get_polygon(MAP_2017_91_T_o, pal, M2, M2_labels, "M2") %>%
      clearControls() %>%
      addLayersControl(
        baseGroups = c("M0", "M1", "M2"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend(pal = pal, values = c(0, max(M0, na.rm = TRUE)), opacity = 0.7, title = paste0("k = ", input$slider_91_MPI),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  output$Dist_60_MPI_Map <- renderLeaflet({
    k_threshold = input$slider_60_MPI
    UrbRurSelection = strtoi(input$UrbRurSelection_MPI_60)
    SensitivitySelection = strtoi(input$SensitivitySelection_60)
    
    
    
    map = switch(SensitivitySelection, switch(UrbRurSelection,
                                              MAP_2017_60_T_o,
                                              MAP_2017_60_U_o,
                                              MAP_2017_60_R_o), switch(UrbRurSelection,
                                                                       MAP_2017_60_T_n,
                                                                       MAP_2017_60_U_n,
                                                                       MAP_2017_60_R_n))
    
    M0 = switch(k_threshold,
                map@data$M0_k1,
                map@data$M0_k2,
                map@data$M0_k3,
                map@data$M0_k4,
                map@data$M0_k5,
                map@data$M0_k6,
                map@data$M0_k7,
                map@data$M0_k8,
                map@data$M0_k9)
    
    M1 = switch(k_threshold,
                map@data$M1_k1,
                map@data$M1_k2,
                map@data$M1_k3,
                map@data$M1_k4,
                map@data$M1_k5,
                map@data$M1_k6,
                map@data$M1_k7,
                map@data$M1_k8,
                map@data$M1_k9)
    
    M2 = switch(k_threshold,
                map@data$M2_k1,
                map@data$M2_k2,
                map@data$M2_k3,
                map@data$M2_k4,
                map@data$M2_k5,
                map@data$M2_k6,
                map@data$M2_k7,
                map@data$M2_k8,
                map@data$M2_k9)
    
    
    # This is the color palette used in the graphs
    pal <- colorNumeric(
      palette = "viridis",
      domain = c(0, max(M0, na.rm = TRUE)),
      reverse = TRUE)
    
    # This creates labels for M0, M1 and M2 
    M0_labels <- get_label(MAP_2017_60_T_o@data$NAME_2, "M<sub>0</sub>", M0, switch(input$slider_60_MPI,
                                                                                    MPI_2017_1_T_o$M0_k1[1],
                                                                                    MPI_2017_1_T_o$M0_k2[1],
                                                                                    MPI_2017_1_T_o$M0_k3[1],
                                                                                    MPI_2017_1_T_o$M0_k4[1],
                                                                                    MPI_2017_1_T_o$M0_k5[1],
                                                                                    MPI_2017_1_T_o$M0_k6[1],
                                                                                    MPI_2017_1_T_o$M0_k7[1],
                                                                                    MPI_2017_1_T_o$M0_k8[1],
                                                                                    MPI_2017_1_T_o$M0_k9[1]))
    
    M1_labels <- get_label(MAP_2017_60_T_o@data$NAME_2, "M<sub>1</sub>", M1, switch(input$slider_60_MPI,
                                                                                    MPI_2017_1_T_o$M1_k1[1],
                                                                                    MPI_2017_1_T_o$M1_k2[1],
                                                                                    MPI_2017_1_T_o$M1_k3[1],
                                                                                    MPI_2017_1_T_o$M1_k4[1],
                                                                                    MPI_2017_1_T_o$M1_k5[1],
                                                                                    MPI_2017_1_T_o$M1_k6[1],
                                                                                    MPI_2017_1_T_o$M1_k7[1],
                                                                                    MPI_2017_1_T_o$M1_k8[1],
                                                                                    MPI_2017_1_T_o$M1_k9[1]))
    
    M2_labels <- get_label(MAP_2017_60_T_o@data$NAME_2, "M<sub>2</sub>", M2, switch(input$slider_60_MPI,
                                                                                    MPI_2017_1_T_o$M2_k1[1],
                                                                                    MPI_2017_1_T_o$M2_k2[1],
                                                                                    MPI_2017_1_T_o$M2_k3[1],
                                                                                    MPI_2017_1_T_o$M2_k4[1],
                                                                                    MPI_2017_1_T_o$M2_k5[1],
                                                                                    MPI_2017_1_T_o$M2_k6[1],
                                                                                    MPI_2017_1_T_o$M2_k7[1],
                                                                                    MPI_2017_1_T_o$M2_k8[1],
                                                                                    MPI_2017_1_T_o$M2_k9[1]))
    
    ## MAPPING MPI 2017 60 districst----------------------------------------------------------------------
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
      get_polygon(MAP_2017_60_T_o, pal, M0, M0_labels, "M0") %>%
      get_polygon(MAP_2017_60_T_o, pal, M1, M1_labels, "M1") %>%
      get_polygon(MAP_2017_60_T_o, pal, M2, M2_labels, "M2") %>%
      clearControls() %>%
      addLayersControl(
        baseGroups = c("M0", "M1", "M2"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend(pal = pal, values = c(0, max(M0, na.rm = TRUE)), opacity = 0.7, title = paste0("k = ", input$slider_60_MPI),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  output$Prov_MPI_Map <- renderLeaflet({
    
    k_threshold = input$slider_10_MPI
    UrbRurSelection = strtoi(input$UrbRurSelection_MPI_10)
    SensitivitySelection = strtoi(input$SensitivitySelection_10)
    
    
    
    map = switch(SensitivitySelection, switch(UrbRurSelection,
                                              MAP_2017_10_T_o,
                                              MAP_2017_10_U_o,
                                              MAP_2017_10_R_o), switch(UrbRurSelection,
                                                                       MAP_2017_10_T_n,
                                                                       MAP_2017_10_U_n,
                                                                       MAP_2017_10_R_n))
    
    
    M0 = switch(k_threshold,
                map@data$M0_k1,
                map@data$M0_k2,
                map@data$M0_k3,
                map@data$M0_k4,
                map@data$M0_k5,
                map@data$M0_k6,
                map@data$M0_k7,
                map@data$M0_k8,
                map@data$M0_k9)
    
    M1 = switch(k_threshold,
                map@data$M1_k1,
                map@data$M1_k2,
                map@data$M1_k3,
                map@data$M1_k4,
                map@data$M1_k5,
                map@data$M1_k6,
                map@data$M1_k7,
                map@data$M1_k8,
                map@data$M1_k9)
    
    M2 = switch(k_threshold,
                map@data$M2_k1,
                map@data$M2_k2,
                map@data$M2_k3,
                map@data$M2_k4,
                map@data$M2_k5,
                map@data$M2_k6,
                map@data$M2_k7,
                map@data$M2_k8,
                map@data$M2_k9)
    
    # This is the color palette used in the graphs
    pal <- colorNumeric(
      palette = "viridis",
      domain = c(0, max(M0, na.rm = TRUE)),
      reverse = TRUE)
    
    
    # This creates labels for M0, M1 and M2 
    M0_labels <- get_label(MAP_2017_10_T_o@data$ADM1_EN, "M<sub>0</sub>", M0, switch(k_threshold,
                                                                                     MPI_2017_1_T_o$M0_k1[1],
                                                                                     MPI_2017_1_T_o$M0_k2[1],
                                                                                     MPI_2017_1_T_o$M0_k3[1],
                                                                                     MPI_2017_1_T_o$M0_k4[1],
                                                                                     MPI_2017_1_T_o$M0_k5[1],
                                                                                     MPI_2017_1_T_o$M0_k6[1],
                                                                                     MPI_2017_1_T_o$M0_k7[1],
                                                                                     MPI_2017_1_T_o$M0_k8[1],
                                                                                     MPI_2017_1_T_o$M0_k9[1]))
    
    M1_labels <- get_label(MAP_2017_10_T_o@data$ADM1_EN, "M<sub>1</sub>", M1, switch(k_threshold,
                                                                                     MPI_2017_1_T_o$M1_k1[1],
                                                                                     MPI_2017_1_T_o$M1_k2[1],
                                                                                     MPI_2017_1_T_o$M1_k3[1],
                                                                                     MPI_2017_1_T_o$M1_k4[1],
                                                                                     MPI_2017_1_T_o$M1_k5[1],
                                                                                     MPI_2017_1_T_o$M1_k6[1],
                                                                                     MPI_2017_1_T_o$M1_k7[1],
                                                                                     MPI_2017_1_T_o$M1_k8[1],
                                                                                     MPI_2017_1_T_o$M1_k9[1]))
    
    M2_labels <- get_label(MAP_2017_10_T_o@data$ADM1_EN, "M<sub>2</sub>", M2, switch(k_threshold,
                                                                                     MPI_2017_1_T_o$M2_k1[1],
                                                                                     MPI_2017_1_T_o$M2_k2[1],
                                                                                     MPI_2017_1_T_o$M2_k3[1],
                                                                                     MPI_2017_1_T_o$M2_k4[1],
                                                                                     MPI_2017_1_T_o$M2_k5[1],
                                                                                     MPI_2017_1_T_o$M2_k6[1],
                                                                                     MPI_2017_1_T_o$M2_k7[1],
                                                                                     MPI_2017_1_T_o$M2_k8[1],
                                                                                     MPI_2017_1_T_o$M2_k9[1]))
    
    
    # MAPPING MPI 2017 90 district ----------------------------------------------------------------------
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
      get_polygon(MAP_2017_10_T_o, pal, M0, M0_labels, "M0") %>%
      get_polygon(MAP_2017_10_T_o, pal, M1, M1_labels, "M1") %>%
      get_polygon(MAP_2017_10_T_o, pal, M2, M2_labels, "M2") %>%
      clearControls() %>%
      addLayersControl(
        baseGroups = c("M0", "M1", "M2"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addLegend(pal = pal, values = c(0, max(M0, na.rm = TRUE)), opacity = 0.7, title = paste0("k = ", input$slider_Prov_MPI),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  output$Dist_91_Decomp_Map <- renderLeaflet({
    # This is the level that the decomposition data is selected on. 
    # 1 = g0, c0, 2 = g1, c1, 3 = g2, c2
    level_selection = strtoi(input$LevelSelection_Decomp_91)
    
    # 1 = Total, 2 = Urban, 3 = Rural
    UrbRurSelection = strtoi(input$UrbRurSelection_Decomp_91)
    
    map = switch(UrbRurSelection,
                 MAP_2017_91_T_o,
                 MAP_2017_91_U_o,
                 MAP_2017_91_R_o)
    nat_data = switch(UrbRurSelection,
                      MPI_2017_1_T_o,
                      MPI_2017_1_U_o,
                      MPI_2017_1_R_o)
    
    
    g_edu_max = switch(input$slider_91_Decomp,
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
    g_edu_dropout = switch(input$slider_91_Decomp,
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
    
    g_hea_chronic = switch(input$slider_91_Decomp,
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
    
    g_hea_visit = switch(input$slider_91_Decomp,
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
    
    g_employment = switch(input$slider_91_Decomp,
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
    
    g_assets = switch(input$slider_91_Decomp,
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
    
    g_services = switch(input$slider_91_Decomp,
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
    
    g_electricity = switch(input$slider_91_Decomp,
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
    
    g_cooking_fuel = switch(input$slider_91_Decomp,
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
    
    g_water = switch(input$slider_91_Decomp,
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
    
    g_toilet = switch(input$slider_91_Decomp,
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
    
    g_land = switch(input$slider_91_Decomp,
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
    
    g_livestock = switch(input$slider_91_Decomp,
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
    
    g_rural_equip = switch(input$slider_91_Decomp,
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
    
    
    
    
    
    c_edu_max = switch(input$slider_91_Decomp,
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
    c_edu_dropout = switch(input$slider_91_Decomp,
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
    
    c_hea_chronic = switch(input$slider_91_Decomp,
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
    
    c_hea_visit = switch(input$slider_91_Decomp,
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
    
    c_employment = switch(input$slider_91_Decomp,
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
    
    c_assets = switch(input$slider_91_Decomp,
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
    
    c_services = switch(input$slider_91_Decomp,
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
    
    c_electricity = switch(input$slider_91_Decomp,
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
    
    c_cooking_fuel = switch(input$slider_91_Decomp,
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
    
    c_water = switch(input$slider_91_Decomp,
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
    
    c_toilet = switch(input$slider_91_Decomp,
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
    
    c_land = switch(input$slider_91_Decomp,
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
    
    c_livestock = switch(input$slider_91_Decomp,
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
    
    c_rural_equip = switch(input$slider_91_Decomp,
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
    
    
    
    n_c_edu_max = switch(input$slider_91_Decomp,
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
    n_c_edu_dropout = switch(input$slider_91_Decomp,
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
    
    n_c_hea_chronic = switch(input$slider_91_Decomp,
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
    
    n_c_hea_visit = switch(input$slider_91_Decomp,
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
    
    n_c_employment = switch(input$slider_91_Decomp,
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
    
    n_c_assets = switch(input$slider_91_Decomp,
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
    
    n_c_services = switch(input$slider_91_Decomp,
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
    
    n_c_electricity = switch(input$slider_91_Decomp,
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
    
    n_c_cooking_fuel = switch(input$slider_91_Decomp,
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
    
    n_c_water = switch(input$slider_91_Decomp,
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
    
    n_c_toilet = switch(input$slider_91_Decomp,
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
    
    n_c_land = switch(input$slider_91_Decomp,
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
    
    n_c_livestock = switch(input$slider_91_Decomp,
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
    
    n_c_rural_equip = switch(input$slider_91_Decomp,
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
    
    n_g_edu_max = switch(input$slider_91_Decomp,
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
    n_g_edu_dropout = switch(input$slider_91_Decomp,
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
    
    n_g_hea_chronic = switch(input$slider_91_Decomp,
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
    
    n_g_hea_visit = switch(input$slider_91_Decomp,
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
    
    n_g_employment = switch(input$slider_91_Decomp,
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
    
    n_g_assets = switch(input$slider_91_Decomp,
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
    
    n_g_services = switch(input$slider_91_Decomp,
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
    
    n_g_electricity = switch(input$slider_91_Decomp,
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
    
    n_g_cooking_fuel = switch(input$slider_91_Decomp,
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
    
    n_g_water = switch(input$slider_91_Decomp,
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
    
    n_g_toilet = switch(input$slider_91_Decomp,
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
    
    n_g_land = switch(input$slider_91_Decomp,
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
    
    n_g_livestock = switch(input$slider_91_Decomp,
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
    
    n_g_rural_equip = switch(input$slider_91_Decomp,
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
    c_g_selection = strtoi(input$c_g_Decomp_91)
    
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
   
    
    edu_max_labels <- get_label(map@data$ADM2_EN, "Max. Education", edu_max, n_edu_max)
    edu_dropout_labels <- get_label(map@data$ADM2_EN, "Education Dropout", edu_dropout, n_edu_dropout)
    hea_chronic_labels <- get_label(map@data$ADM2_EN, "Chronic Illness", hea_chronic, n_hea_chronic)
    hea_visit_labels <- get_label(map@data$ADM2_EN, "Lack of Health Visit", hea_visit, n_hea_visit)
    employment_labels <- get_label(map@data$ADM2_EN, "Unemployment", employment, n_employment)
    assets_labels <- get_label(map@data$ADM2_EN, "Household Assets", assets, n_assets)
    services_labels <- get_label(map@data$ADM2_EN, "Access to Services", services, n_services)
    electricity_labels <- get_label(map@data$ADM2_EN, "Lack of Electricity", electricity, n_electricity)
    cooking_fuel_labels <- get_label(map@data$ADM2_EN, "Poor Cooking Fuel", cooking_fuel, n_cooking_fuel)
    water_labels <- get_label(map@data$ADM2_EN, "Poor Water Source", water, n_water)
    toilet_labels <- get_label(map@data$ADM2_EN, "Lack of Toilet", toilet, n_toilet)
    land_labels <- get_label(map@data$ADM2_EN, "Lack of Land", land, n_land)
    livestock_labels <- get_label(map@data$ADM2_EN, "Lack of Livestock", livestock, n_livestock)
    rural_equip_labels <- get_label(map@data$ADM2_EN, "Lack of Rural Equipment", rural_equip, n_rural_equip)

    
    pal <- colorNumeric(
      palette = "viridis",
      domain = switch(c_g_selection,
                      c(0, .6),
                      c(0, 1)),
      reverse = TRUE)
    
    ## MAPPING MPI Decomposition 2017 60 Districs----------------------------------------------------------------------
    # These lines of code fix the positioning of the "No Data" label. Previously, it
    # was appearing to the right of the data instead of at the bottom where it was 
    # supposed to. 
    css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
    html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
    
    # This is where the map gets plotted 
    m <- leaflet(
      options = leafletOptions(
        minZoom = 0, maxZoom= 18,
        drag = FALSE)) %>% addTiles() %>%
      setView(lng = 30, lat=-19, zoom=6)
    
    m %>%
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
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = c(0, 0.6), opacity = 0.7, title = paste0("k = ", input$slider_91_Decomp),
                na.label = "No Data",
                group = c("Poverty Index", "Max. Education"),
                position = "bottomleft") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  output$Dist_60_Decomp_Map <- renderLeaflet({
    # This is the level that the decomposition data is selected on. 
    # 1 = g0, c0, 2 = g1, c1, 3 = g2, c2
    level_selection = strtoi(input$LevelSelection_Decomp_60)
    
    # 1 = Total, 2 = Urban, 3 = Rural
    UrbRurSelection = strtoi(input$UrbRurSelection_Decomp_60)
    
    map = switch(UrbRurSelection,
                 MAP_2017_60_T_o,
                 MAP_2017_60_U_o,
                 MAP_2017_60_R_o)
    nat_data = switch(UrbRurSelection,
                      MPI_2017_1_T_o,
                      MPI_2017_1_U_o,
                      MPI_2017_1_R_o)
    
    
    g_edu_max = switch(input$slider_60_Decomp,
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
    g_edu_dropout = switch(input$slider_60_Decomp,
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
    
    g_hea_chronic = switch(input$slider_60_Decomp,
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
    
    g_hea_visit = switch(input$slider_60_Decomp,
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
    
    g_employment = switch(input$slider_60_Decomp,
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
    
    g_assets = switch(input$slider_60_Decomp,
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
    
    g_services = switch(input$slider_60_Decomp,
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
    
    g_electricity = switch(input$slider_60_Decomp,
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
    
    g_cooking_fuel = switch(input$slider_60_Decomp,
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
    
    g_water = switch(input$slider_60_Decomp,
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
    
    g_toilet = switch(input$slider_60_Decomp,
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
    
    g_land = switch(input$slider_60_Decomp,
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
    
    g_livestock = switch(input$slider_60_Decomp,
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
    
    g_rural_equip = switch(input$slider_60_Decomp,
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
    
    
    
    
    
    c_edu_max = switch(input$slider_60_Decomp,
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
    c_edu_dropout = switch(input$slider_60_Decomp,
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
    
    c_hea_chronic = switch(input$slider_60_Decomp,
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
    
    c_hea_visit = switch(input$slider_60_Decomp,
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
    
    c_employment = switch(input$slider_60_Decomp,
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
    
    c_assets = switch(input$slider_60_Decomp,
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
    
    c_services = switch(input$slider_60_Decomp,
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
    
    c_electricity = switch(input$slider_60_Decomp,
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
    
    c_cooking_fuel = switch(input$slider_60_Decomp,
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
    
    c_water = switch(input$slider_60_Decomp,
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
    
    c_toilet = switch(input$slider_60_Decomp,
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
    
    c_land = switch(input$slider_60_Decomp,
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
    
    c_livestock = switch(input$slider_60_Decomp,
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
    
    c_rural_equip = switch(input$slider_60_Decomp,
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
    
    
    
    n_c_edu_max = switch(input$slider_60_Decomp,
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
    n_c_edu_dropout = switch(input$slider_60_Decomp,
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
    
    n_c_hea_chronic = switch(input$slider_60_Decomp,
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
    
    n_c_hea_visit = switch(input$slider_60_Decomp,
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
    
    n_c_employment = switch(input$slider_60_Decomp,
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
    
    n_c_assets = switch(input$slider_60_Decomp,
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
    
    n_c_services = switch(input$slider_60_Decomp,
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
    
    n_c_electricity = switch(input$slider_60_Decomp,
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
    
    n_c_cooking_fuel = switch(input$slider_60_Decomp,
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
    
    n_c_water = switch(input$slider_60_Decomp,
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
    
    n_c_toilet = switch(input$slider_60_Decomp,
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
    
    n_c_land = switch(input$slider_60_Decomp,
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
    
    n_c_livestock = switch(input$slider_60_Decomp,
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
    
    n_c_rural_equip = switch(input$slider_60_Decomp,
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
    
    n_g_edu_max = switch(input$slider_60_Decomp,
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
    n_g_edu_dropout = switch(input$slider_60_Decomp,
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
    
    n_g_hea_chronic = switch(input$slider_60_Decomp,
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
    
    n_g_hea_visit = switch(input$slider_60_Decomp,
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
    
    n_g_employment = switch(input$slider_60_Decomp,
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
    
    n_g_assets = switch(input$slider_60_Decomp,
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
    
    n_g_services = switch(input$slider_60_Decomp,
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
    
    n_g_electricity = switch(input$slider_60_Decomp,
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
    
    n_g_cooking_fuel = switch(input$slider_60_Decomp,
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
    
    n_g_water = switch(input$slider_60_Decomp,
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
    
    n_g_toilet = switch(input$slider_60_Decomp,
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
    
    n_g_land = switch(input$slider_60_Decomp,
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
    
    n_g_livestock = switch(input$slider_60_Decomp,
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
    
    n_g_rural_equip = switch(input$slider_60_Decomp,
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
    c_g_selection = strtoi(input$c_g_Decomp_60)
    
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
    
    ## MAPPING MPI Decomposition 2017 10 Province----------------------------------------------------------------------
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
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = c(0, 0.6), opacity = 0.7, title = paste0("k = ", input$slider_60_Decomp),
                na.label = "No Data",
                group = c("Poverty Index", "Max. Education"),
                position = "bottomleft") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  output$Prov_Decomp_Map <- renderLeaflet({
    # This is the level that the decomposition data is selected on. 
    # 1 = g0, c0, 2 = g1, c1, 3 = g2, c2
    level_selection = strtoi(input$LevelSelection_Decomp_Prov)
    
    # 1 = Total, 2 = Urban, 3 = Rural
    UrbRurSelection = strtoi(input$UrbRurSelection_Decomp_Prov)
    
    map = switch(UrbRurSelection,
                 MAP_2017_10_T_o,
                 MAP_2017_10_U_o,
                 MAP_2017_10_R_o)
    nat_data = switch(UrbRurSelection,
                      MPI_2017_1_T_o,
                      MPI_2017_1_U_o,
                      MPI_2017_1_R_o)
    
    g_edu_max = switch(input$slider_Prov_Decomp,
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
    g_edu_dropout = switch(input$slider_Prov_Decomp,
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
    
    g_hea_chronic = switch(input$slider_Prov_Decomp,
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
    
    g_hea_visit = switch(input$slider_Prov_Decomp,
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
    
    g_employment = switch(input$slider_Prov_Decomp,
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
    
    g_assets = switch(input$slider_Prov_Decomp,
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
    
    g_services = switch(input$slider_Prov_Decomp,
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
    
    g_electricity = switch(input$slider_Prov_Decomp,
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
    
    g_cooking_fuel = switch(input$slider_Prov_Decomp,
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
    
    g_water = switch(input$slider_Prov_Decomp,
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
    
    g_toilet = switch(input$slider_Prov_Decomp,
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
    
    g_land = switch(input$slider_Prov_Decomp,
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
    
    g_livestock = switch(input$slider_Prov_Decomp,
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
    
    g_rural_equip = switch(input$slider_Prov_Decomp,
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
    
    
    
    
    
    c_edu_max = switch(input$slider_Prov_Decomp,
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
    c_edu_dropout = switch(input$slider_Prov_Decomp,
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
    
    c_hea_chronic = switch(input$slider_Prov_Decomp,
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
    
    c_hea_visit = switch(input$slider_Prov_Decomp,
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
    
    c_employment = switch(input$slider_Prov_Decomp,
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
    
    c_assets = switch(input$slider_Prov_Decomp,
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
    
    c_services = switch(input$slider_Prov_Decomp,
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
    
    c_electricity = switch(input$slider_Prov_Decomp,
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
    
    c_cooking_fuel = switch(input$slider_Prov_Decomp,
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
    
    c_water = switch(input$slider_Prov_Decomp,
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
    
    c_toilet = switch(input$slider_Prov_Decomp,
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
    
    c_land = switch(input$slider_Prov_Decomp,
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
    
    c_livestock = switch(input$slider_Prov_Decomp,
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
    
    c_rural_equip = switch(input$slider_Prov_Decomp,
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
    
    
    
    n_c_edu_max = switch(input$slider_Prov_Decomp,
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
    n_c_edu_dropout = switch(input$slider_Prov_Decomp,
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
    
    n_c_hea_chronic = switch(input$slider_Prov_Decomp,
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
    
    n_c_hea_visit = switch(input$slider_Prov_Decomp,
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
    
    n_c_employment = switch(input$slider_Prov_Decomp,
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
    
    n_c_assets = switch(input$slider_Prov_Decomp,
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
    
    n_c_services = switch(input$slider_Prov_Decomp,
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
    
    n_c_electricity = switch(input$slider_Prov_Decomp,
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
    
    n_c_cooking_fuel = switch(input$slider_Prov_Decomp,
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
    
    n_c_water = switch(input$slider_Prov_Decomp,
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
    
    n_c_toilet = switch(input$slider_Prov_Decomp,
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
    
    n_c_land = switch(input$slider_Prov_Decomp,
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
    
    n_c_livestock = switch(input$slider_Prov_Decomp,
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
    
    n_c_rural_equip = switch(input$slider_Prov_Decomp,
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
    
    n_g_edu_max = switch(input$slider_Prov_Decomp,
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
    n_g_edu_dropout = switch(input$slider_Prov_Decomp,
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
    
    n_g_hea_chronic = switch(input$slider_Prov_Decomp,
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
    
    n_g_hea_visit = switch(input$slider_Prov_Decomp,
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
    
    n_g_employment = switch(input$slider_Prov_Decomp,
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
    
    n_g_assets = switch(input$slider_Prov_Decomp,
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
    
    n_g_services = switch(input$slider_Prov_Decomp,
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
    
    n_g_electricity = switch(input$slider_Prov_Decomp,
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
    
    n_g_cooking_fuel = switch(input$slider_Prov_Decomp,
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
    
    n_g_water = switch(input$slider_Prov_Decomp,
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
    
    n_g_toilet = switch(input$slider_Prov_Decomp,
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
    
    n_g_land = switch(input$slider_Prov_Decomp,
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
    
    n_g_livestock = switch(input$slider_Prov_Decomp,
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
    
    n_g_rural_equip = switch(input$slider_Prov_Decomp,
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
    c_g_selection = strtoi(input$c_g_Decomp_Prov)
    
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
    
    edu_max_labels <- get_label(map@data$ADM1_EN, "Max. Education", edu_max, n_edu_max)
    edu_dropout_labels <- get_label(map@data$ADM1_EN, "Education Dropout", edu_dropout, n_edu_dropout)
    hea_chronic_labels <- get_label(map@data$ADM1_EN, "Chronic Illness", hea_chronic, n_hea_chronic)
    hea_visit_labels <- get_label(map@data$ADM1_EN, "Lack of Health Visit", hea_visit, n_hea_visit)
    employment_labels <- get_label(map@data$ADM1_EN, "Unemployment", employment, n_employment)
    assets_labels <- get_label(map@data$ADM1_EN, "Household Assets", assets, n_assets)
    services_labels <- get_label(map@data$ADM1_EN, "Access to Services", services, n_services)
    electricity_labels <- get_label(map@data$ADM1_EN, "Lack of Electricity", electricity, n_electricity)
    cooking_fuel_labels <- get_label(map@data$ADM1_EN, "Poor Cooking Fuel", cooking_fuel, n_cooking_fuel)
    water_labels <- get_label(map@data$ADM1_EN, "Poor Water Source", water, n_water)
    toilet_labels <- get_label(map@data$ADM1_EN, "Lack of Toilet", toilet, n_toilet)
    land_labels <- get_label(map@data$ADM1_EN, "Lack of Land", land, n_land)
    livestock_labels <- get_label(map@data$ADM1_EN, "Lack of Livestock", livestock, n_livestock)
    rural_equip_labels <- get_label(map@data$ADM1_EN, "Lack of Rural Equipment", rural_equip, n_rural_equip)
    
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = switch(c_g_selection,
                      c(0, .6),
                      c(0, 1)),
      reverse = TRUE)
    
    ## MAPPING Change 2011 - 2017----------------------------------------------------------------------
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
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = c(0, 0.6), opacity = 0.7, title = paste0("k = ", input$slider_Prov_Decomp),
                na.label = "No Data",
                group = c("Poverty Index", "Max. Education"),
                position = "bottomleft") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  output$M0_Comparison_Map <- renderLeaflet({
    UrbRurSelection = strtoi(input$UrbRurSelection_M0)
    
    RegionSelection = strtoi(input$RegionSelection_M0)
    
    
    
    k_threshold = input$slider_M0_Comparison
    
    
    map_2017 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2017_60_T_o,
                             MAP_2017_10_T_o),
                      switch(RegionSelection,
                             MAP_2017_60_U_o, 
                             MAP_2017_10_U_o),
                      switch(RegionSelection,
                             MAP_2017_60_R_o,
                             MAP_2017_10_R_o))
    
    
    map_2011 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2011_60_T_o,
                             MAP_2011_10_T_o),
                      switch(RegionSelection,
                             MAP_2011_60_U_o, 
                             MAP_2011_10_U_o),
                      switch(RegionSelection,
                             MAP_2011_60_R_o,
                             MAP_2011_10_R_o))
    
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
    
    M0_2011 <- M0_2011 %>% round(digits = 3)
    M0_2017 <- M0_2017 %>% round(digits = 3)
    
    M0_change = (M0_2017 - M0_2011) %>% round(digits = 3)
    
    
    
    map_2017@data$M0_change = M0_change
    
    names = switch(RegionSelection,
                   map_2017@data$NAME_2,
                   map_2017@data$ADM1_EN)
    
    change_labels <- sprintf(
      paste0("<strong>%s</strong><br/>
    <strong>" , "M<sub>0</sub> Change" , ":</strong> %g<br/>"),
      names, M0_change) %>% lapply(htmltools::HTML)
    
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
      
      addLegend(pal = pal, values = M0_change, opacity = 0.7, title = paste0("k = ", k_threshold),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  # This is where the scatte rplot gets plotted
  output$M0_Scatterplot <- renderPlotly({
    UrbRurSelection = strtoi(input$UrbRurSelection_M0)
    RegionSelection = strtoi(input$RegionSelection_M0)
    k_threshold = input$slider_M0_Comparison
    
    map_2017 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2017_60_T_o,
                             MAP_2017_10_T_o),
                      switch(RegionSelection,
                             MAP_2017_60_U_o, 
                             MAP_2017_10_U_o),
                      switch(RegionSelection,
                             MAP_2017_60_R_o,
                             MAP_2017_10_R_o))
    
    
    map_2011 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2011_60_T_o,
                             MAP_2011_10_T_o),
                      switch(RegionSelection,
                             MAP_2011_60_U_o, 
                             MAP_2011_10_U_o),
                      switch(RegionSelection,
                             MAP_2011_60_R_o,
                             MAP_2011_10_R_o))
    
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
    
    names = switch(RegionSelection,
                   map_2017@data$NAME_2,
                   map_2017@data$ADM1_EN)
    M0_2011 <- M0_2011 %>% round(digits = 3)
    M0_2017 <- M0_2017 %>% round(digits = 3)
    create_scatter(names, M0_2011, M0_2017, "M<sub>0</sub> for 2011", "M<sub>0</sub> for 2017", "Comparison of \\(M_0\\) from 2011 to 2017") 
  })
  # This is where the scatterplot gets plotted
  output$M1_Scatterplot <- renderPlotly({
    UrbRurSelection = strtoi(input$UrbRurSelection_M1)
    
    RegionSelection = strtoi(input$RegionSelection_M1)
    
    
    
    k_threshold = input$slider_M1_Comparison
    
    
    map_2017 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2017_60_T_o,
                             MAP_2017_10_T_o),
                      switch(RegionSelection,
                             MAP_2017_60_U_o, 
                             MAP_2017_10_U_o),
                      switch(RegionSelection,
                             MAP_2017_60_R_o,
                             MAP_2017_10_R_o))
    
    
    map_2011 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2011_60_T_o,
                             MAP_2011_10_T_o),
                      switch(RegionSelection,
                             MAP_2011_60_U_o, 
                             MAP_2011_10_U_o),
                      switch(RegionSelection,
                             MAP_2011_60_R_o,
                             MAP_2011_10_R_o))
    
    M1_2017 = switch(k_threshold,
                     map_2017@data$M1_k1,
                     map_2017@data$M1_k2,
                     map_2017@data$M1_k3,
                     map_2017@data$M1_k4,
                     map_2017@data$M1_k5,
                     map_2017@data$M1_k6,
                     map_2017@data$M1_k7,
                     map_2017@data$M1_k8,
                     map_2017@data$M1_k9)
    
    M1_2011 = switch(k_threshold,
                     map_2011@data$M1_k1,
                     map_2011@data$M1_k2,
                     map_2011@data$M1_k3,
                     map_2011@data$M1_k4,
                     map_2011@data$M1_k5,
                     map_2011@data$M1_k6,
                     map_2011@data$M1_k7,
                     map_2011@data$M1_k8,
                     map_2011@data$M1_k9)
    
    names = switch(RegionSelection,
                   map_2017@data$NAME_2,
                   map_2017@data$ADM1_EN)
    M1_2011 <- M1_2011 %>% round(digits = 3)
    M1_2017 <- M1_2017 %>% round(digits = 3)
    
    create_scatter(names, M1_2011, M1_2017, "M<sub>1</sub> for 2011", "M<sub>1</sub> for 2017", "Comparison of \\(M_1 \\) from 2011 to 2017") 
  })
   
  # This is where the map gets plotted
  output$M1_Comparison_Map <- renderLeaflet({
    UrbRurSelection = strtoi(input$UrbRurSelection_M1)
    
    RegionSelection = strtoi(input$RegionSelection_M1)
    
    
    
    k_threshold = input$slider_M1_Comparison
    
    
    map_2017 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2017_60_T_o,
                             MAP_2017_10_T_o),
                      switch(RegionSelection,
                             MAP_2017_60_U_o, 
                             MAP_2017_10_U_o),
                      switch(RegionSelection,
                             MAP_2017_60_R_o,
                             MAP_2017_10_R_o))
    
    
    map_2011 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2011_60_T_o,
                             MAP_2011_10_T_o),
                      switch(RegionSelection,
                             MAP_2011_60_U_o, 
                             MAP_2011_10_U_o),
                      switch(RegionSelection,
                             MAP_2011_60_R_o,
                             MAP_2011_10_R_o))
    
    M1_2017 = switch(k_threshold,
                     map_2017@data$M1_k1,
                     map_2017@data$M1_k2,
                     map_2017@data$M1_k3,
                     map_2017@data$M1_k4,
                     map_2017@data$M1_k5,
                     map_2017@data$M1_k6,
                     map_2017@data$M1_k7,
                     map_2017@data$M1_k8,
                     map_2017@data$M1_k9)
    
    M1_2011 = switch(k_threshold,
                     map_2011@data$M1_k1,
                     map_2011@data$M1_k2,
                     map_2011@data$M1_k3,
                     map_2011@data$M1_k4,
                     map_2011@data$M1_k5,
                     map_2011@data$M1_k6,
                     map_2011@data$M1_k7,
                     map_2011@data$M1_k8,
                     map_2011@data$M1_k9)
    
    M1_change = (M1_2017 - M1_2011) %>% round(digits=3)
    
    map_2017@data$M1_change = M1_change
    
    names = switch(RegionSelection,
                   map_2017@data$NAME_2,
                   map_2017@data$ADM1_EN)
    
    change_labels <- sprintf(
      paste0("<strong>%s</strong><br/>
    <strong>" , "M<sub>1</sub> Change" , ":</strong> %g<br/>"),
      names, M1_change) %>% lapply(htmltools::HTML)
    
    css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
    html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = M1_change,
      reverse = TRUE)
    
    # This is where the map gets plotted 
    leaflet(
      options = leafletOptions(
        minZoom = 0, maxZoom= 18,
        drag = FALSE)) %>% addTiles() %>%
      setView(lng = 30, lat=-19, zoom=6) %>% 
      get_polygon(map_2017, pal, M1_change, change_labels, "M1") %>%
      clearControls() %>%
      
      addLegend(pal = pal, values = M1_change, opacity = 0.7, title = paste0("k = ", k_threshold),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  # This is where the scatterplot gets plotted
  output$M2_Scatterplot <- renderPlotly({
    UrbRurSelection = strtoi(input$UrbRurSelection_M2)
    
    RegionSelection = strtoi(input$RegionSelection_M2)
    
    
    
    k_threshold = input$slider_M2_Comparison
    
    map_2017 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2017_60_T_o,
                             MAP_2017_10_T_o),
                      switch(RegionSelection,
                             MAP_2017_60_U_o, 
                             MAP_2017_10_U_o),
                      switch(RegionSelection,
                             MAP_2017_60_R_o,
                             MAP_2017_10_R_o))
    
    
    map_2011 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2011_60_T_o,
                             MAP_2011_10_T_o),
                      switch(RegionSelection,
                             MAP_2011_60_U_o, 
                             MAP_2011_10_U_o),
                      switch(RegionSelection,
                             MAP_2011_60_R_o,
                             MAP_2011_10_R_o))
    
    M2_2017 = switch(k_threshold,
                     map_2017@data$M2_k1,
                     map_2017@data$M2_k2,
                     map_2017@data$M2_k3,
                     map_2017@data$M2_k4,
                     map_2017@data$M2_k5,
                     map_2017@data$M2_k6,
                     map_2017@data$M2_k7,
                     map_2017@data$M2_k8,
                     map_2017@data$M2_k9)
    
    M2_2011 = switch(k_threshold,
                     map_2011@data$M2_k1,
                     map_2011@data$M2_k2,
                     map_2011@data$M2_k3,
                     map_2011@data$M2_k4,
                     map_2011@data$M2_k5,
                     map_2011@data$M2_k6,
                     map_2011@data$M2_k7,
                     map_2011@data$M2_k8,
                     map_2011@data$M2_k9)
    
    names = switch(RegionSelection,
                   map_2017@data$NAME_2,
                   map_2017@data$ADM1_EN)
    M2_2011 <- M2_2011 %>% round(digits = 3)
    M2_2017 <- M2_2017 %>% round(digits = 3)
    
    create_scatter(names, M2_2011, M2_2017, "M<sub>2</sub> for 2011", "M<sub>2</sub> for 2017", "Comparison of \\(M_2\\) from 2011 to 2017")
  }) 
  
  # This is where the map plot gets plotted
  output$M2_Comparison_Map <- renderLeaflet({
    UrbRurSelection = strtoi(input$UrbRurSelection_M2)
    
    RegionSelection = strtoi(input$RegionSelection_M2)
    
    
    
    k_threshold = input$slider_M2_Comparison
    
    
    map_2017 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2017_60_T_o,
                             MAP_2017_10_T_o),
                      switch(RegionSelection,
                             MAP_2017_60_U_o, 
                             MAP_2017_10_U_o),
                      switch(RegionSelection,
                             MAP_2017_60_R_o,
                             MAP_2017_10_R_o))
    
    
    map_2011 = switch(UrbRurSelection, 
                      switch(RegionSelection,
                             MAP_2011_60_T_o,
                             MAP_2011_10_T_o),
                      switch(RegionSelection,
                             MAP_2011_60_U_o, 
                             MAP_2011_10_U_o),
                      switch(RegionSelection,
                             MAP_2011_60_R_o,
                             MAP_2011_10_R_o))
    
    M2_2017 = switch(k_threshold,
                     map_2017@data$M2_k1,
                     map_2017@data$M2_k2,
                     map_2017@data$M2_k3,
                     map_2017@data$M2_k4,
                     map_2017@data$M2_k5,
                     map_2017@data$M2_k6,
                     map_2017@data$M2_k7,
                     map_2017@data$M2_k8,
                     map_2017@data$M2_k9)
    
    M2_2011 = switch(k_threshold,
                     map_2011@data$M2_k1,
                     map_2011@data$M2_k2,
                     map_2011@data$M2_k3,
                     map_2011@data$M2_k4,
                     map_2011@data$M2_k5,
                     map_2011@data$M2_k6,
                     map_2011@data$M2_k7,
                     map_2011@data$M2_k8,
                     map_2011@data$M2_k9)
    
    M2_change = (M2_2017 - M2_2011) %>% round(digits = 3)
    
    map_2017@data$M2_change = M2_change
    
    names = switch(RegionSelection,
                   map_2017@data$NAME_2,
                   map_2017@data$ADM1_EN)
    
    change_labels <- sprintf(
      paste0("<strong>%s</strong><br/>
    <strong>" , "M<sub>2</sub> Change" , ":</strong> %g<br/>"),
      names, M2_change) %>% lapply(htmltools::HTML)
    
    css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
    html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = M2_change,
      reverse = TRUE)
    
    # This is where the map gets plotted 
    leaflet(
      options = leafletOptions(
        minZoom = 0, maxZoom= 12,
        drag = FALSE)) %>% addTiles() %>%
      setView(lng = 30, lat=-19, zoom=6) %>% 
      get_polygon(map_2017, pal, M2_change, change_labels, "M2") %>%
      clearControls() %>%
      
      addLegend(pal = pal, values = M2_change, opacity = 0.7, title = paste0("k = ", k_threshold),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  ## Graphing Ranked District 91 Bar Chart --------------------------------------
  ranked_dis_data <- read_csv("./data/MappingData/OriginalMPI/2017/2017_91_District.csv") 
  #ranked_dis_data[[3]] <-  ranked_dis_data[[3]] %>% round(digits =3) 
  # Suggestion
  #
  M0_dist_91_rank <- reactive({
    input$M0_k_91_threshold
  })
  
  
  
  output$M0_dis_91_ranking <- renderPlotly({
    if (M0_dist_91_rank () == "1") {
      M0_k1_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M0_k1)) %>% 
        ggplot(aes(x = District , y = M0_k1)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 1", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k1_ranking)
      
    }
    
    else if (M0_dist_91_rank () == "2") {
      M0_k2_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M0_k2)) %>% 
        ggplot(aes(x = District , y = M0_k2%>% round(digits = 3))) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 2", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k2_ranking)
      
    }
    
    else if (M0_dist_91_rank () == "3") {
      M0_k3_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M0_k3)) %>% 
        ggplot(aes(x = District , y = M0_k3 )) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 3", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k3_ranking)
      
    }
    
    else if (M0_dist_91_rank () == "4") {
      M0_k4_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M0_k4)) %>% 
        ggplot(aes(x = District , y = M0_k4)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 4", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k4_ranking)
      
    }
    
    else if (M0_dist_91_rank () == "5") {
      M0_k5_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M0_k5)) %>% 
        ggplot(aes(x = District , y = M0_k5)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 5", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k5_ranking)
      
    }
    
    else if (M0_dist_91_rank () == "6") {
      M0_k6_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M0_k6)) %>% 
        ggplot(aes(x = District , y = M0_k6)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 6", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k6_ranking)
      
    }
    
    else if (M0_dist_91_rank () == "7") {
      M0_k7_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M0_k7)) %>% 
        ggplot(aes(x = District , y = M0_k7)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 7", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k7_ranking)
      
    }
    
    else if (M0_dist_91_rank () == "8") {
      M0_k8_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M0_k8)) %>% 
        ggplot(aes(x = District , y = M0_k8)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 8", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k8_ranking)
      
    }
    
    else if (M0_dist_91_rank () == "9") {
      
      M0_k9_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M0_k9)) %>% 
        ggplot(aes(x = District , y = M0_k9)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 9", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k9_ranking)
      
    }
    
    
  })
  
  M1_dist_91_rank <- reactive({
    input$M1_k_91_threshold
  })
  
  
  
  output$M1_dis_91_ranking <- renderPlotly({
    if (M1_dist_91_rank () == "1") {
      M1_k1_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M1_k1)) %>% 
        ggplot(aes(x = District , y = M1_k1)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 1", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k1_ranking)
      
    }
    
    else if (M1_dist_91_rank () == "2") {
      M1_k2_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M1_k2)) %>% 
        ggplot(aes(x = District , y = M1_k2)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 2", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k2_ranking)
      
    }
    
    else if (M1_dist_91_rank () == "3") {
      M1_k3_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M1_k3)) %>% 
        ggplot(aes(x = District , y = M1_k3)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 3", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k3_ranking)
      
    }
    
    else if (M1_dist_91_rank () == "4") {
      M1_k4_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M1_k4)) %>% 
        ggplot(aes(x = District , y = M1_k4)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 4", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k4_ranking)
      
    }
    
    else if (M1_dist_91_rank () == "5") {
      M1_k5_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M1_k5)) %>% 
        ggplot(aes(x = District , y = M1_k5)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 5", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k5_ranking)
      
    }
    
    else if (M1_dist_91_rank () == "6") {
      M1_k6_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M1_k6)) %>% 
        ggplot(aes(x = District , y = M1_k6)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 6", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k6_ranking)
      
    }
    
    else if (M1_dist_91_rank () == "7") {
      M1_k7_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M1_k7)) %>% 
        ggplot(aes(x = District , y = M1_k7)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 7", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k7_ranking)
      
    }
    
    else if (M1_dist_91_rank () == "8") {
      M1_k8_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M1_k8)) %>% 
        ggplot(aes(x = District , y = M1_k8)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 8", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k8_ranking)
      
    }
    
    else if (M1_dist_91_rank () == "9") {
      
      M1_k9_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M1_k9)) %>% 
        ggplot(aes(x = District , y = M1_k9)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 9", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k9_ranking)
      
    }
    
    
  })
  
  M2_dist_91_rank <- reactive({
    input$M2_k_91_threshold
  })
  
  
  
  output$M2_dis_91_ranking <- renderPlotly({
    if (M2_dist_91_rank () == "1") {
      M2_k1_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M2_k1)) %>% 
        ggplot(aes(x = District , y = M2_k1)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 1", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k1_ranking)
      
    }
    
    else if (M2_dist_91_rank () == "2") {
      M2_k2_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M2_k2)) %>% 
        ggplot(aes(x = District , y = M2_k2)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 2", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k2_ranking)
      
    }
    
    else if (M2_dist_91_rank () == "3") {
      M2_k3_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M2_k3)) %>% 
        ggplot(aes(x = District , y = M2_k3)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 3", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k3_ranking)
      
    }
    
    else if (M2_dist_91_rank () == "4") {
      M2_k4_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M2_k4)) %>% 
        ggplot(aes(x = District , y = M2_k4)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 4", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k4_ranking)
      
    }
    
    else if (M2_dist_91_rank () == "5") {
      M2_k5_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M2_k5)) %>% 
        ggplot(aes(x = District , y = M2_k5)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 5", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k5_ranking)
      
    }
    
    else if (M2_dist_91_rank () == "6") {
      M2_k6_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M2_k6)) %>% 
        ggplot(aes(x = District , y = M2_k6)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 6", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k6_ranking)
      
    }
    
    else if (M2_dist_91_rank () == "7") {
      M2_k7_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M2_k7)) %>% 
        ggplot(aes(x = District , y = M2_k7)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 7", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k7_ranking)
      
    }
    
    else if (M2_dist_91_rank () == "8") {
      M2_k8_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M2_k8)) %>% 
        ggplot(aes(x = District , y = M2_k8)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 8", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k8_ranking)
      
    }
    
    else if (M2_dist_91_rank () == "9") {
      
      M2_k9_ranking <- ranked_dis_data %>% 
        mutate(District = fct_reorder(District_name, M2_k9)) %>% 
        ggplot(aes(x = District , y = M2_k9)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 9", x = "Districts", title = "91 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k9_ranking)
      
    }
    
    
  })
  
  
  ## Graphing Ranked District 60 Bar Chart --------------------------------------
  
  ranked_data <- read_csv("./data/MappingData/OriginalMPI/2017/2017_District.csv")
  # Suggestion
  #
  M0_dist_rank <- reactive({
    input$M0_k_threshold
  })
  
  
  
  output$M0_ranking <- renderPlotly({
    if (M0_dist_rank() == "1") {
      M0_k1_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M0_k1)) %>% 
        ggplot(aes(x = District , y = M0_k1)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 1", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k1_ranking)
      
    }
    
    else if (M0_dist_rank() == "2") {
      M0_k2_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M0_k2)) %>% 
        ggplot(aes(x = District , y = M0_k2)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 2", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k2_ranking)
      
    }
    
    else if (M0_dist_rank() == "3") {
      M0_k3_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M0_k3)) %>% 
        ggplot(aes(x = District , y = M0_k3)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 3", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k3_ranking)
      
    }
    
    else if (M0_dist_rank() == "4") {
      M0_k4_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M0_k4)) %>% 
        ggplot(aes(x = District , y = M0_k4)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 4", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k4_ranking)
      
    }
    
    else if (M0_dist_rank() == "5") {
      M0_k5_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M0_k5)) %>% 
        ggplot(aes(x = District , y = M0_k5)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 5", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k5_ranking)
      
    }
    
    else if (M0_dist_rank() == "6") {
      M0_k6_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M0_k6)) %>% 
        ggplot(aes(x = District , y = M0_k6)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 6", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k6_ranking)
      
    }
    
    else if (M0_dist_rank() == "7") {
      M0_k7_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M0_k7)) %>% 
        ggplot(aes(x = District , y = M0_k7)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 7", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k7_ranking)
      
    }
    
    else if (M0_dist_rank() == "8") {
      M0_k8_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M0_k8)) %>% 
        ggplot(aes(x = District , y = M0_k8)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 8", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k8_ranking)
      
    }
    
    else if (M0_dist_rank() == "9") {
      
      M0_k9_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M0_k9)) %>% 
        ggplot(aes(x = District , y = M0_k9)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 9", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M0_k9_ranking)
      
    }
    
    
  })
  
  M1_dist_rank <- reactive({
    input$M1_k_threshold
  })
  
  output$M1_ranking <- renderPlotly({
    if (M1_dist_rank() == "1") {
      M1_k1_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M1_k1)) %>% 
        ggplot(aes(x = District , y = M1_k1)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 1", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k1_ranking)
      
    }
    
    else if (M1_dist_rank() == "2") {
      M1_k2_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M1_k2)) %>% 
        ggplot(aes(x = District , y = M1_k2)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 2", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k2_ranking)
      
    }
    
    else if (M1_dist_rank() == "3") {
      M1_k3_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M1_k3)) %>% 
        ggplot(aes(x = District , y = M1_k3)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 3", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k3_ranking)
      
    }
    
    else if (M1_dist_rank() == "4") {
      M1_k4_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M1_k4)) %>% 
        ggplot(aes(x = District , y = M1_k4)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 4", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k4_ranking)
      
    }
    
    else if (M1_dist_rank() == "5") {
      M1_k5_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M1_k5)) %>% 
        ggplot(aes(x = District , y = M1_k5)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 5", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k5_ranking)
      
    }
    
    else if (M1_dist_rank() == "6") {
      M1_k6_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M1_k6)) %>% 
        ggplot(aes(x = District , y = M1_k6)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 6", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k6_ranking)
      
    }
    
    else if (M1_dist_rank() == "7") {
      M1_k7_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M1_k7)) %>% 
        ggplot(aes(x = District , y = M1_k7)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 7", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k7_ranking)
      
    }
    
    else if (M1_dist_rank() == "8") {
      M1_k8_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M1_k8)) %>% 
        ggplot(aes(x = District , y = M1_k8)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 8", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k8_ranking)
      
    }
    
    else if (M1_dist_rank() == "9") {
      
      M1_k9_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M1_k9)) %>% 
        ggplot(aes(x = District , y = M1_k9)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 9", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M1_k9_ranking)
      
    }
    
    
  })
  
  M2_dist_rank <- reactive({
    input$M2_k_threshold
  })
  
  output$M2_ranking <- renderPlotly({
    if (M2_dist_rank() == "1") {
      M2_k1_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M2_k1)) %>% 
        ggplot(aes(x = District , y = M2_k1)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 1", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k1_ranking)
      
    }
    
    else if (M2_dist_rank() == "2") {
      M2_k2_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M2_k2)) %>% 
        ggplot(aes(x = District , y = M2_k2)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 2", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k2_ranking)
      
    }
    
    else if (M2_dist_rank() == "3") {
      M2_k3_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M2_k3)) %>% 
        ggplot(aes(x = District , y = M2_k3)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 3", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k3_ranking)
      
    }
    
    else if (M2_dist_rank() == "4") {
      M2_k4_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M2_k4)) %>% 
        ggplot(aes(x = District , y = M2_k4)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 4", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k4_ranking)
      
    }
    
    else if (M2_dist_rank() == "5") {
      M2_k5_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M2_k5)) %>% 
        ggplot(aes(x = District , y = M2_k5)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 5", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k5_ranking)
      
    }
    
    else if (M2_dist_rank() == "6") {
      M2_k6_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M2_k6)) %>% 
        ggplot(aes(x = District , y = M2_k6)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 6", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k6_ranking)
      
    }
    
    else if (M2_dist_rank() == "7") {
      M2_k7_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M2_k7)) %>% 
        ggplot(aes(x = District , y = M2_k7)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 7", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k7_ranking)
      
    }
    
    else if (M2_dist_rank() == "8") {
      M2_k8_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M2_k8)) %>% 
        ggplot(aes(x = District , y = M2_k8)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 8", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k8_ranking)
      
    }
    
    else if (M2_dist_rank() == "9") {
      
      M2_k9_ranking <- ranked_data %>% 
        mutate(District = fct_reorder(District_name, M2_k9)) %>% 
        ggplot(aes(x = District , y = M2_k9)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 9", x = "Districts", title = "60 District Comparison") +
        theme_minimal()
      
      ggplotly(M2_k9_ranking)
      
    }
    
    
  })
  
  ## Graphing Province Rankings ----------------------------------------------
  
  prov_ranked <- read_csv("./data/MappingData/OriginalMPI/2017/2017_Province.csv") 
  
  M0_prov_k_threshold <- reactive({
    input$M0_prov_k
  })
  
  output$M0_prov_ranking <- renderPlotly({
    if (M0_prov_k_threshold() == "1") {
      M0_k1_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M0_k1)) %>% 
        ggplot(aes(x = Province, y = M0_k1)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 1", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M0_k1_prov_ranking)
      
    }
    
    else if (M0_prov_k_threshold() == "2") {
      M0_k2_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M0_k2)) %>% 
        ggplot(aes(x = Province, y = M0_k2)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 2", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M0_k2_prov_ranking)
      
    }
    
    else if (M0_prov_k_threshold() == "3") {
      M0_k3_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M0_k3)) %>% 
        ggplot(aes(x = Province, y = M0_k3)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 3", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M0_k3_prov_ranking)
      
    }
    
    else if (M0_prov_k_threshold() == "4") {
      M0_k4_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M0_k4)) %>% 
        ggplot(aes(x = Province, y = M0_k4)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 4", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M0_k4_prov_ranking)
      
    }
    
    else if (M0_prov_k_threshold() == "5") {
      M0_k5_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M0_k5)) %>% 
        ggplot(aes(x = Province, y = M0_k5)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 5", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M0_k5_prov_ranking)
      
    }
    
    else if (M0_prov_k_threshold() == "6") {
      M0_k6_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M0_k6)) %>% 
        ggplot(aes(x = Province, y = M0_k6)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 6", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M0_k6_prov_ranking)
      
    }
    
    else if (M0_prov_k_threshold() == "7") {
      M0_k7_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M0_k7)) %>% 
        ggplot(aes(x = Province, y = M0_k7)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 7", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M0_k7_prov_ranking)
      
    }
    
    else if (M0_prov_k_threshold() == "8") {
      M0_k8_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M0_k8)) %>% 
        ggplot(aes(x = Province, y = M0_k8)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 8", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M0_k8_prov_ranking)
      
    }
    
    else if (M0_prov_k_threshold() == "9") {
      M0_k9_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M0_k9)) %>% 
        ggplot(aes(x = Province, y = M0_k9)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M0 at Threshold K = 9", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M0_k9_prov_ranking)
      
    }
    
    
  })
  
  M1_prov_k_threshold <- reactive({
    input$M1_prov_k
  })
  
  output$M1_prov_ranking <- renderPlotly({
    if (M1_prov_k_threshold() == "1") {
      M1_k1_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M1_k1)) %>% 
        ggplot(aes(x = Province, y = M1_k1)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 1", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M1_k1_prov_ranking)
      
    }
    
    else if (M1_prov_k_threshold() == "2") {
      M1_k2_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M1_k2)) %>% 
        ggplot(aes(x = Province, y = M1_k2)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 2", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M1_k2_prov_ranking)
      
    }
    
    else if (M1_prov_k_threshold() == "3") {
      M1_k3_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M1_k3)) %>% 
        ggplot(aes(x = Province, y = M1_k3)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 3", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M1_k3_prov_ranking)
      
    }
    
    else if (M1_prov_k_threshold() == "4") {
      M1_k4_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M1_k4)) %>% 
        ggplot(aes(x = Province, y = M1_k4)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 4", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M1_k4_prov_ranking)
      
    }
    
    else if (M1_prov_k_threshold() == "5") {
      M1_k5_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M1_k5)) %>% 
        ggplot(aes(x = Province, y = M1_k5)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 5", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M1_k5_prov_ranking)
      
    }
    
    else if (M1_prov_k_threshold() == "6") {
      M1_k6_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M1_k6)) %>% 
        ggplot(aes(x = Province, y = M1_k6)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 6", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M1_k6_prov_ranking)
      
    }
    
    else if (M1_prov_k_threshold() == "7") {
      M1_k7_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M1_k7)) %>% 
        ggplot(aes(x = Province, y = M1_k7)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 7", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M1_k7_prov_ranking)
      
    }
    
    else if (M1_prov_k_threshold() == "8") {
      M1_k8_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M1_k8)) %>% 
        ggplot(aes(x = Province, y = M1_k8)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 8", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M1_k8_prov_ranking)
      
    }
    
    else if (M1_prov_k_threshold() == "9") {
      M1_k9_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M1_k9)) %>% 
        ggplot(aes(x = Province, y = M1_k9)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M1 at Threshold K = 9", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M1_k9_prov_ranking)
      
    }
    
    
  })
  
  M2_prov_k_threshold <- reactive({
    input$M2_prov_k
  })
  
  output$M2_prov_ranking <- renderPlotly({
    if (M2_prov_k_threshold() == "1") {
      M2_k1_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M2_k1)) %>% 
        ggplot(aes(x = Province, y = M2_k1)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 1", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M2_k1_prov_ranking)
      
    }
    
    else if (M2_prov_k_threshold() == "2") {
      M2_k2_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M2_k2)) %>% 
        ggplot(aes(x = Province, y = M2_k2)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 2", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M2_k2_prov_ranking)
      
    }
    
    else if (M2_prov_k_threshold() == "3") {
      M2_k3_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M2_k3)) %>% 
        ggplot(aes(x = Province, y = M2_k3)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 3", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M2_k3_prov_ranking)
      
    }
    
    else if (M2_prov_k_threshold() == "4") {
      M2_k4_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M2_k4)) %>% 
        ggplot(aes(x = Province, y = M2_k4)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 4", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M2_k4_prov_ranking)
      
    }
    
    else if (M2_prov_k_threshold() == "5") {
      M2_k5_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M2_k5)) %>% 
        ggplot(aes(x = Province, y = M2_k5)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 5", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M2_k5_prov_ranking)
      
    }
    
    else if (M2_prov_k_threshold() == "6") {
      M2_k6_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M2_k6)) %>% 
        ggplot(aes(x = Province, y = M2_k6)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 6", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M2_k6_prov_ranking)
      
    }
    
    else if (M2_prov_k_threshold() == "7") {
      M2_k7_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M2_k7)) %>% 
        ggplot(aes(x = Province, y = M2_k7)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 7", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M2_k7_prov_ranking)
      
    }
    
    else if (M2_prov_k_threshold() == "8") {
      M2_k8_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M2_k8)) %>% 
        ggplot(aes(x = Province, y = M2_k8)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 8", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M2_k8_prov_ranking)
      
    }
    
    else if (M2_prov_k_threshold() == "9") {
      M2_k9_prov_ranking <- prov_ranked %>% 
        mutate(Province = fct_reorder(Province_name, M2_k9)) %>% 
        ggplot(aes(x = Province, y = M2_k9)) +
        geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
        coord_flip() +
        labs(y = "M2 at Threshold K = 9", x = "Province", title = "Province Comparison") +
        theme_minimal()
      
      ggplotly(M2_k9_prov_ranking)
      
    }
    
  })
}

# Run the App--------------------------
shinyApp(ui = ui, server = server)