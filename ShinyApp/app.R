
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
of the gap for each component of the multidimensional poverty index."


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
  print(x_data)
  print(y_data)
  x_data <- round(x_data,digits = 3)
  y_data <- round(y_data,digits = 3)
  # replace NA with 0
  M0_Comparison <- mutate_if(M0_Comparison, is.numeric, ~replace(., is.na(.), 0))
  
  colnames(M0_Comparison)[1] = "Name"
  # return (ggplot(M0_Comparison, aes(x = x_data, y = y_data)) + #, text=~Name)) +
  #           geom_label_repel(aes(label = Name), size = 3, max.overlaps = 4,
  # 
  #                            min.segment.length = unit(0, 'lines'),
  #                            nudge_y = 0.01) +
  #           geom_point(
  #             color= x_data,
  #             fill="#69b3a2",
  #             shape=22,
  #             alpha=1,
  #             size=2,
  #             stroke = 1
  #           ) +
  #           ggtitle(title) +
  #           xlab(x_label) +
  #           ylab(y_label) +
  #           theme_bw() +
  #           geom_abline()) %>% ggplotly()
  #)
          
          
    t <- list(
            family = "sans serif",
            size = 14,
            color = toRGB("grey50"))
    
    return (plot_ly(M0_Comparison, x = ~x_data, y = ~y_data, text=~Name) %>%

            # +
            #             # # xlab(x_label) +
            #             # # ylab(y_label) +
            #             # theme_bw() +
            #             geom_abline() %>%
             
            add_text(textfont = t, textposition = "top right")%>% 
            add_markers()%>%
            layout(shapes=list(type='line', x0=0, x1=(max(x_data,y_data)+0.01), y0=0, y1=(max(x_data,y_data)+0.01)), 
                   showlegend = FALSE,
                   xaxis=list(range.default(), title = x_label), 
                   yaxis=list(range.default(),title=y_label) )  
    )
  
  
}



# UI -------------------------------------------------------------

ui <- navbarPage(title = "Zimbabwe",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                ## Tab main -----------------------------------------------------------
                 tabPanel("Project Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   br(""),
                                   h1(strong("Using 2017 PICES Data to Create a Multidimensional Poverty Index of Zimbabwe")),
                                   fluidRow(style = "margin: 2px;",
                                            img(src = "Zimbabwe_Flag.png", height="100", width="200", alt="Image", style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000;")),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      h4("Department of Agriculture and Applied Economics")
                                   
                          ),
                          
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Project Overview"), align = "center"),
                                          p("OVERVIEW TEXT GOES HERE")),
                                   column(4,
                                          h2(strong("Introduction to Zimbabwe"), align = "center"),
                                          p("Nestled in the Southeastern tip of Africa, Zimbabwe neighbors South Africa, Mozambique, Zambia, and Botswana. Zimbabwe gained independence from Great Britain in 1980 and was ruled by Prime Minister and eventually President Robert MUGABE until his resignation in 2017. Presently, Emmerson Mnangagwa holds office. 
                                            The country is home to roughly 14,830,000 inhabitants, 10% of whom live in the capital city of Harare. Although large agglomerations exist in other major urban areas including Bulawayo and Chitungwiza, population distribution is relatively even otherwise. Zimbabwe’s central government is responsible for regulating 
                                            its 10 provinces and 59 further subdivided districts. Zimbabwe’s terrain consists mostly of plateau upon which forests thrive and arable land is plenty. Because of this, 67.5% of the labor force works in agriculture growing sugar cane, tobacco, fruits, and vegetables among other things. Another 7.3% of the labor force 
                                            takes advantage of the Zimbabwe’s rich natural resources and participates in the industry sector mining and exporting coal, gold, platinum copper, and other metals as well as manufacturing wood products, cement, chemicals, fertilizer, and food. Despite being relatively well-educated and extremely literate, the population 
                                            suffers from both unemployment and severe underemployment in which individuals are overqualified for the jobs they have or are not given adequate work hours. In combination with ubiquitous low wages, this creates an obstacle for economic growth. Monetary poverty measures in 2017 revealed roughly 63% of Zimbabwean households 
                                            lived in poverty. This is reflected in income inequality, overall low standards of living, malnourishment, low life expectancy, high rates of infant/maternal mortality, and difficulty accessing health and education resources.")),

                                   column(4,
                                          h2(strong("Recent History"), align = "center"),
                                          p("After gaining independence in 1980, there was widespread hope that the economic and labor exploitation Africans suffered at the hands of an imperial Great Britain would diminish. While initial trends were encouraging, this hope dwindled as a multitude of factors sent the Zimbabwean economy into decline. Most prominent among 
                                            these factors was inconsistent policy put forth by the central government which resulted in vague and evolving strategies on combatting poverty. An initial scientific socialist policy was applied between 1980 and 1990 to address poverty but was ineffective and thus abandoned due to financial downturn and prolonged drought which 
                                            forced agricultural workers into the cities where they faced even greater poverty due to unemployment. In an attempt to revamp the economy, Zimbabwe sought help from the International Monetary Fund (IMF) and the World Bank (WB) which meant an adoption of more capitalistic policy. The costs of necessities including food, water, and 
                                            education went up as a result, harming and expanding the already existing poor population. The late 1990’s and 2000’s brought ever greater poverty and financial distress to Zimbabwe as a continuing government budget deficit mixed with a fiscal policy focused on increasing the amount of money in circulation resulted in hyperinflation. 
                                            In turn, this increased the economic crisis as foreign investment dropped and Zimbabwean currency crashed. During this time, unemployment skyrocketed and a massive informal sector of the economy emerged. In 2009, Zimbabwe adopted the U.S. dollar along with a handful of other currencies. Though this move somewhat stabilized the 
                                            economy at first, a 2013 shift in government rendered these efforts futile. By 2017, inflation increased significantly as did overall economic crisis and poverty.")),
                                   
                                
                                          # h2(strong("Application of a Multidimensional Poverty Index")),
                                          # p("A brief introduction to Zimbabwe makes clear the severity and the urgency of the poverty situation. Although a money metric approach to measuring poverty is historically prevalent, this sort of strategy is unable to accurately paint an accurate picture of poverty in Zimbabwe. This is most notably due to the extreme hyperinflation the 
                                          #   country suffers from. Because the actual value of money is constantly evolving, the importance of monetary wealth accumulation in determining poverty is questionable. Additionally, variations in consumption tendencies, prices of goods and necessities, and household income distribution can make it difficult to provide an accurate account 
                                          #   of money metric poverty as the value of money is hardly standardized. This volatility also renders money metric comparisons of poverty over time futile as the modern value of currency is incomparable to that of years past. As the practicality of a monetary poverty measure becomes increasingly suspect, the value of alternative poverty measure 
                                          #   methods is revealed. "),
                                          # p("An Alkire Foster (AF) method, developed by Sabina Alkire and James Foster, will be utilized in this project to measure poverty in Zimbabwe. The AF method first denotes the different kinds of deprivations households experience simultaneously. These deprivations make clear who is impoverished in a population and are then used to construct a non-monetary 
                                          #   Multidimensional Poverty Index (MPI). MPI’s are powerful insofar as they provide a non-monetary measure poverty as it exists in its various manifestations. In this way, an MPI accounts for the hyperinflation in Zimbabwe by defining poverty as the inability to satisfy a certain list of needs or capabilities rather than the accumulation of money 
                                          #   that may or may not fulfill such needs. The list, as pictured below, is comprised of variables that indicate deprivation. Each variable corresponds to a broader dimension of poverty. These variables and dimensions are normatively chosen to be applicable in the context of Zimbabwe. The MPI created by the 2021 DSPG Zimbabwe team can be utilized to decompose
                                          #   multidimensional poverty as it exists in different subgroups including the national, provincial, and district level. Additionally, the MPI can be deconstructed to analyze at what strength each deprivation is contributing to poverty within groups. By emulating the work of Stoeffler, et al., this MPI can also be used to track changes in multifaceted poverty 
                                          #   over time. The combination of these unique aspects of the MPI allows it to be used not only to accurately measure poverty as it exists today, but to evaluate the effectiveness of policy going forward.")
                                   
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))))
                 ),

                 ## Tab data and methodology ----------------------------------------------------
                 tabPanel("Data & Methodology", value = "dm",
                             tabsetPanel(
                               tabPanel("Data", value = "data"),
                               tabPanel("Methodology", value = "methodology",
                                        fluidPage(
                                          box(
                                            withMathJax(),
                                            title = h3(strong("MPI Methodology")),
                                            width = 12,
                                            em(h4("A brief overview of the Mathematics behind the Multidimensional Poverty Index")), tags$br(),
                                            h5("The aggregate methodology for determining the multidimensional poverty 
       indices proposed by Alkine and Foster in 2011 involve a matrix with \\(n\\) 
       rows and \\(d\\) columns, where \\(n\\) is the number of people within the 
       state and \\(d\\) is the number of dimensions for assessing poverty. There 
       are three main measurements denoted on the \\(M\\) scale: \\(M_{0}, M_{1}\\) and \\(M_{2}\\).
       The A-F method employed in this study contains eight dimensions of poverty. 
       Within each dimension, there are one or two variables that indicate whether 
       or not an individual is deprived in that area. Each variable has a specific
       weight associated with it depending on its contribution to overall poverty
       and how it pertains to rural and urban communities differently. For a given 
       individual, the total number of deprivations are added up and if he or she falls
       above a given threshold, \\(k\\), then that individual is considered poor. 
       Having multiple dimensions of poverty allows us to decompose the original 
       measure into its individual variables to identify which are contributing 
       most to the overal index of poverty."),
                                            tags$br(),
                                            h5("The \\(M_{0}\\) index is known as the Adjusted Headcount Ratio. The simple headcount
       ratio is simply the number of individuals considered to be poor divided by
       the entire population. The \\(M_{0}\\) index adjusts for the multidimensionality
       of the algorithm by multiplying the simple headcount ratio, \\(H\\), by the 
       average deprivation share, \\(A\\). This metric can be thought of as a more
       accurate measure of the simple headcount ratio."),
                                            tags$br(),
                                            h5("The \\(M_{1}\\) index is known as the Adjusted Poverty Gap. This examines the distance
       between the prescribed threshold, \\(k\\), and an individual's true number of 
       deprivations. This helps examine the subset of poor individuals to efficiently
       assess which individuals are the 'poorest' in the country."),
                                            tags$br(),
                                            h5("The \\(M_{2}\\) index is known as the Adjusted Poverty Severity. This is
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
                                            em(h5("\\(d = \\) Number of Dimensions")),
                                            em(h5("\\(v_{d} = \\) Number of variables for a Specific Dimension")),
                                            em(h5("\\(w_{i,j} = \\) Weight of a Specific Variable for a Specific Dimension"))
                                            
                                            
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
                                        ))
                             )),

                 ## Tab maps --------------------------------------------------------------------
                 tabPanel("Mapping MPI", value = "maps",
                          dashboardPage(
                            skin = 'black',
                            dashboardHeader(
                              title = 'Mapping MPI'
                            ),
                            
                            
                            dashboardSidebar(
                              sidebarMenu(
                                menuItem(
                                  "91 District MPI Map",
                                  tabName = '91_Dist'
                                ),
                                menuItem(
                                  "60 District MPI Map",
                                  tabName = '60_Dist'
                                ),
                                menuSubItem("District Rankings",
                                            tabName = "rank_60"
                                            ),
                                menuItem(
                                  "Province MPI Map",
                                  tabName = "Prov"
                                )
                              )
                            ),
                            
                            dashboardBody(tabItems(
                              tabItem(
                                tabName = "91_Dist",
                                # Everything has to be put in a row or column
                                fluidPage(
                                  box(
                                    title = "91 District MPI Map of Zimbabwe",
                                    withSpinner(leafletOutput("Dist_91_MPI_Map")),
                                    width = 8,
                                    height = 500
                                  ),
                                  box(
                                    withMathJax(),
                                    title = "Description",
                                    p("This graphic shows a detailed visualization of Zimbabwe's districts, 
                                      broken up into 91 distinct regions. The standard district model uses 60 districts,
                                      but the 2017 pices data designed specific urban areas within districts. There are 
                                      three layers to this graph: \\(M_{0}\\), \\(M_{1}\\) and \\(M_{2}\\). \\(M_{0}\\)
                                      shows the adjusted headcount ratio designed by Alkire-Foster et al. 2011 and takes
                                      into account all of the dimensions described in the methodology section. \\(M_{1}\\)
                                      is the adjusted poverty gap and is an index to show how far the people considered poor 
                                      are from the poverty line. Lastly, \\(M_{2}\\) is the square of the poverty gap and 
                                      weights people who are farther away from the poverty gap higher. This is a measure of the 
                                      poverty severity. To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      adjust the slider below the graph."),
                                    width = 4
                                  ),
                                  box(
                                    sliderInput("slider_91_MPI", "K-Threshold Value", 1, 9, 3),
                                    width = 6,
                                    footer = slider_caption
                                  ),
                                  box(
                                    radioButtons("UrbRurSelection_MPI_91", "Select Urban/Rural Filter", 
                                                 choiceNames = c("All",
                                                                 "Urban",
                                                                 "Rural"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = urban_rural_caption
                                  ),
                                  box(
                                    radioButtons("SensitivitySelection_91", "Select Which MPI to Use:",
                                                 choiceNames = c("Original MPI",
                                                                 "Education-Adjusted MPI"),
                                                 choiceValues = c(1, 2))
                                    
                                    ),
                                  box(
                                    withMathJax(),
                                    title = strong("Trends"),
                                    width = 12,
                                    h5("\\(M_{0}\\):"),
                                    p("Looking at the original poverty index and focusing on the M0 index, 
                                      we can see that for low k-threshold values, a large portion of the population 
                                      can be considered to be multidimensionally poor. Additionally, urban districts 
                                      and urban households tend to have lower M0 scores as compared to their rural counterparts. 
                                      As we increase the k-threshold values, thereby increasing the criteria to be labelled 
                                      multidimensionally poor, we notice that fewer people across the country can be identified 
                                      as such. Interestingly, whereas the greater Harare and Bulawayo areas have low M0 values for 
                                      low k-thresholds, their M0 values for higher k-thresholds are above the national average, 
                                      implying that while those districts are better on average, some of the most poverty-stricken 
                                      households reside within their bounds (particularly the Epworth district). 	Switching from 
                                      the original poverty index to the Education-adjusted MPI, we can see that, on average, the M0 
                                      scores are higher when adjusting the poverty line for the primary education variable from ‘nobody 
                                      in the household has a primary school education’ to ‘nobody in the household has a secondary school 
                                      education’. This is intuitive, given that more individuals who would have not been considered education-deprived 
                                      in the original M0 (adjusted-headcount) index will be exactly such in the adjusted M0. 
                                      Similar trends exist for the education-adjusted M0 index as was the case for the original M0 index – 
                                      fewer households are considered multidimensionally poor as we increase k, urban districts & households 
                                      tend to have lower M0 scores, and some of the households most vulnerable to multidimensional poverty are 
                                      present around the biggest cities as we increase k values."),
                                    h5("\\(M_{1}\\):"),
                                    p("When focus is placed on the M1 index, a clearer picture of the depth of poverty is formed. If k-threshold values 
                                      are low, poverty throughout much of Zimbabwe can be considered deep as a majority of M1 values exceed the national M1 value. 
                                      Similar to the M0 trends, urban districts tend to have lower M1 values than rural districts, implying the presence of deeper
                                      poverty in rural districts. Although the number of districts portraying deep poverty generally decreases as k-threshold values 
                                      increase, rural districts neighboring Harare, including Bindura, Goromonzi, and Marondera similarly maintain high M1 values as 
                                      k-threshold values increase as does a cluster of districts in the southeastern region of the country.	M1 values when the sensitivity 
                                      analysis is accounted for increase, indicating deeper poverty when changing the level of education from primary school to secondary school. 
                                      As k-threshold values increase, M1 values predictably decrease on average as the criteria of deep poverty becomes more drastic. Outliers of 
                                      this decrease exist in similar areas as the original M1 index as southwestern Zimbabwe maintains relatively high M1 values when compared to 
                                      other regions. Rural M1 values are, on average, higher than urban M1 values with few exceptions in Umguza, Bubi, and Mutaza."),
                                    h5("\\(M_{2}\\):"),
                                    p("A look at M2 values of the original index reveals much of the same. Low k-threshold values render high rates of poverty severity across a 
                                      large proportion of Zimbabwe’s population. As k-threshold values increase, M2 values fall throughout most of the country but remain substantially 
                                      high in the western portion of the country as well as around Harare, implying a greater number of impoverished households are further away from the 
                                      poverty line than other impoverished households in these regions. If we distinguish between urban and rural, we can see that urban districts tend 
                                      to have less severe poverty than rural districts excluding the urban aggregates in Umguza, Bubi, and Mutasa. 	As is the case with the M0 and M1 indexes, 
                                      a look at the Education-adjusted MPI shows an increase in M2 values across the board. It is reasonable to conclude that the addition of the population that 
                                      has not completed secondary school to the education variable is significant as it consistently results in higher MPI values. In regard to the overall sensitivity 
                                      analysis, this means an expanded education threshold captures a significant part of the population deprived in the education dimension as M0, M1, and M2 values 
                                      all increase substantially. Interestingly, when selected for urban districts, the cities of Harare and Bulawayo show relatively high M2 values as the k-threshold 
                                      value increases, raising the possibility of disproportionate education deprivation in the two major urban landscapes of Zimbabwe.")
                                     )
                                  )),
                              tabItem(
                                tabName = "60_Dist",
                                fluidPage(
                                  box(
                                    title = "60 District MPI Map of Zimbabwe",
                                    withSpinner(leafletOutput("Dist_60_MPI_Map")),
                                    width = 8,
                                    height = 500
                                  ),
                                  box(
                                    withMathJax(),
                                    title = "Description",
                                    p("This graphic shows a detailed visualization of Zimbabwe's districts, 
                                      broken up into 91 distinct regions. The standard district model uses 60 districts,
                                      but the 2017 pices data designed specific urban areas within districts. There are 
                                      three layers to this graph: \\(M_{0}\\), \\(M_{1}\\) and \\(M_{2}\\). \\(M_{0}\\)
                                      shows the adjusted headcount ratio designed by Alkire-Foster et al. 2011 and takes
                                      into account all of the dimensions described in the methodology section. \\(M_{1}\\)
                                      is the adjusted poverty gap and is an index to show how far the people considered poor 
                                      are from the poverty line. Lastly, \\(M_{2}\\) is the square of the poverty gap and 
                                      weights people who are farther away from the poverty gap higher. This is a measure of the 
                                      poverty severity. To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      adjust the slider below the graph."),
                                    width = 4
                                  ),
                                  box(
                                    sliderInput("slider_60_MPI", "K-Threshold Value", 1, 9, 3),
                                    width = 6,
                                    footer = slider_caption
                                  ),
                                  box(
                                    radioButtons("UrbRurSelection_MPI_60", "Select Urban/Rural Filter", 
                                                 choiceNames = c("All",
                                                                 "Urban",
                                                                 "Rural"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = urban_rural_caption
                                  ),
                                  box(
                                    radioButtons("SensitivitySelection_60", "Select Which MPI to Use:",
                                                 choiceNames = c("Original MPI",
                                                                 "Education-Adjusted MPI"),
                                                 choiceValues = c(1, 2))
                                    
                                  ),
                                  box(
                                    withMathJax(),
                                    title = strong("Trends"),
                                    width = 12,
                                    h5("\\(M_{0}\\):"),
                                    p("When viewing M0 values at the 60-district level, it is clear that a majority of Zimbabwe’s 
                                      districts can be categorized as multidimensionally poor as most exceed the national M0 value. As 
                                      k-threshold values increase, making the criteria for poverty more severe, most districts exhibit 
                                      very low M0 values while a cluster of districts (Lupane, Nkayi, Tsholotsho, Bulilima, Mangwe, and 
                                      Matobo) in the western part of the country maintain high M0 values. The region northeast of Harare 
                                      holds similar M0 values at a high k-threshold. This trend indicates high rural poverty incidence two 
                                      intersections, one between Matabeleland North and Matabeleland South, the other between Mashonaland 
                                      Central and Mashonaland East. When defined by aggregated urban households, most districts fall below the 
                                      national M0 value, indicating less poverty incidence in urban areas than in their rural counterparts as 
                                      well as the nation at large. Looking at the M0 measure when MPI is adjusted for the sensitivity analysis, 
                                      we can see district M0 values increase consistently throughout Zimbabwe. As k-threshold values increase, 
                                      the majority of districts move out of multidimensional poverty. The southwest region and the northeast region 
                                      surrounding Harare are exceptions. The districts of Bindura and Marondera are hold especially high M0 values at 
                                      high k-thresholds, signaling a exceedingly high rate of poverty incidence. When the impact of urban and rural 
                                      households on M0 is compared, it becomes clear that the trend of disproportionate poverty incidence in the rural 
                                      regions continues."),
                                    h5("\\(M_{1}\\):"),
                                    p("When we look at M1 values as they exist at the 60-district level, we can see that a majority of Zimbabwe 
                                      exhibits deep multidimensional poverty as a many of the districts have M1 values higher than the national average. 
                                      As k-threshold values increase, M1 values tend to decrease, although outliers can be found in the northwestern region 
                                      of the country as well as the region surrounding Harare. Distinguishing between urban and rural reveals that urban areas 
                                      tend to have more shallow poverty than the national average as well as their rural counterparts. Poverty depth, M1, 
                                      seems to reflect poverty incidence, M0 in both urban and rural areas as those with high M0 values have higher M1 values 
                                      and those with low M0 values have low M1 values. Overall, M1 value when adjusted for a more encompassing education dimension 
                                      reveals higher m1 values and, thus, deeper levels of poverty than the original dimension. As k-threshold values increase, 
                                      the number of households that exist far from the poverty line decrease as evidenced by low M1 values in a majority of Zimbabwe’s 
                                      districts. The urban and rural distinction brings to light another continuing trend as most rural districts have larger M1 values 
                                      than their urban counterparts."),
                                    h5("\\(M_{2}\\):"),
                                    p("Poverty severity, reflected by the M2 value, indicates moderate poverty severity throughout Zimbabwe that tends to decrease as k-threshold 
                                    values increase. Urban areas consistently have lower M2 values and thus less severe poverty than the national average as well as rural areas. 
                                    This said, poverty severity remains a problem in both Bulawayo and Harare. Rural areas maintain an opposite trend and consistently have M2 values higher 
                                    than urban areas and the nation, revealing the presence of relatively severe multidimensional poverty in northwest regions. The severity of poverty throughout 
                                    Zimbabwe slightly increases as the education dimension is expanded to account for secondary schooling, potentially indicating only a small amount of households 
                                    greatly deprived from in this variable. When urban households are selected at high k-threshold values, Harare and Bulawayo become the only two districts with M2 
                                    values not equal to zero, implying greater severity due to lack of secondary education in these districts than elsewhere. Rural districts consistently exhibit
                                    higher M2 values than urban districts and have more variation in M2 value when k-threshold values are adjusted. ")
                                  )
                                )
                              ),
                              tabItem(tabName = "rank_60",
                                     tabsetPanel(
                                       tabPanel("M0",
                                                fluidRow(
                                                      plotlyOutput("M0_ranking", height = 750),
                                                  box(sliderInput("M0_k_threshold", "K-Threshold Value", 1, 9, 3),
                                                      width = 6,
                                                      footer = slider_caption))),
     
                                       tabPanel("M1",
                                                fluidRow(
                                                    plotlyOutput("M1_ranking", height = 750)),
                                                box(sliderInput("M1_k_threshold", "K-Threshold Value", 1, 9, 3),
                                                    width = 6,
                                                    footer = slider_caption)),
                                       tabPanel("M2",
                                                fluidRow(
                                                    plotlyOutput("M2_ranking", height = 750)),
                                                box(sliderInput("M1_k_threshold", "K-Threshold Value", 1, 9, 3),
                                                    width = 6,
                                                    footer = slider_caption))
                                                )),
                               
                              tabItem(
                                tabName = "Prov",
                                fluidPage(
                                  box(
                                    title = "Province-Level MPI Map of Zimbabwe",
                                    withSpinner(leafletOutput("Prov_MPI_Map")),
                                    width = 8,
                                    height = 500
                                  ),
                                  box(
                                    withMathJax(),
                                    title = "Description",
                                    p("This graphic shows a detailed visualization of Zimbabwe's districts, 
                                      broken up into 91 distinct regions. The standard district model uses 60 districts,
                                      but the 2017 pices data designed specific urban areas within districts. There are 
                                      three layers to this graph: \\(M_{0}\\), \\(M_{1}\\) and \\(M_{2}\\). \\(M_{0}\\)
                                      shows the adjusted headcount ratio designed by Alkire-Foster et al. 2011 and takes
                                      into account all of the dimensions described in the methodology section. \\(M_{1}\\)
                                      is the adjusted poverty gap and is an index to show how far the people considered poor 
                                      are from the poverty line. Lastly, \\(M_{2}\\) is the square of the poverty gap and 
                                      weights people who are farther away from the poverty gap higher. This is a measure of the 
                                      poverty severity. To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      adjust the slider below the graph."),
                                    width = 4
                                  ),
                                  box(
                                    sliderInput("slider_10_MPI", "K-Threshold Value", 1, 9, 3),
                                    width = 6,
                                    footer = slider_caption
                                  ),
                                  box(
                                    radioButtons("UrbRurSelection_MPI_10", "Select Urban/Rural Filter", 
                                                 choiceNames = c("All",
                                                                 "Urban",
                                                                 "Rural"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = urban_rural_caption
                                  ),
                                  box(
                                    radioButtons("SensitivitySelection_10", "Select Which MPI to Use:",
                                                 choiceNames = c("Original MPI",
                                                                 "Education-Adjusted MPI"),
                                                 choiceValues = c(1, 2))
                                    
                                  ),
                                  box(
                                    withMathJax(),
                                    title = strong("Trends"),
                                    width = 12,
                                    h5("\\(M_{0}\\):"),
                                    p("High province level poverty incidence appears to be widespread throughout Zimbabwe as every province, 
                                      excluding Mashonaland West and the city provinces of Bulawayo and Harare, have M0 values higher than the 
                                      national M0 value. As k-threshold value increases, province level M0 values decrease. Interestingly, Harare’s 
                                      M0 value exceeds those of the other provinces at high k-threshold, marking a persistent poverty incidence that 
                                      meets drastic criteria in the capital city. When urban households are drawn out, M0 values decrease throughout 
                                      all provinces, especially Masvingo and Mashonaland West. A switch to rural households results in increased M0 values, 
                                      and therefore increased poverty incidence, across the board. When the education adjustment is made, provinces reflect 
                                      substantially higher M0 values in accordance with ongoing trends. Mashonaland Central, Mashonaland East, Matabeleland North, 
                                      and Matabeleland South are especially impacted by this adjustment and show M0 values nearing 0.5. Increases of k-threshold 
                                      show decreased M0 values throughout although Harare and Mashonaland East have relatively high values. The urban and rural 
                                      split reveals disparity among rural households when compared to their urban counterparts. "),
                                    h5("\\(M_{1}\\):"),
                                    p(""),
                                    h5("\\(M_{2}\\):"),
                                    p("")
                                  )
                                )
                              )
                            )))),
                 
                 tabPanel("MPI Decomposition", value = "decomposition",
                          dashboardPage(
                            skin = 'blue',
                            dashboardHeader(
                              title = 'MPI Decomposition'
                            ),
                            
                            
                            dashboardSidebar(
                              sidebarMenu(
                                menuItem(
                                  "91 District MPI Map",
                                  tabName = '91_Decomp'
                                ),
                                menuItem(
                                  "60 District MPI Map",
                                  tabName = '60_Decomp'
                                ),
                                menuItem(
                                  "Province MPI Map",
                                  tabName = "Prov_Decomp"
                                )
                              )
                            ),
                            
                            dashboardBody(tabItems(
                              tabItem(
                                tabName = "91_Decomp",
                                # Everything has to be put in a row or column
                                fluidPage(
                                  box(
                                    title = "91 District Decomposition Map of Zimbabwe",
                                    withSpinner(leafletOutput("Dist_91_Decomp_Map")),
                                    width = 8,
                                    height = 500
                                  ),
                                  box(
                                    withMathJax(),
                                    title = "Description",
                                    p("This graphic shows a detailed visualization of Zimbabwe's districts, 
                                      broken up into 91 distinct regions. The standard district model uses 60 districts,
                                      but the 2017 pices data designed specific urban areas within districts. There are 
                                      three layers to this graph: \\(M_{0}\\), \\(M_{1}\\) and \\(M_{2}\\). \\(M_{0}\\)
                                      shows the adjusted headcount ratio designed by Alkire-Foster et al. 2011 and takes
                                      into account all of the dimensions described in the methodology section. \\(M_{1}\\)
                                      is the adjusted poverty gap and is an index to show how far the people considered poor 
                                      are from the poverty line. Lastly, \\(M_{2}\\) is the square of the poverty gap and 
                                      weights people who are farther away from the poverty gap higher. This is a measure of the 
                                      poverty severity. To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      adjust the slider below the graph."),
                                    width = 4,
                                    height = 500
                                  ),
                                  box(
                                    sliderInput("slider_91_Decomp", "K-Threshold Value", 1, 9, 3),
                                    footer = slider_caption
                                  ),
                                  box(
                                    radioButtons("UrbRurSelection_Decomp_91", "Select Urban/Rural Filter", 
                                                 choiceNames = c("All",
                                                                 "Urban",
                                                                 "Rural"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = urban_rural_caption
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("LevelSelection_Decomp_91", "Select Poverty Index to Examine", 
                                                 choiceNames = c("Adj. Headcount Ratio \\(M_{0}\\)",
                                                                 "Adj. Poverty Gap \\(M_{1}\\)",
                                                                 "Adj. Poverty Severity \\(M_{2}\\)"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = level_caption
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("c_g_Decomp_91", "Select Method to Examine", 
                                                 choiceNames = c("Percent Contribution to MPI",
                                                                 "Raw Poverty Gap in Variable"),
                                                 choiceValues = c(1, 2)),
                                    footer = c_g_caption
                                  )
                                  )),
                              tabItem(
                                tabName = "60_Decomp",
                                # Everything has to be put in a row or column
                                fluidPage(
                                  box(
                                    title = "60 District Decomposition Map of Zimbabwe",
                                    withSpinner(leafletOutput("Dist_60_Decomp_Map")),
                                    width = 8,
                                    height = 500
                                  ),
                                  box(
                                    withMathJax(),
                                    title = "Description",
                                    p("This graphic shows a detailed visualization of Zimbabwe's districts, 
                                      broken up into 91 distinct regions. The standard district model uses 60 districts,
                                      but the 2017 pices data designed specific urban areas within districts. There are 
                                      three layers to this graph: \\(M_{0}\\), \\(M_{1}\\) and \\(M_{2}\\). \\(M_{0}\\)
                                      shows the adjusted headcount ratio designed by Alkire-Foster et al. 2011 and takes
                                      into account all of the dimensions described in the methodology section. \\(M_{1}\\)
                                      is the adjusted poverty gap and is an index to show how far the people considered poor 
                                      are from the poverty line. Lastly, \\(M_{2}\\) is the square of the poverty gap and 
                                      weights people who are farther away from the poverty gap higher. This is a measure of the 
                                      poverty severity. To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      adjust the slider below the graph."),
                                    width = 4,
                                    height = 500
                                  ),
                                  box(
                                    sliderInput("slider_60_Decomp", "K-Threshold Value", 1, 9, 3),
                                    footer = slider_caption
                                  ),
                                  box(
                                    radioButtons("UrbRurSelection_Decomp_60", "Select Urban/Rural Filter", 
                                                 choiceNames = c("All",
                                                                 "Urban",
                                                                 "Rural"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = urban_rural_caption
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("LevelSelection_Decomp_60", "Select Poverty Index to Examine", 
                                                 choiceNames = c("Adj. Headcount Ratio \\(M_{0}\\)",
                                                                 "Adj. Poverty Gap \\(M_{1}\\)",
                                                                 "Adj. Poverty Severity \\(M_{2}\\)"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = level_caption
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("c_g_Decomp_60", "Select Method to Examine", 
                                                 choiceNames = c("Percent Contribution to MPI",
                                                                 "Raw Poverty Gap in Variable"),
                                                 choiceValues = c(1, 2)),
                                    footer = c_g_caption
                                  )
                                )),
                              tabItem(
                                tabName = "Prov_Decomp",
                                # Everything has to be put in a row or column
                                fluidPage(
                                  box(
                                    title = "Province Decomposition Map of Zimbabwe",
                                    withSpinner(leafletOutput("Prov_Decomp_Map")),
                                    width = 8,
                                    height = 500
                                  ),
                                  box(
                                    withMathJax(),
                                    title = "Description",
                                    p("This graphic shows a detailed visualization of Zimbabwe's districts, 
                                      broken up into 91 distinct regions. The standard district model uses 60 districts,
                                      but the 2017 pices data designed specific urban areas within districts. There are 
                                      three layers to this graph: \\(M_{0}\\), \\(M_{1}\\) and \\(M_{2}\\). \\(M_{0}\\)
                                      shows the adjusted headcount ratio designed by Alkire-Foster et al. 2011 and takes
                                      into account all of the dimensions described in the methodology section. \\(M_{1}\\)
                                      is the adjusted poverty gap and is an index to show how far the people considered poor 
                                      are from the poverty line. Lastly, \\(M_{2}\\) is the square of the poverty gap and 
                                      weights people who are farther away from the poverty gap higher. This is a measure of the 
                                      poverty severity. To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      adjust the slider below the graph."),
                                    width = 4,
                                    height = 500
                                  ),
                                  box(
                                    sliderInput("slider_Prov_Decomp", "K-Threshold Value", 1, 9, 3),
                                    footer = slider_caption
                                  ),
                                  box(
                                    radioButtons("UrbRurSelection_Decomp_Prov", "Select Urban/Rural Filter", 
                                                 choiceNames = c("All",
                                                                 "Urban",
                                                                 "Rural"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = urban_rural_caption
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("LevelSelection_Decomp_Prov", "Select Poverty Index to Examine", 
                                                 choiceNames = c("Adj. Headcount Ratio \\(M_{0}\\)",
                                                                 "Adj. Poverty Gap \\(M_{1}\\)",
                                                                 "Adj. Poverty Severity \\(M_{2}\\)"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = level_caption
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("c_g_Decomp_Prov", "Select Method to Examine", 
                                                 choiceNames = c("Percent Contribution to MPI",
                                                                 "Raw Poverty Gap in Variable"),
                                                 choiceValues = c(1, 2)),
                                    footer = c_g_caption
                                  )
                                ))
                              
                            ))
                            )
                          ),
                ## Tab Temporal Comparison--------------------------------------
                tabPanel("2011 to 2017 Data", value = "Comparison",
                         dashboardPage(
                           skin = 'blue',
                           dashboardHeader(
                             title = 'MPI Decomposition'
                           ),
                           
                           
                           dashboardSidebar(
                             sidebarMenu(
                               menuItem(
                                 "M0 Comparison Map",
                                 tabName = 'M0_Comp'
                               ),
                               menuItem(
                                 "M1 Comparison",
                                 tabName = "M1_Comp"
                               ),
                               menuItem(
                                 "M2 Comparison Map",
                                 tabName = 'M2_Comp'
                               )
                             )
                           ),
                           
                           dashboardBody(
                             tabItems(
                               tabItem(
                                 tabName = "M0_Comp",
                                 # Everything has to be put in a row or column
                                 fluidPage(
                                   box(
                                     title = "Comparison of M0 in Zimbabwe",
                                     withSpinner(leafletOutput("M0_Comparison_Map")),
                                     width = 6,
                                     height = 500
                                   ),
                                   box(
                                     withMathJax(),
                                     title = "Scatterplot",
                                     #withSpinner(plotlyOutput("M0_Scatterplot"))
                                     withSpinner(plotlyOutput("M0_Scatterplot")),
                                     width = 6,
                                     height = 500
                                   ),
                                   box(
                                     sliderInput("slider_M0_Comparison", "K-Threshold Value", 1, 9, 3),
                                     footer = slider_caption
                                   ),
                                   box(
                                     radioButtons("UrbRurSelection_M0", "Select Urban/Rural Filter", 
                                                  choiceNames = c("All",
                                                                  "Urban",
                                                                  "Rural"),
                                                  choiceValues = c(1, 2, 3)),
                                     footer = urban_rural_caption
                                   ),
                                   box(
                                     radioButtons("RegionSelection_M0", "Select Boundaries to Display", 
                                                  choiceNames = c("Districts",
                                                                  "Provinces"),
                                                  choiceValues = c(1, 2)),
                                     footer = urban_rural_caption
                                   )
                                 )
                               )
                             ,
                               tabItem(
                                 tabName = "M1_Comp",
                                 # Everything has to be put in a row or column
                                 fluidPage(
                                   box(
                                     title = "Comparison of M1 in Zimbabwe",
                                     withSpinner(leafletOutput("M1_Comparison_Map")),
                                     width = 6,
                                     height = 500
                                   ),
                                   box(
                                     withMathJax(),
                                     title = "Scatterplot",
                                     withSpinner(plotlyOutput("M1_Scatterplot")),
                                     width = 6,
                                     height = 500
                                   ),
                                   box(
                                     sliderInput("slider_M1_Comparison", "K-Threshold Value", 1, 9, 3),
                                     footer = slider_caption
                                   ),
                                   box(
                                     radioButtons("UrbRurSelection_M1", "Select Urban/Rural Filter", 
                                                  choiceNames = c("All",
                                                                  "Urban",
                                                                  "Rural"),
                                                  choiceValues = c(1, 2, 3)),
                                     footer = urban_rural_caption
                                   ),
                                   box(
                                     radioButtons("RegionSelection_M1", "Select Boundaries to Display", 
                                                  choiceNames = c("Districts",
                                                                  "Provinces"),
                                                  choiceValues = c(1, 2)),
                                     footer = urban_rural_caption
                                   )
                                 )
                               ),
                             tabItem(
                               tabName = "M2_Comp",
                               # Everything has to be put in a row or column
                               fluidPage(
                                 box(
                                   title = "Comparison of M2 in Zimbabwe",
                                   withSpinner(leafletOutput("M2_Comparison_Map")),
                                   width = 6,
                                   height = 500
                                 ),
                                 box(
                                   withMathJax(),
                                   title = "Scatterplot",
                                   withSpinner(plotlyOutput("M2_Scatterplot")),
                                   width = 6,
                                   height = 500
                                 ),
                                 box(
                                   sliderInput("slider_M2_Comparison", "K-Threshold Value", 1, 9, 3),
                                   footer = slider_caption
                                 ),
                                 box(
                                   radioButtons("UrbRurSelection_M2", "Select Urban/Rural Filter", 
                                                choiceNames = c("All",
                                                                "Urban",
                                                                "Rural"),
                                                choiceValues = c(1, 2, 3)),
                                   footer = urban_rural_caption
                                 ),
                                 box(
                                   radioButtons("RegionSelection_M2", "Select Boundaries to Display", 
                                                choiceNames = c("Districts",
                                                                "Provinces"),
                                                choiceValues = c(1, 2)),
                                   footer = urban_rural_caption
                                 )
                               )
                             )
                             )
                           )
                         )
                         ),
                                   

                  
        

                ## Tab DSPG Team------------------------------------------------
                tabPanel("Our Team", value = "team",
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
                                   fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                            column(6, align = "center",
                                                   h4(strong("DSPG Team Members")),
                                                   img(src = "team-yang.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                                   img(src = "team-sambath.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                                   img(src = "team-atticus.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                                   img(src = "team-matt.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                                   p(a(href = 'https://www.linkedin.com/in/yang-cheng-200118191/', 'Yang Cheng', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                                     p(a(href = 'https://www.linkedin.com/in/sambath-jayapregasham-097803127/', 'Sambath Jayapregasham', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                                       p(a(href = 'https://www.linkedin.com/in/atticus-rex-717581191/', 'Atticus Rex', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics)"),
                                                         p( a(href = 'https://www.linkedin.com/in/matthew-burkholder-297b9119a/', 'Matthew Burkholder', target = '_blank'), "(Virginia Tech, Philosophy, Politics, & Economics)"),
                                                   p("", style = "padding-top:10px;")
                                                            
                                            ),
                                            column(6, align = "center",
                                                   h4(strong("Virginia Tech Faculty Members")),
                                                   p("", style = "padding-top:10px;")
                                                   # img(src = "team-teja.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                                   # img(src = "team-brandon.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                                   # p(a(href = "https://aaec.vt.edu/people/faculty/chen-susan.html", 'Dr. Susan Chen', target = '_blank'), "(Project Lead, Research Assistant Professor);",
                                                   #   a(href = "https://aaec.vt.edu/people/faculty/gupta-anubhab.html", 'Dr. Anubhab Gupta', target = '_blank'), "(Postdoctoral Research Associate);",
                                                   # p("", style = "padding-top:10px;")
                                            )
                                   ),
                                   fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                            h4(strong("Project Stakeholders"), align = "center"),
                                            p("Grown Chirongwe (Zimstat)"),
                                            p("Dhiraj (World Bank")

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
      addLegend(pal = pal, values = c(0, max(M0, na.rm = TRUE)), opacity = 0.7, title = paste0("Index with k = ", input$slider_91_MPI),
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
      addLegend(pal = pal, values = c(0, max(M0, na.rm = TRUE)), opacity = 0.7, title = paste0("Index with k = ", input$slider_60_MPI),
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

# Graphing Ranked District Bar Chart --------------------------------------

    ranked_data <- read_csv("data/MappingData/OriginalMPI/2017/2017_District.csv")
    
    M0_dist_rank <- reactive({
      input$M0_k_threshold
    })
    
    output$M0_ranking <- renderPlotly({
      if (M0_dist_rank() == "1") {
        M0_k1_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M0_k1)) %>% 
          ggplot(aes(x = District_name, y = M0_k1)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
          coord_flip() +
          labs(y = "M0 at Threshold K = 1", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M0_k1_ranking)
        
      }
      
      else if (M0_dist_rank() == "2") {
        M0_k2_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M0_k2)) %>% 
          ggplot(aes(x = District_name, y = M0_k2)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M0 at Threshold K = 2", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M0_k2_ranking)
        
      }
      
      else if (M0_dist_rank() == "3") {
        M0_k3_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M0_k3)) %>% 
          ggplot(aes(x = District_name, y = M0_k3)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M0 at Threshold K = 3", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M0_k3_ranking)
        
      }
      
      else if (M0_dist_rank() == "4") {
        M0_k4_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M0_k4)) %>% 
          ggplot(aes(x = District_name, y = M0_k4)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M0 at Threshold K = 4", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M0_k4_ranking)
        
      }
      
      else if (M0_dist_rank() == "5") {
        M0_k5_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M0_k5)) %>% 
          ggplot(aes(x = District_name, y = M0_k5)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M0 at Threshold K = 5", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M0_k5_ranking)
        
      }
      
      else if (M0_dist_rank() == "6") {
        M0_k6_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M0_k6)) %>% 
          ggplot(aes(x = District_name, y = M0_k6)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M0 at Threshold K = 6", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M0_k6_ranking)
        
      }
      
      else if (M0_dist_rank() == "7") {
        M0_k7_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M0_k7)) %>% 
          ggplot(aes(x = District_name, y = M0_k7)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M0 at Threshold K = 7", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M0_k7_ranking)
        
      }
      
      else if (M0_dist_rank() == "8") {
        M0_k8_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M0_k8)) %>% 
          ggplot(aes(x = District_name, y = M0_k8)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M0 at Threshold K = 8", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M0_k8_ranking)
        
      }
      
      else if (M0_dist_rank() == "9") {
        
        M0_k9_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M0_k9)) %>% 
          ggplot(aes(x = District_name, y = M0_k9)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M0 at Threshold K = 9", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M0_k9_ranking)
        
      }
      
      
    })
    
    M1_k_threshold <- reactive({
      input$M1_k_threshold
    })
    
    output$M1_ranking <- renderPlotly({
      if (M1_k_threshold() == "1") {
        M1_k1_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M1_k1)) %>% 
          ggplot(aes(x = District_name, y = M1_k1)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M1 at Threshold K = 1", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M1_k1_ranking)
        
      }
      
      else if (M1_k_threshold() == "2") {
        M1_k2_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M1_k2)) %>% 
          ggplot(aes(x = District_name, y = M1_k2)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M1 at Threshold K = 2", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M1_k2_ranking)
        
      }
      
      else if (M1_k_threshold() == "3") {
        M1_k3_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M1_k3)) %>% 
          ggplot(aes(x = District_name, y = M1_k3)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M1 at Threshold K = 3", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M1_k3_ranking)
        
      }
      
      else if (M1_k_threshold() == "4") {
        M1_k4_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M1_k4)) %>% 
          ggplot(aes(x = District_name, y = M1_k4)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M1 at Threshold K = 4", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M1_k4_ranking)
        
      }
      
      else if (M1_k_threshold() == "5") {
        M1_k5_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M1_k5)) %>% 
          ggplot(aes(x = District_name, y = M1_k5)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M1 at Threshold K = 5", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M1_k5_ranking)
        
      }
      
      else if (M1_k_threshold() == "6") {
        M1_k6_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M1_k6)) %>% 
          ggplot(aes(x = District_name, y = M1_k6)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M1 at Threshold K = 6", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M1_k6_ranking)
        
      }
      
      else if (M1_k_threshold() == "7") {
        M1_k7_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M1_k7)) %>% 
          ggplot(aes(x = District_name, y = M1_k7)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M1 at Threshold K = 7", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M1_k7_ranking)
        
      }
      
      else if (M1_k_threshold() == "8") {
        M1_k8_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M1_k8)) %>% 
          ggplot(aes(x = District_name, y = M1_k8)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M1 at Threshold K = 8", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M1_k8_ranking)
        
      }
      
      else if (M1_k_threshold() == "9") {
        
        M1_k9_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M1_k9)) %>% 
          ggplot(aes(x = District_name, y = M1_k9)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M1 at Threshold K = 9", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M1_k9_ranking)
        
      }
      
      
    })
    
    M2_k_threshold <- reactive({
      input$M2_k_threshold
    })
    
    output$M2_ranking <- renderPlotly({
      if (M2_k_threshold() == "1") {
        M2_k1_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M2_k1)) %>% 
          ggplot(aes(x = District_name, y = M2_k1)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M2 at Threshold K = 1", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M2_k1_ranking)
        
      }
      
      else if (M2_k_threshold() == "2") {
        M2_k2_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M2_k2)) %>% 
          ggplot(aes(x = District_name, y = M2_k2)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M2 at Threshold K = 2", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M2_k2_ranking)
        
      }
      
      else if (M2_k_threshold() == "3") {
        M2_k3_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M2_k3)) %>% 
          ggplot(aes(x = District_name, y = M2_k3)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M2 at Threshold K = 3", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M2_k3_ranking)
        
      }
      
      else if (M2_k_threshold() == "4") {
        M2_k4_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M2_k4)) %>% 
          ggplot(aes(x = District_name, y = M2_k4)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M2 at Threshold K = 4", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M2_k4_ranking)
        
      }
      
      else if (M2_k_threshold() == "5") {
        M2_k5_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M2_k5)) %>% 
          ggplot(aes(x = District_name, y = M2_k5)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M2 at Threshold K = 5", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M2_k5_ranking)
        
      }
      
      else if (M2_k_threshold() == "6") {
        M2_k6_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M2_k6)) %>% 
          ggplot(aes(x = District_name, y = M2_k6)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M2 at Threshold K = 6", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M2_k6_ranking)
        
      }
      
      else if (M2_k_threshold() == "7") {
        M2_k7_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M2_k7)) %>% 
          ggplot(aes(x = District_name, y = M2_k7)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M2 at Threshold K = 7", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M2_k7_ranking)
        
      }
      
      else if (M2_k_threshold() == "8") {
        M2_k8_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M2_k8)) %>% 
          ggplot(aes(x = District_name, y = M2_k8)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M2 at Threshold K = 8", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M2_k8_ranking)
        
      }
      
      else if (M2_k_threshold() == "9") {
        
        M2_k9_ranking <- ranked_data %>% 
          mutate(District_name = fct_reorder(District_name, M2_k9)) %>% 
          ggplot(aes(x = District_name, y = M2_k9)) +
          geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
          coord_flip() +
          labs(y = "M2 at Threshold K = 9", x = "Districts", title = "District Comparison") +
          theme_minimal()
        
        ggplotly(M2_k9_ranking)
        
      }
      
      
    })
    
    ## MAPPING MPI 2017 90 district ----------------------------------------------------------------------
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
      addLegend(pal = pal, values = c(0, max(M0, na.rm = TRUE)), opacity = 0.7, title = paste0("Index with k = ", input$slider_Prov_MPI),
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
        options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend(pal = pal, values = c(0, 0.6), opacity = 0.7, title = paste0("Legend (with k = ", input$slider_91_Decomp, ")"),
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
        options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend(pal = pal, values = c(0, 0.6), opacity = 0.7, title = paste0("Legend (with k = ", input$slider_60_Decomp, ")"),
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
        options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend(pal = pal, values = c(0, 0.6), opacity = 0.7, title = paste0("Legend (with k = ", input$slider_Prov_Decomp, ")"),
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
      
      addLegend(pal = pal, values = M0_change, opacity = 0.7, title = paste0("Index with k = ", k_threshold),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
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
    
    create_scatter(names, M0_2011, M0_2017, "M0 for 2011", "M0 for 2017", "Comparison of M0 from 2011 to 2017") 
  })
  
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
    
    create_scatter(names, M1_2011, M1_2017, "M1 for 2011", "M1 for 2017", "Comparison of M1 from 2011 to 2017") 
  })
  
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
      
      addLegend(pal = pal, values = M1_change, opacity = 0.7, title = paste0("Index with k = ", k_threshold),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
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
    
    create_scatter(names, M2_2011, M2_2017, "M2 for 2011", "M2 for 2017", "Comparison of M2 from 2011 to 2017")
  }) 
  
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
    <strong>" , "M<sub>1</sub> Change" , ":</strong> %g<br/>"),
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
        minZoom = 0, maxZoom= 18,
        drag = FALSE)) %>% addTiles() %>%
      setView(lng = 30, lat=-19, zoom=6) %>% 
      get_polygon(map_2017, pal, M2_change, change_labels, "M2") %>%
      clearControls() %>%
      
      addLegend(pal = pal, values = M2_change, opacity = 0.7, title = paste0("Index with k = ", k_threshold),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  }

# Run the App--------------------------
shinyApp(ui = ui, server = server)