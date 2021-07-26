#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html\">' +
                              '<img src=\"VTDSPG Logo.png\", alt=\"DSPG 2021 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('economic');
           }
           "

## National Data----------------------------------------------------------------

# Loads the national data
National_2017 = read.csv(file = "MappingData/OriginalMPI/2017/2017_National.csv")
National_Urban_2017 = read.csv(file = "MappingData/OriginalMPI/2017/2017_National_Urban.csv")
National_Rural_2017 = read.csv(file = "MappingData/OriginalMPI/2017/2017_National_Rural.csv")
# Data--------------------------------------------------------------------------
## 91 District DATA LOADING-----------------------------------------------------------------

## 91 District data-------------------------------------------------------------

## Loading the shapefile
Dist_91_Map <-  readOGR(dsn = paste0(getwd(),"/Shapefiles/91DistrictShapefiles"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")

# Loading the MPI data at the district level
Dist_91_Total_2017 = read.csv(file = 'MappingData/OriginalMPI/2017/2017_91_District.csv')
Dist_91_Urban_2017 = read.csv(file = 'MappingData/OriginalMPI/2017/2017_91_District_Urban.csv')
Dist_91_Rural_2017 = read.csv(file = 'MappingData/OriginalMPI/2017/2017_91_District_Rural.csv')

## Loading the MPI data at the district level
Dist_91_MPI = read.csv(file = 'MappingData/2017_91_District.csv')
National_2017 = read.csv(file = 'MappingData/2017_National.csv')

## NAME FIXING
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

# MERGING DATA

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

## 60 District data-------------------------------------------------------------

# Loads the shapefile
Dist_60_Map <- readOGR(dsn = paste0(getwd(),"/Shapefiles/60DistrictShapefiles"), layer="gadm36_ZWE_2")

# Loads the district data
Dist_60_Total_2017 = read.csv("MappingData/OriginalMPI/2017/2017_District.csv")
Dist_60_Urban_2017 = read.csv("MappingData/OriginalMPI/2017/2017_District_Urban.csv")
Dist_60_Rural_2017 = read.csv("MappingData/OriginalMPI/2017/2017_District_Rural.csv")


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

## Province Data----------------------------------------------------------------

# Loads the shapefile
Prov_Map <- readOGR(dsn = paste0(getwd(),"/Shapefiles/ProvinceShapefiles"), layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# Loads the district data
Prov_Total_2017 = read.csv("MappingData/OriginalMPI/2017/2017_Province.csv")
Prov_Urban_2017 = read.csv("MappingData/OriginalMPI/2017/2017_Province_Urban.csv")
Prov_Rural_2017 = read.csv("MappingData/OriginalMPI/2017/2017_Province_Rural.csv")


## CLEANING PROVINCE DATA

# Renames the columns in the data to merge
colnames(Prov_Total_2017)[2] <- "ADM1_EN"
colnames(Prov_Urban_2017)[2] <- "ADM1_EN"
colnames(Prov_Urban_2017)[2] <- "ADM1_EN"

# To avoid overlap in data, three different maps are created to host the rural, 
# urban and total MPI Data and decompositions 
Prov_Total_Map = Prov_Map
Prov_Urban_Map = Prov_Map
Prov_Rural_Map = Prov_Map

# Merges the shapefiles with the data csv files 
Prov_Total_Map@data = merge(Prov_Total_Map@data, Prov_Total_2017, by = c("ADM1_EN"), sort = FALSE)
Prov_Urban_Map@data = merge(Prov_Urban_Map@data, Prov_Urban_2017, by = c("ADM1_EN"), sort = FALSE)
Prov_Rural_Map@data = merge(Prov_Rural_Map@data, Prov_Urban_2017, by = c("ADM1_EN"), sort = FALSE)

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
  label <- sprintf(
    paste0("<strong>%s</strong><br/>
    <strong>" , metric_name , ":</strong> %g<br/>
    <strong>National " , metric_name , ":</strong> %g"),
    name_data, metric, national_metric) %>% lapply(htmltools::HTML)
  return(label)
}

# user -------------------------------------------------------------
ui <- navbarPage(title = "Zimbabwe",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                ## Tab main -----------------------------------------------------------
                 tabPanel("Project Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "VTDSPG Logo.png", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Using 2017 PICES Data to Create a Multidimensional Poverty Index of Zimbabwe"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      h4("Department of Agriculture"),
                                      br()
                                   )
                          ),
                          
                          fluidRow(style = "margin: 2px;",
                                   align = "left",
                                   img(src = "Zimbabwe_Flag.png", height="100", width="200", alt="Image", style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000;")
                          ),
                          
                          fluidRow(style = "margin: 6px;",
                                   column(8,
                                          h2(strong("Introduction to Zimbabwe")),
                                          p("Nestled in the Southeastern tip of Africa, Zimbabwe neighbors South Africa, Mozambique, Zambia, and Botswana. Zimbabwe gained independence from Great Britain in 1980 and was ruled by Prime Minister and eventually President Robert MUGABE until his resignation in 2017. Presently, Emmerson Mnangagwa holds office. 
                                            The country is home to roughly 14,830,000 inhabitants, 10% of whom live in the capital city of Harare. Although large agglomerations exist in other major urban areas including Bulawayo and Chitungwiza, population distribution is relatively even otherwise. Zimbabwe’s central government is responsible for regulating 
                                            its 10 provinces and 59 further subdivided districts. Zimbabwe’s terrain consists mostly of plateau upon which forests thrive and arable land is plenty. Because of this, 67.5% of the labor force works in agriculture growing sugar cane, tobacco, fruits, and vegetables among other things. Another 7.3% of the labor force 
                                            takes advantage of the Zimbabwe’s rich natural resources and participates in the industry sector mining and exporting coal, gold, platinum copper, and other metals as well as manufacturing wood products, cement, chemicals, fertilizer, and food. Despite being relatively well-educated and extremely literate, the population 
                                            suffers from both unemployment and severe underemployment in which individuals are overqualified for the jobs they have or are not given adequate work hours. In combination with ubiquitous low wages, this creates an obstacle for economic growth. Monetary poverty measures in 2017 revealed roughly 63% of Zimbabwean households 
                                            lived in poverty. This is reflected in income inequality, overall low standards of living, malnourishment, low life expectancy, high rates of infant/maternal mortality, and difficulty accessing health and education resources."),

                                 
                                          h2(strong("Recent History")),
                                          p("After gaining independence in 1980, there was widespread hope that the economic and labor exploitation Africans suffered at the hands of an imperial Great Britain would diminish. While initial trends were encouraging, this hope dwindled as a multitude of factors sent the Zimbabwean economy into decline. Most prominent among 
                                            these factors was inconsistent policy put forth by the central government which resulted in vague and evolving strategies on combatting poverty. An initial scientific socialist policy was applied between 1980 and 1990 to address poverty but was ineffective and thus abandoned due to financial downturn and prolonged drought which 
                                            forced agricultural workers into the cities where they faced even greater poverty due to unemployment. In an attempt to revamp the economy, Zimbabwe sought help from the International Monetary Fund (IMF) and the World Bank (WB) which meant an adoption of more capitalistic policy. The costs of necessities including food, water, and 
                                            education went up as a result, harming and expanding the already existing poor population. The late 1990’s and 2000’s brought ever greater poverty and financial distress to Zimbabwe as a continuing government budget deficit mixed with a fiscal policy focused on increasing the amount of money in circulation resulted in hyperinflation. 
                                            In turn, this increased the economic crisis as foreign investment dropped and Zimbabwean currency crashed. During this time, unemployment skyrocketed and a massive informal sector of the economy emerged. In 2009, Zimbabwe adopted the U.S. dollar along with a handful of other currencies. Though this move somewhat stabilized the 
                                            economy at first, a 2013 shift in government rendered these efforts futile. By 2017, inflation increased significantly as did overall economic crisis and poverty."),
                                   
                                
                                          h2(strong("Application of a Multidimensional Poverty Index")),
                                          p("A brief introduction to Zimbabwe makes clear the severity and the urgency of the poverty situation. Although a money metric approach to measuring poverty is historically prevalent, this sort of strategy is unable to accurately paint an accurate picture of poverty in Zimbabwe. This is most notably due to the extreme hyperinflation the 
                                            country suffers from. Because the actual value of money is constantly evolving, the importance of monetary wealth accumulation in determining poverty is questionable. Additionally, variations in consumption tendencies, prices of goods and necessities, and household income distribution can make it difficult to provide an accurate account 
                                            of money metric poverty as the value of money is hardly standardized. This volatility also renders money metric comparisons of poverty over time futile as the modern value of currency is incomparable to that of years past. As the practicality of a monetary poverty measure becomes increasingly suspect, the value of alternative poverty measure 
                                            methods is revealed. "),
                                          p("An Alkire Foster method, developed by Sabina Alkire and James Foster, will be utilized in this project to measure poverty in Zimbabwe. The AF method first denotes the different kinds of deprivations households experience simultaneously. These deprivations make clear who is impoverished in a population and are then used to construct a non-monetary 
                                            Multidimensional Poverty Index (MPI). MPI’s are powerful insofar as they provide a picture of non-monetary poverty as it exists in its various manifestations. In this way, an MPI accounts for the hyperinflation in Zimbabwe by defining poverty as the inability to satisfy a certain list of needs or capabilities rather than the accumulation of money 
                                            that may or may not fulfill such needs. The list, as pictured below, is comprised of variables that indicate deprivation. Each variable corresponds to a broader dimension of poverty. These variables and dimensions are normatively chosen to be applicable in the context of Zimbabwe. The MPI created by the 2021 DSPG Zimbabwe team can be utilized to decompose
                                            multidimensional poverty as it exists in different subgroups including the national, provincial, and district level. Additionally, the MPI can be deconstructed to analyze at what strength each deprivation is contributing to poverty within groups. By emulating the work of Stoeffler, et al., this MPI can also be used to track changes in multifaceted poverty 
                                            over time. The combination of these unique aspects of the MPI allows it to be used not only to accurately measure poverty as it exists today, but to evaluate the effectiveness of policy going forward.")
                                   
                          )),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: July 2021'))))
                 ),

                 ## Tab data and methodology ----------------------------------------------------
                 tabPanel("Data & Methodology", value = "dm",
                             tabsetPanel(
                               tabPanel("Data", value = "data"),
                               tabPanel("Methodology", value = "methodology")
                             )),

                 ## Tab maps --------------------------------------------------------------------
                 tabPanel("Mapping MPI", value = "maps",
                          dashboardPage(
                            skin = 'blue',
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
                                    leafletOutput("Dist_91_MPI_Map"),
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
                                  ))),
                              tabItem(
                                tabName = "60_Dist",
                                fluidPage(
                                  box(
                                    title = "60 District MPI Map of Zimbabwe",
                                    leafletOutput("Dist_60_MPI_Map"),
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
                                  )
                                )
                              ),
                              tabItem(
                                tabName = "Prov",
                                fluidPage(
                                  box(
                                    title = "Province-Level MPI Map of Zimbabwe",
                                    leafletOutput("Prov_MPI_Map"),
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
                                    sliderInput("slider_Prov_MPI", "K-Threshold Value", 1, 9, 3),
                                    width = 6,
                                    footer = slider_caption
                                  ),
                                  box(
                                    radioButtons("UrbRurSelection_MPI_Prov", "Select Urban/Rural Filter", 
                                                 choiceNames = c("All",
                                                                 "Urban",
                                                                 "Rural"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = urban_rural_caption
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
                                    leafletOutput("Dist_91_Decomp_Map"),
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
                                    radioButtons("LevelSelection_Decomp_91", "Select Level to Examine", 
                                                 choiceNames = c("Adj. Headcount Ratio \\(M_{0}\\)",
                                                                 "Adj. Poverty Gap \\(M_{1}\\)",
                                                                 "Adj. Poverty Severity \\(M_{2}\\)"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = level_caption
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("c_g_Decomp_91", "Select Measure to Examine", 
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
                                    leafletOutput("Dist_60_Decomp_Map"),
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
                                    radioButtons("LevelSelection_Decomp_60", "Select Level to Examine", 
                                                 choiceNames = c("Adj. Headcount Ratio \\(M_{0}\\)",
                                                                 "Adj. Poverty Gap \\(M_{1}\\)",
                                                                 "Adj. Poverty Severity \\(M_{2}\\)"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = level_caption
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("c_g_Decomp_60", "Select Measure to Examine", 
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
                                    leafletOutput("Prov_Decomp_Map"),
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
                                    radioButtons("LevelSelection_Decomp_Prov", "Select Level to Examine", 
                                                 choiceNames = c("Adj. Headcount Ratio \\(M_{0}\\)",
                                                                 "Adj. Poverty Gap \\(M_{1}\\)",
                                                                 "Adj. Poverty Severity \\(M_{2}\\)"),
                                                 choiceValues = c(1, 2, 3)),
                                    footer = level_caption
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("c_g_Decomp_Prov", "Select Measure to Examine", 
                                                 choiceNames = c("Percent Contribution to MPI",
                                                                 "Raw Poverty Gap in Variable"),
                                                 choiceValues = c(1, 2)),
                                    footer = c_g_caption
                                  )
                                ))
                              
                            ))
                            )
                          ),
                          tabPanel("DSPG Team", value = "team",
                                   fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                            h1(strong("Zimbabwe Team"), align = "center"),
                                            br(),
                                            h4(strong("UVA Data Science for the Public Good")),
                                            p("The", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"), 
                                              "is a summer immersive program held at the", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics', 'University of Virginia Biocomplexity Institute’s Social and Decision Analytics division (SDAD).'), 
                                              "In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around 
                              critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences 
                              to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program 
                              highlights, how to apply, and our annual symposium, please visit", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'the official Biocomplexity DSPG website.', target = "_blank")),
                                            p("", style = "padding-top:10px;")
                                   ),
                                   fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                            column(6, align = "center",
                                                   h4(strong("DSPG Team Members")),
                                                   img(src = "team-yang.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                                   img(src = "team-sambath.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                                   img(src = "team-atticus.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                                   img(src = "team-matt.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                                   p(a(href = 'https://www.linkedin.com/in/yang-cheng-200118191/', 'Yang Cheng', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);",
                                                     a(href = 'https://www.linkedin.com/in/tasfia-chowdhury-89005a1b2/', 'Tasfia Chowdhury', target = '_blank'), "(Indiana University Bloomington, Political Science);",
                                                     a(href = 'https://www.linkedin.com/in/igomez-3099/', 'Isabel Gomez', target = '_blank'), "(Smith College, Statistical and Data Science)."),
                                                   p("", style = "padding-top:10px;")
                                            ),
                                            column(6, align = "center",
                                                   h4(strong("Virginia Tech Faculty Members")),
                                                   # img(src = "team-teja.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                                   # img(src = "team-brandon.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                                   # img(src = "team-sallie.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                                   # p(a(href = "https://www.linkedin.com/in/tejapristavec/", 'Teja Pristavec', target = '_blank'), "(Project Lead, Research Assistant Professor);",
                                                   #   a(href = "https://biocomplexity.virginia.edu/brandon-kramer", 'Brandon Kramer', target = '_blank'), "(Postdoctoral Research Associate);",
                                                   #   a(href = 'https://biocomplexity.virginia.edu/sallie-keller', 'Sallie Keller', target = '_blank'), "(Division Director and Distinguished Professor)."),
                                                   # p("", style = "padding-top:10px;")
                                            )
                                   ),
                                   fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                            h4(strong("Project Stakeholders")),
                                            p(""),
                                            p("")
                                            # p(a(href = 'https://www.linkedin.com/in/nancy-bell-aa293810/', 'Nancy Bell', target = '_blank'), "(Virginia Department of Health);",
                                            #   a(href = 'https://www.linkedin.com/in/terri-alt-3138b4101/', 'Terri Alt', target = '_blank'), "(Virginia Cooperative Extension, Patrick County at Virginia Tech)."),
                                            # p("", style = "padding-top:10px;"),
                                            # h4(strong("Acknowledgments")),
                                            # p("We would like to thank Healthy Patrick County, an association of concerned Patrick County residents, and Brandon Kramer for their input to this project.")
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
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  
  output$Dist_91_MPI_Map <- renderLeaflet({
    # Creating variables for M0, M1 and M2
    M0_Total <- switch(input$slider_91_MPI,
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
    
    M1_Total = switch(input$slider_91_MPI,
                      Dist_91_Total_Map@data$M1_k1,
                      Dist_91_Total_Map@data$M1_k2,
                      Dist_91_Total_Map@data$M1_k3,
                      Dist_91_Total_Map@data$M1_k4,
                      Dist_91_Total_Map@data$M1_k5,
                      Dist_91_Total_Map@data$M1_k6,
                      Dist_91_Total_Map@data$M1_k7,
                      Dist_91_Total_Map@data$M1_k8,
                      Dist_91_Total_Map@data$M1_k9)
    
    M2_Total = switch(input$slider_91_MPI,
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
    M0_Urban <- switch(input$slider_91_MPI,
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
    
    M1_Urban = switch(input$slider_91_MPI,
                      Dist_91_Urban_Map@data$M1_k1,
                      Dist_91_Urban_Map@data$M1_k2,
                      Dist_91_Urban_Map@data$M1_k3,
                      Dist_91_Urban_Map@data$M1_k4,
                      Dist_91_Urban_Map@data$M1_k5,
                      Dist_91_Urban_Map@data$M1_k6,
                      Dist_91_Urban_Map@data$M1_k7,
                      Dist_91_Urban_Map@data$M1_k8,
                      Dist_91_Urban_Map@data$M1_k9)
    
    M2_Urban = switch(input$slider_91_MPI,
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
    M0_Rural <- switch(input$slider_91_MPI,
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
    
    M1_Rural = switch(input$slider_91_MPI,
                      Dist_91_Rural_Map@data$M1_k1,
                      Dist_91_Rural_Map@data$M1_k2,
                      Dist_91_Rural_Map@data$M1_k3,
                      Dist_91_Rural_Map@data$M1_k4,
                      Dist_91_Rural_Map@data$M1_k5,
                      Dist_91_Rural_Map@data$M1_k6,
                      Dist_91_Rural_Map@data$M1_k7,
                      Dist_91_Rural_Map@data$M1_k8,
                      Dist_91_Rural_Map@data$M1_k9)
    
    M2_Rural = switch(input$slider_91_MPI,
                      Dist_91_Rural_Map@data$M2_k1,
                      Dist_91_Rural_Map@data$M2_k2,
                      Dist_91_Rural_Map@data$M2_k3,
                      Dist_91_Rural_Map@data$M2_k4,
                      Dist_91_Rural_Map@data$M2_k5,
                      Dist_91_Rural_Map@data$M2_k6,
                      Dist_91_Rural_Map@data$M2_k7,
                      Dist_91_Rural_Map@data$M2_k8,
                      Dist_91_Rural_Map@data$M2_k9)
    
    UrbRurSelection = strtoi(input$UrbRurSelection_MPI_91)
    
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
    M0_labels <- get_label(Dist_91_Map@data$ADM2_EN, "M<sub>0</sub>", M0, switch(input$slider_91_MPI,
                                                                                 National_2017$M0_k1[1],
                                                                                 National_2017$M0_k2[1],
                                                                                 National_2017$M0_k3[1],
                                                                                 National_2017$M0_k4[1],
                                                                                 National_2017$M0_k5[1],
                                                                                 National_2017$M0_k6[1],
                                                                                 National_2017$M0_k7[1],
                                                                                 National_2017$M0_k8[1],
                                                                                 National_2017$M0_k9[1]))
    
    M1_labels <- get_label(Dist_91_Map@data$ADM2_EN, "M<sub>1</sub>", M1, switch(input$slider_91_MPI,
                                                                                 National_2017$M1_k1[1],
                                                                                 National_2017$M1_k2[1],
                                                                                 National_2017$M1_k3[1],
                                                                                 National_2017$M1_k4[1],
                                                                                 National_2017$M1_k5[1],
                                                                                 National_2017$M1_k6[1],
                                                                                 National_2017$M1_k7[1],
                                                                                 National_2017$M1_k8[1],
                                                                                 National_2017$M1_k9[1]))
    
    M2_labels <- get_label(Dist_91_Map@data$ADM2_EN, "M<sub>2</sub>", M2, switch(input$slider_91_MPI,
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
      addLegend(pal = pal, values = c(0, max(M0, na.rm = TRUE)), opacity = 0.7, title = paste0("Index with k = ", input$slider_91_MPI),
                position = "bottomright") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  output$Dist_60_MPI_Map <- renderLeaflet({
    # Creating variables for M0, M1 and M2
    M0_Total <- switch(input$slider_60_MPI,
                       Dist_60_Total_Map@data$M0_k1,
                       Dist_60_Total_Map@data$M0_k2,
                       Dist_60_Total_Map@data$M0_k3,
                       Dist_60_Total_Map@data$M0_k4,
                       Dist_60_Total_Map@data$M0_k5,
                       Dist_60_Total_Map@data$M0_k6,
                       Dist_60_Total_Map@data$M0_k7,
                       Dist_60_Total_Map@data$M0_k8,
                       Dist_60_Total_Map@data$M0_k9
    )
    
    M1_Total = switch(input$slider_60_MPI,
                      Dist_60_Total_Map@data$M1_k1,
                      Dist_60_Total_Map@data$M1_k2,
                      Dist_60_Total_Map@data$M1_k3,
                      Dist_60_Total_Map@data$M1_k4,
                      Dist_60_Total_Map@data$M1_k5,
                      Dist_60_Total_Map@data$M1_k6,
                      Dist_60_Total_Map@data$M1_k7,
                      Dist_60_Total_Map@data$M1_k8,
                      Dist_60_Total_Map@data$M1_k9)
    
    M2_Total = switch(input$slider_60_MPI,
                      Dist_60_Total_Map@data$M2_k1,
                      Dist_60_Total_Map@data$M2_k2,
                      Dist_60_Total_Map@data$M2_k3,
                      Dist_60_Total_Map@data$M2_k4,
                      Dist_60_Total_Map@data$M2_k5,
                      Dist_60_Total_Map@data$M2_k6,
                      Dist_60_Total_Map@data$M2_k7,
                      Dist_60_Total_Map@data$M2_k8,
                      Dist_60_Total_Map@data$M2_k9)
    
    
    # Urban variables for M0, M1 and M2 
    M0_Urban <- switch(input$slider_60_MPI,
                       Dist_60_Urban_Map@data$M0_k1,
                       Dist_60_Urban_Map@data$M0_k2,
                       Dist_60_Urban_Map@data$M0_k3,
                       Dist_60_Urban_Map@data$M0_k4,
                       Dist_60_Urban_Map@data$M0_k5,
                       Dist_60_Urban_Map@data$M0_k6,
                       Dist_60_Urban_Map@data$M0_k7,
                       Dist_60_Urban_Map@data$M0_k8,
                       Dist_60_Urban_Map@data$M0_k9
    )
    
    M1_Urban = switch(input$slider_60_MPI,
                      Dist_60_Urban_Map@data$M1_k1,
                      Dist_60_Urban_Map@data$M1_k2,
                      Dist_60_Urban_Map@data$M1_k3,
                      Dist_60_Urban_Map@data$M1_k4,
                      Dist_60_Urban_Map@data$M1_k5,
                      Dist_60_Urban_Map@data$M1_k6,
                      Dist_60_Urban_Map@data$M1_k7,
                      Dist_60_Urban_Map@data$M1_k8,
                      Dist_60_Urban_Map@data$M1_k9)
    
    M2_Urban = switch(input$slider_60_MPI,
                      Dist_60_Urban_Map@data$M2_k1,
                      Dist_60_Urban_Map@data$M2_k2,
                      Dist_60_Urban_Map@data$M2_k3,
                      Dist_60_Urban_Map@data$M2_k4,
                      Dist_60_Urban_Map@data$M2_k5,
                      Dist_60_Urban_Map@data$M2_k6,
                      Dist_60_Urban_Map@data$M2_k7,
                      Dist_60_Urban_Map@data$M2_k8,
                      Dist_60_Urban_Map@data$M2_k9)
    
    # Rural MPI Variables
    
    # Urban variables for M0, M1 and M2 
    M0_Rural <- switch(input$slider_60_MPI,
                       Dist_60_Rural_Map@data$M0_k1,
                       Dist_60_Rural_Map@data$M0_k2,
                       Dist_60_Rural_Map@data$M0_k3,
                       Dist_60_Rural_Map@data$M0_k4,
                       Dist_60_Rural_Map@data$M0_k5,
                       Dist_60_Rural_Map@data$M0_k6,
                       Dist_60_Rural_Map@data$M0_k7,
                       Dist_60_Rural_Map@data$M0_k8,
                       Dist_60_Rural_Map@data$M0_k9
    )
    
    M1_Rural = switch(input$slider_60_MPI,
                      Dist_60_Rural_Map@data$M1_k1,
                      Dist_60_Rural_Map@data$M1_k2,
                      Dist_60_Rural_Map@data$M1_k3,
                      Dist_60_Rural_Map@data$M1_k4,
                      Dist_60_Rural_Map@data$M1_k5,
                      Dist_60_Rural_Map@data$M1_k6,
                      Dist_60_Rural_Map@data$M1_k7,
                      Dist_60_Rural_Map@data$M1_k8,
                      Dist_60_Rural_Map@data$M1_k9)
    
    M2_Rural = switch(input$slider_60_MPI,
                      Dist_60_Rural_Map@data$M2_k1,
                      Dist_60_Rural_Map@data$M2_k2,
                      Dist_60_Rural_Map@data$M2_k3,
                      Dist_60_Rural_Map@data$M2_k4,
                      Dist_60_Rural_Map@data$M2_k5,
                      Dist_60_Rural_Map@data$M2_k6,
                      Dist_60_Rural_Map@data$M2_k7,
                      Dist_60_Rural_Map@data$M2_k8,
                      Dist_60_Rural_Map@data$M2_k9)
    
    # 1 = Total, 2 = Urban, 3 = Rural
    UrbRurSelection = strtoi(input$UrbRurSelection_MPI_60)
    
    print(UrbRurSelection)
    
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
    M0_labels <- get_label(Dist_60_Map@data$NAME_2, "M<sub>0</sub>", M0, switch(input$slider_60_MPI,
                                                                                National_2017$M0_k1[1],
                                                                                National_2017$M0_k2[1],
                                                                                National_2017$M0_k3[1],
                                                                                National_2017$M0_k4[1],
                                                                                National_2017$M0_k5[1],
                                                                                National_2017$M0_k6[1],
                                                                                National_2017$M0_k7[1],
                                                                                National_2017$M0_k8[1],
                                                                                National_2017$M0_k9[1]))
    
    M1_labels <- get_label(Dist_60_Map@data$NAME_2, "M<sub>1</sub>", M1, switch(input$slider_60_MPI,
                                                                                National_2017$M1_k1[1],
                                                                                National_2017$M1_k2[1],
                                                                                National_2017$M1_k3[1],
                                                                                National_2017$M1_k4[1],
                                                                                National_2017$M1_k5[1],
                                                                                National_2017$M1_k6[1],
                                                                                National_2017$M1_k7[1],
                                                                                National_2017$M1_k8[1],
                                                                                National_2017$M1_k9[1]))
    
    M2_labels <- get_label(Dist_60_Map@data$NAME_2, "M<sub>2</sub>", M2, switch(input$slider_60_MPI,
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
      get_polygon(Dist_60_Map, pal, M0, M0_labels, "M0") %>%
      get_polygon(Dist_60_Map, pal, M1, M1_labels, "M1") %>%
      get_polygon(Dist_60_Map, pal, M2, M2_labels, "M2") %>%
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
    
    # Creating variables for M0, M1 and M2
    M0_Total <- switch(input$slider_Prov_MPI,
                       Prov_Total_Map@data$M0_k1,
                       Prov_Total_Map@data$M0_k2,
                       Prov_Total_Map@data$M0_k3,
                       Prov_Total_Map@data$M0_k4,
                       Prov_Total_Map@data$M0_k5,
                       Prov_Total_Map@data$M0_k6,
                       Prov_Total_Map@data$M0_k7,
                       Prov_Total_Map@data$M0_k8,
                       Prov_Total_Map@data$M0_k9
    )
    
    M1_Total = switch(input$slider_Prov_MPI,
                      Prov_Total_Map@data$M1_k1,
                      Prov_Total_Map@data$M1_k2,
                      Prov_Total_Map@data$M1_k3,
                      Prov_Total_Map@data$M1_k4,
                      Prov_Total_Map@data$M1_k5,
                      Prov_Total_Map@data$M1_k6,
                      Prov_Total_Map@data$M1_k7,
                      Prov_Total_Map@data$M1_k8,
                      Prov_Total_Map@data$M1_k9)
    
    M2_Total = switch(input$slider_Prov_MPI,
                      Prov_Total_Map@data$M2_k1,
                      Prov_Total_Map@data$M2_k2,
                      Prov_Total_Map@data$M2_k3,
                      Prov_Total_Map@data$M2_k4,
                      Prov_Total_Map@data$M2_k5,
                      Prov_Total_Map@data$M2_k6,
                      Prov_Total_Map@data$M2_k7,
                      Prov_Total_Map@data$M2_k8,
                      Prov_Total_Map@data$M2_k9)
    
    
    # Urban variables for M0, M1 and M2 
    M0_Urban <- switch(input$slider_Prov_MPI,
                       Prov_Urban_Map@data$M0_k1,
                       Prov_Urban_Map@data$M0_k2,
                       Prov_Urban_Map@data$M0_k3,
                       Prov_Urban_Map@data$M0_k4,
                       Prov_Urban_Map@data$M0_k5,
                       Prov_Urban_Map@data$M0_k6,
                       Prov_Urban_Map@data$M0_k7,
                       Prov_Urban_Map@data$M0_k8,
                       Prov_Urban_Map@data$M0_k9
    )
    
    M1_Urban = switch(input$slider_Prov_MPI,
                      Prov_Urban_Map@data$M1_k1,
                      Prov_Urban_Map@data$M1_k2,
                      Prov_Urban_Map@data$M1_k3,
                      Prov_Urban_Map@data$M1_k4,
                      Prov_Urban_Map@data$M1_k5,
                      Prov_Urban_Map@data$M1_k6,
                      Prov_Urban_Map@data$M1_k7,
                      Prov_Urban_Map@data$M1_k8,
                      Prov_Urban_Map@data$M1_k9)
    
    M2_Urban = switch(input$slider_Prov_MPI,
                      Prov_Urban_Map@data$M2_k1,
                      Prov_Urban_Map@data$M2_k2,
                      Prov_Urban_Map@data$M2_k3,
                      Prov_Urban_Map@data$M2_k4,
                      Prov_Urban_Map@data$M2_k5,
                      Prov_Urban_Map@data$M2_k6,
                      Prov_Urban_Map@data$M2_k7,
                      Prov_Urban_Map@data$M2_k8,
                      Prov_Urban_Map@data$M2_k9)
    
    # Rural MPI Variables
    
    # Urban variables for M0, M1 and M2 
    M0_Rural <- switch(input$slider_Prov_MPI,
                       Prov_Rural_Map@data$M0_k1,
                       Prov_Rural_Map@data$M0_k2,
                       Prov_Rural_Map@data$M0_k3,
                       Prov_Rural_Map@data$M0_k4,
                       Prov_Rural_Map@data$M0_k5,
                       Prov_Rural_Map@data$M0_k6,
                       Prov_Rural_Map@data$M0_k7,
                       Prov_Rural_Map@data$M0_k8,
                       Prov_Rural_Map@data$M0_k9
    )
    
    M1_Rural = switch(input$slider_Prov_MPI,
                      Prov_Rural_Map@data$M1_k1,
                      Prov_Rural_Map@data$M1_k2,
                      Prov_Rural_Map@data$M1_k3,
                      Prov_Rural_Map@data$M1_k4,
                      Prov_Rural_Map@data$M1_k5,
                      Prov_Rural_Map@data$M1_k6,
                      Prov_Rural_Map@data$M1_k7,
                      Prov_Rural_Map@data$M1_k8,
                      Prov_Rural_Map@data$M1_k9)
    
    M2_Rural = switch(input$slider_Prov_MPI,
                      Prov_Rural_Map@data$M2_k1,
                      Prov_Rural_Map@data$M2_k2,
                      Prov_Rural_Map@data$M2_k3,
                      Prov_Rural_Map@data$M2_k4,
                      Prov_Rural_Map@data$M2_k5,
                      Prov_Rural_Map@data$M2_k6,
                      Prov_Rural_Map@data$M2_k7,
                      Prov_Rural_Map@data$M2_k8,
                      Prov_Rural_Map@data$M2_k9)
    
    # 1 = Total, 2 = Urban, 3 = Rural
    UrbRurSelection = strtoi(input$UrbRurSelection_MPI_Prov)
    
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
    M0_labels <- get_label(Prov_Map@data$ADM1_EN, "M<sub>0</sub>", M0, switch(input$slider_Prov_MPI,
                                                                              National_2017$M0_k1[1],
                                                                              National_2017$M0_k2[1],
                                                                              National_2017$M0_k3[1],
                                                                              National_2017$M0_k4[1],
                                                                              National_2017$M0_k5[1],
                                                                              National_2017$M0_k6[1],
                                                                              National_2017$M0_k7[1],
                                                                              National_2017$M0_k8[1],
                                                                              National_2017$M0_k9[1]))
    
    M1_labels <- get_label(Prov_Map@data$ADM1_EN, "M<sub>1</sub>", M1, switch(input$slider_Prov_MPI,
                                                                              National_2017$M1_k1[1],
                                                                              National_2017$M1_k2[1],
                                                                              National_2017$M1_k3[1],
                                                                              National_2017$M1_k4[1],
                                                                              National_2017$M1_k5[1],
                                                                              National_2017$M1_k6[1],
                                                                              National_2017$M1_k7[1],
                                                                              National_2017$M1_k8[1],
                                                                              National_2017$M1_k9[1]))
    
    M2_labels <- get_label(Prov_Map@data$ADM1_EN, "M<sub>2</sub>", M2, switch(input$slider_Prov_MPI,
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
      get_polygon(Prov_Map, pal, M0, M0_labels, "M0") %>%
      get_polygon(Prov_Map, pal, M1, M1_labels, "M1") %>%
      get_polygon(Prov_Map, pal, M2, M2_labels, "M2") %>%
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
                 Dist_91_Total_Map,
                 Dist_91_Urban_Map,
                 Dist_91_Rural_Map)
    nat_data = switch(UrbRurSelection,
                      National_2017,
                      National_Urban_2017,
                      National_Rural_2017)
    
    
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
                 Dist_60_Total_Map,
                 Dist_60_Urban_Map,
                 Dist_60_Rural_Map)
    nat_data = switch(UrbRurSelection,
                      National_2017,
                      National_Urban_2017,
                      National_Rural_2017)
    
    
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
                 Prov_Total_Map,
                 Prov_Urban_Map,
                 Prov_Rural_Map)
    nat_data = switch(UrbRurSelection,
                      National_2017,
                      National_Urban_2017,
                      National_Rural_2017)
    
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
      addLegend(pal = pal, values = c(0, 0.6), opacity = 0.7, title = paste0("Legend (with k = ", input$slider_Prov_Decomp, ")"),
                na.label = "No Data",
                group = c("Poverty Index", "Max. Education"),
                position = "bottomleft") %>%
      htmlwidgets::prependContent(html_fix)
  })
  }

# Run the App---------------------
shinyApp(ui = ui, server = server)