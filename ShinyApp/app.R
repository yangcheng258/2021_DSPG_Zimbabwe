
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
library(stringr)
library(shinyjs)
library(rgdal)
library(dplyr)
library(sf)
library(gpclib)
library(maptools)
library(shinydashboard)
library(ggpolypath)
gpclibPermit()

# Data--------------------------------------------------------------------------
## 91 District DATA LOADING-----------------------------------------------------------------


## Loading the shapefile
Dist_91_Map <-  readOGR(dsn = paste0(getwd(),"/Shapefiles/91DistrictShapefiles"), layer="zwe_admbnda_adm2_zimstat_ocha_20180911")

## Loading the MPI data at the district level
Dist_91_MPI = read.csv(file = 'MappingData/2017_91_District.csv')
National_2017 = read.csv(file = 'MappingData/2017_National.csv')

## NAME FIXING------------------------------------------------------------------
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

## MERGING DATA-----------------------------------------------------------------

# Rename the district to match the shapefile
colnames(Dist_91_MPI)[1] <- "ADM2_EN"

# Merges the MPI data with the shapefile data while preserving the order of 
# the shapefile names to match up with the polygons
Dist_91_Map@data = merge(Dist_91_Map@data, Dist_91_MPI, by = c("ADM2_EN"), sort = FALSE)


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



## 60 District LOADING DATA ----------------------------------------------------------------

# Loads the shapefile
Dist_60_Map <- readOGR(dsn = paste0(getwd(),"/Shapefiles/60DistrictShapefiles"), layer="gadm36_ZWE_2")

# Loads the district data
Dist_60_Total_2017 = read.csv("MappingData/2017_District.csv")
Dist_60_Urban_2017 = read.csv("MappingData/2017_District_Urban.csv")
Dist_60_Rural_2017 = read.csv("MappingData/2017_District_Rural.csv")

# Loads the national data
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

## Province LOADING DATA ----------------------------------------------------------------

# Loads the shapefile
Prov_Map <- readOGR(dsn = paste0(getwd(),"/Shapefiles/ProvinceShapefiles"), layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# Loads the district data
Prov_Total_2017 = read.csv("MappingData/2017_Province.csv")
Prov_Urban_2017 = read.csv("MappingData/2017_Province_Urban.csv")
Prov_Rural_2017 = read.csv("MappingData/2017_Province_Rural.csv")

## CLEANING Province DATA-------------------------------------------------------

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
                            skin = 'black',
                            dashboardHeader(
                              title = 'Mapping MPI'
                            ),
                            
                            
                            dashboardSidebar(
                              sidebarMenu(
                                menuItem(
                                  "91 District Map",
                                  tabName = '91_Dist'
                                ),
                                menuItem(
                                  "Detailed District Map",
                                  tabName = '60_Dist'
                                ),
                                menuItem(
                                  "Detailed Province Map",
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
                                    leafletOutput("Dist_91_Map"),
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
                                    sliderInput("slider_91", "K-Threshold Value", 1, 9, 3),
                                    width = 12
                                  ))),
                              tabItem(
                                tabName = "60_Dist",
                                fluidPage(
                                  box(
                                    title = "60 District MPI and Decomposition Map of Zimbabwe",
                                    leafletOutput("Dist_60_Map"),
                                    width = 8,
                                    height = 500
                                  ),
                                  box(
                                    withMathJax(),
                                    title = "Description",
                                    p("This graphic shows the poverty indices at the more general 60-District level,
                                      as well as a decomposition of the variables used to make the poverty indices. 
                                      To adjust which index to examine, adjust the 'Select Index to Display' choices. 
                                      This graphic also includes data on the urban and rural populations within each district. 
                                      To adjust the graphic to display the urban, rural or total populations, adjust the 
                                      'Select Urban/Rural Filter' choices. To examine the breakdown of each variables' contribution
                                      to a particular index, hover over the dropdown menu in the top right corner of the map
                                      and select the particular variable of interest. By hovering over each district, the 
                                      measure for that district compared with the national metrics is displayed. And as always,
                                      to adjust the k-constant for the poverty threshold, use the slider at the bottom of the page."),
                                    width = 4
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("MPI_Buttons_60", "Select Index to Display:", 
                                                 choiceNames = c("Adj. Headcount Ratio \\((M_{0})\\)",
                                                                 "Adj. Poverty Gap \\((M_{1})\\)",
                                                                 "Adj. Poverty Severity \\((M_{2})\\)"),
                                                 choiceValues = c(1, 2, 3))
                                  ),
                                  box(
                                    radioButtons("UrbRur_Buttons_60", "Select Urban/Rural Filter", 
                                                 choiceNames = c("All",
                                                                 "Urban",
                                                                 "Rural"),
                                                 choiceValues = c(1, 2, 3))
                                  ),
                                  box(
                                    sliderInput("slider_60", "K-Threshold Value", 1, 9, 3),
                                    width = 12
                                  )
                                )
                              ),
                              tabItem(
                                tabName = "Prov",
                                fluidPage(
                                  box(
                                    title = "Province MPI and Decomposition Map of Zimbabwe",
                                    leafletOutput("Prov_Map"),
                                    width = 8,
                                    height = 500
                                  ),
                                  box(
                                    withMathJax(),
                                    title = "Description",
                                    p("This graphic shows the poverty indices at the Province level,
                                      as well as a decomposition of the variables used to make the poverty indices. 
                                      To adjust which index to examine, adjust the 'Select Index to Display' choices. 
                                      This graphic also includes data on the urban and rural populations within each province. 
                                      To adjust the graphic to display the urban, rural or total populations, adjust the 
                                      'Select Urban/Rural Filter' choices. To examine the breakdown of each variables' contribution
                                      to a particular index, hover over the dropdown menu in the top right corner of the map
                                      and select the particular variable of interest. By hovering over each province, the 
                                      measure for that district compared with the national metrics is displayed. And as always,
                                      to adjust the k-constant for the poverty threshold, use the slider at the bottom of the page."),
                                    width = 4
                                  ),
                                  box(
                                    withMathJax(),
                                    radioButtons("MPI_Buttons_Prov", "Select Index to Display:", 
                                                 choiceNames = c("Adj. Headcount Ratio \\((M_{0})\\)",
                                                                 "Adj. Poverty Gap \\((M_{1})\\)",
                                                                 "Adj. Poverty Severity \\((M_{2})\\)"),
                                                 choiceValues = c(1, 2, 3))
                                  ),
                                  box(
                                    radioButtons("UrbRur_Buttons_Prov", "", 
                                                 choiceNames = c("Total",
                                                                 "Urban",
                                                                 "Rural"),
                                                 choiceValues = c(1, 2, 3))
                                  ),
                                  box(
                                    sliderInput("slider_Prov", "K-Threshold Value", 1, 9, 3),
                                    width = 12
                                  )
                                ))
                            )))),
                 
                          ## Tab team -----------------------------------------------------------
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
  
  
  output$Dist_91_Map <- renderLeaflet({
    # Creating variables for M0, M1 and M2
    M0 <- switch(input$slider_91,
                 Dist_91_Map@data$M0_k1,
                 Dist_91_Map@data$M0_k2,
                 Dist_91_Map@data$M0_k3,
                 Dist_91_Map@data$M0_k4,
                 Dist_91_Map@data$M0_k5,
                 Dist_91_Map@data$M0_k6,
                 Dist_91_Map@data$M0_k7,
                 Dist_91_Map@data$M0_k8,
                 Dist_91_Map@data$M0_k9
    )
    
    M1 = switch(input$slider_91,
                Dist_91_Map@data$M1_k1,
                Dist_91_Map@data$M1_k2,
                Dist_91_Map@data$M1_k3,
                Dist_91_Map@data$M1_k4,
                Dist_91_Map@data$M1_k5,
                Dist_91_Map@data$M1_k6,
                Dist_91_Map@data$M1_k7,
                Dist_91_Map@data$M1_k8,
                Dist_91_Map@data$M1_k9)
    
    M2 = switch(input$slider_91,
                Dist_91_Map@data$M2_k1,
                Dist_91_Map@data$M2_k2,
                Dist_91_Map@data$M2_k3,
                Dist_91_Map@data$M2_k4,
                Dist_91_Map@data$M2_k5,
                Dist_91_Map@data$M2_k6,
                Dist_91_Map@data$M2_k7,
                Dist_91_Map@data$M2_k8,
                Dist_91_Map@data$M2_k9)
    
    
    # This is the color palette used in the graphs
    pal <- colorNumeric(
      palette = c(
        "#ffffff",
        "#fffe88",
        "#fffd38",
        "#ed7e00",
        "#f66324",
        "#fe4747",
        "#df326e",
        "#c5208e",
        "#b112a8",
        "#9214b2",
        "#6d17bd",
        "#4f19c7",
        "#301bd0",
        "#2d15a3",
        "#290c57"
      ),
      domain = M0,
      reverse = FALSE)
    
    # This creates labels for M0, M1 and M2 
    M0_labels <- get_label(Dist_91_Map@data$ADM2_EN, "M<sub>0</sub>", M0, switch(input$slider_91,
                                                                                 National_2017$M0_k1[1],
                                                                                 National_2017$M0_k2[1],
                                                                                 National_2017$M0_k3[1],
                                                                                 National_2017$M0_k4[1],
                                                                                 National_2017$M0_k5[1],
                                                                                 National_2017$M0_k6[1],
                                                                                 National_2017$M0_k7[1],
                                                                                 National_2017$M0_k8[1],
                                                                                 National_2017$M0_k9[1]))
    
    M1_labels <- get_label(Dist_91_Map@data$ADM2_EN, "M<sub>1</sub>", M1, switch(input$slider_91,
                                                                                 National_2017$M1_k1[1],
                                                                                 National_2017$M1_k2[1],
                                                                                 National_2017$M1_k3[1],
                                                                                 National_2017$M1_k4[1],
                                                                                 National_2017$M1_k5[1],
                                                                                 National_2017$M1_k6[1],
                                                                                 National_2017$M1_k7[1],
                                                                                 National_2017$M1_k8[1],
                                                                                 National_2017$M1_k9[1]))
    
    M2_labels <- get_label(Dist_91_Map@data$ADM2_EN, "M<sub>2</sub>", M2, switch(input$slider_91,
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
      addLegend(pal = pal, values = M0, opacity = 0.7, title = switch(input$slider_91,
                                                                      "Index with K=1",
                                                                      "Index with K=2",
                                                                      "Index with K=3",
                                                                      "Index with K=4",
                                                                      "Index with K=5",
                                                                      "Index with K=6",
                                                                      "Index with K=7",
                                                                      "Index with K=8",
                                                                      "Index with K=9"),
                position = "bottomright")
  })
  
  output$Dist_60_Map <- renderLeaflet({
    # Variables for total data are created 
    M0_Total = switch(input$slider_60,
                      Dist_60_Total_Map@data$M0_k1,
                      Dist_60_Total_Map@data$M0_k2,
                      Dist_60_Total_Map@data$M0_k3,
                      Dist_60_Total_Map@data$M0_k4,
                      Dist_60_Total_Map@data$M0_k5,
                      Dist_60_Total_Map@data$M0_k6,
                      Dist_60_Total_Map@data$M0_k7,
                      Dist_60_Total_Map@data$M0_k8,
                      Dist_60_Total_Map@data$M0_k9)
    
    M1_Total = switch(input$slider_60,
                      Dist_60_Total_Map@data$M1_k1,
                      Dist_60_Total_Map@data$M1_k2,
                      Dist_60_Total_Map@data$M1_k3,
                      Dist_60_Total_Map@data$M1_k4,
                      Dist_60_Total_Map@data$M1_k5,
                      Dist_60_Total_Map@data$M1_k6,
                      Dist_60_Total_Map@data$M1_k7,
                      Dist_60_Total_Map@data$M1_k8,
                      Dist_60_Total_Map@data$M1_k9)
    
    M2_Total = switch(input$slider_60,
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
    M0_Urban = switch(input$slider_60,
                      Dist_60_Urban_Map@data$M0_k1,
                      Dist_60_Urban_Map@data$M0_k2,
                      Dist_60_Urban_Map@data$M0_k3,
                      Dist_60_Urban_Map@data$M0_k4,
                      Dist_60_Urban_Map@data$M0_k5,
                      Dist_60_Urban_Map@data$M0_k6,
                      Dist_60_Urban_Map@data$M0_k7,
                      Dist_60_Urban_Map@data$M0_k8,
                      Dist_60_Urban_Map@data$M0_k9)
    
    M1_Urban = switch(input$slider_60,
                      Dist_60_Urban_Map@data$M1_k1,
                      Dist_60_Urban_Map@data$M1_k2,
                      Dist_60_Urban_Map@data$M1_k3,
                      Dist_60_Urban_Map@data$M1_k4,
                      Dist_60_Urban_Map@data$M1_k5,
                      Dist_60_Urban_Map@data$M1_k6,
                      Dist_60_Urban_Map@data$M1_k7,
                      Dist_60_Urban_Map@data$M1_k8,
                      Dist_60_Urban_Map@data$M1_k9)
    
    M2_Urban = switch(input$slider_60,
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
    M0_Rural = switch(input$slider_60,
                      Dist_60_Rural_Map@data$M0_k1,
                      Dist_60_Rural_Map@data$M0_k2,
                      Dist_60_Rural_Map@data$M0_k3,
                      Dist_60_Rural_Map@data$M0_k4,
                      Dist_60_Rural_Map@data$M0_k5,
                      Dist_60_Rural_Map@data$M0_k6,
                      Dist_60_Rural_Map@data$M0_k7,
                      Dist_60_Rural_Map@data$M0_k8,
                      Dist_60_Rural_Map@data$M0_k9)
    
    M1_Rural = switch(input$slider_60,
                      Dist_60_Rural_Map@data$M1_k1,
                      Dist_60_Rural_Map@data$M1_k2,
                      Dist_60_Rural_Map@data$M1_k3,
                      Dist_60_Rural_Map@data$M1_k4,
                      Dist_60_Rural_Map@data$M1_k5,
                      Dist_60_Rural_Map@data$M1_k6,
                      Dist_60_Rural_Map@data$M1_k7,
                      Dist_60_Rural_Map@data$M1_k8,
                      Dist_60_Rural_Map@data$M1_k9)
    
    M2_Rural = switch(input$slider_60,
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
    mpi_selection = strtoi(input$MPI_Buttons_60)
    
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
    urban_rural_selection = strtoi(input$UrbRur_Buttons_60)
    
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
      palette = c(
        "#ffffff",
        "#fffe88",
        "#fffd38",
        "#ed7e00",
        "#f66324",
        "#fe4747",
        "#df326e",
        "#c5208e",
        "#b112a8",
        "#9214b2",
        "#6d17bd",
        "#4f19c7",
        "#301bd0",
        "#2d15a3",
        "#290c57"
      ),
      domain = c(0, 1),
      reverse = FALSE)
    
    
    index_labels <- get_label(Dist_60_Total_Map@data$NAME_2, paste0("M<sub>", mpi_selection - 1, "</sub>"), index, switch(input$slider_60,
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
      addLegend(pal = pal, values = c(0, 1), opacity = 0.7, title = paste0("Legend (with k = ", input$slider_60, ")"),
                na.label = "No Data",
                group = c("Poverty Index", "Max. Education"),
                position = "bottomleft") %>%
      htmlwidgets::prependContent(html_fix)
  })
  
  output$Prov_Map <- renderLeaflet({
    
    # Variables for total data are created 
    M0_Total = switch(input$slider_Prov,
                      Prov_Total_Map@data$M0_k1,
                      Prov_Total_Map@data$M0_k2,
                      Prov_Total_Map@data$M0_k3,
                      Prov_Total_Map@data$M0_k4,
                      Prov_Total_Map@data$M0_k5,
                      Prov_Total_Map@data$M0_k6,
                      Prov_Total_Map@data$M0_k7,
                      Prov_Total_Map@data$M0_k8,
                      Prov_Total_Map@data$M0_k9)
    
    M1_Total = switch(input$slider_Prov,
                      Prov_Total_Map@data$M1_k1,
                      Prov_Total_Map@data$M1_k2,
                      Prov_Total_Map@data$M1_k3,
                      Prov_Total_Map@data$M1_k4,
                      Prov_Total_Map@data$M1_k5,
                      Prov_Total_Map@data$M1_k6,
                      Prov_Total_Map@data$M1_k7,
                      Prov_Total_Map@data$M1_k8,
                      Prov_Total_Map@data$M1_k9)
    
    M2_Total = switch(input$slider_Prov,
                      Prov_Total_Map@data$M2_k1,
                      Prov_Total_Map@data$M2_k2,
                      Prov_Total_Map@data$M2_k3,
                      Prov_Total_Map@data$M2_k4,
                      Prov_Total_Map@data$M2_k5,
                      Prov_Total_Map@data$M2_k6,
                      Prov_Total_Map@data$M2_k7,
                      Prov_Total_Map@data$M2_k8,
                      Prov_Total_Map@data$M2_k9)
    
    # Variables for Urban data are created 
    M0_Urban = switch(input$slider_Prov,
                      Prov_Urban_Map@data$M0_k1,
                      Prov_Urban_Map@data$M0_k2,
                      Prov_Urban_Map@data$M0_k3,
                      Prov_Urban_Map@data$M0_k4,
                      Prov_Urban_Map@data$M0_k5,
                      Prov_Urban_Map@data$M0_k6,
                      Prov_Urban_Map@data$M0_k7,
                      Prov_Urban_Map@data$M0_k8,
                      Prov_Urban_Map@data$M0_k9)
    
    M1_Urban = switch(input$slider_Prov,
                      Prov_Urban_Map@data$M1_k1,
                      Prov_Urban_Map@data$M1_k2,
                      Prov_Urban_Map@data$M1_k3,
                      Prov_Urban_Map@data$M1_k4,
                      Prov_Urban_Map@data$M1_k5,
                      Prov_Urban_Map@data$M1_k6,
                      Prov_Urban_Map@data$M1_k7,
                      Prov_Urban_Map@data$M1_k8,
                      Prov_Urban_Map@data$M1_k9)
    
    M2_Urban = switch(input$slider_Prov,
                      Prov_Urban_Map@data$M2_k1,
                      Prov_Urban_Map@data$M2_k2,
                      Prov_Urban_Map@data$M2_k3,
                      Prov_Urban_Map@data$M2_k4,
                      Prov_Urban_Map@data$M2_k5,
                      Prov_Urban_Map@data$M2_k6,
                      Prov_Urban_Map@data$M2_k7,
                      Prov_Urban_Map@data$M2_k8,
                      Prov_Urban_Map@data$M2_k9)
    
    # Variables for total data are created 
    M0_Rural = switch(input$slider_Prov,
                      Prov_Rural_Map@data$M0_k1,
                      Prov_Rural_Map@data$M0_k2,
                      Prov_Rural_Map@data$M0_k3,
                      Prov_Rural_Map@data$M0_k4,
                      Prov_Rural_Map@data$M0_k5,
                      Prov_Rural_Map@data$M0_k6,
                      Prov_Rural_Map@data$M0_k7,
                      Prov_Rural_Map@data$M0_k8,
                      Prov_Rural_Map@data$M0_k9)
    
    M1_Rural = switch(input$slider_Prov,
                      Prov_Rural_Map@data$M1_k1,
                      Prov_Rural_Map@data$M1_k2,
                      Prov_Rural_Map@data$M1_k3,
                      Prov_Rural_Map@data$M1_k4,
                      Prov_Rural_Map@data$M1_k5,
                      Prov_Rural_Map@data$M1_k6,
                      Prov_Rural_Map@data$M1_k7,
                      Prov_Rural_Map@data$M1_k8,
                      Prov_Rural_Map@data$M1_k9)
    
    M2_Rural = switch(input$slider_Prov,
                      Prov_Rural_Map@data$M2_k1,
                      Prov_Rural_Map@data$M2_k2,
                      Prov_Rural_Map@data$M2_k3,
                      Prov_Rural_Map@data$M2_k4,
                      Prov_Rural_Map@data$M2_k5,
                      Prov_Rural_Map@data$M2_k6,
                      Prov_Rural_Map@data$M2_k7,
                      Prov_Rural_Map@data$M2_k8,
                      Prov_Rural_Map@data$M2_k9)
    
    # We need to select how these variables affect M0, M1 and M2 by selecting
    # which index to look at 1 = M0, 2 = M1, 3 = M2
    mpi_selection = strtoi(input$MPI_Buttons_Prov)
    
    # This allocates the total MPI Variables
    education_max_Total = switch(mpi_selection, Prov_Total_Map@data$M0_education_max, Prov_Total_Map@data$M1_education_max,Prov_Total_Map@data$M2_education_max)
    education_dropout_Total = switch(mpi_selection, Prov_Total_Map@data$M0_education_dropout, Prov_Total_Map@data$M1_education_dropout,Prov_Total_Map@data$M2_education_dropout)
    health_chronic_Total = switch(mpi_selection, Prov_Total_Map@data$M0_health_chronic, Prov_Total_Map@data$M1_health_chronic,Prov_Total_Map@data$M2_health_chronic)
    health_visit_Total = switch(mpi_selection, Prov_Total_Map@data$M0_health_visit, Prov_Total_Map@data$M1_health_visit,Prov_Total_Map@data$M2_health_visit)
    employment_Total = switch(mpi_selection, Prov_Total_Map@data$M0_employment, Prov_Total_Map@data$M1_employment,Prov_Total_Map@data$M2_employment)
    assets_Total = switch(mpi_selection, Prov_Total_Map@data$M0_assets, Prov_Total_Map@data$M1_assets,Prov_Total_Map@data$M2_assets)
    services_Total = switch(mpi_selection, Prov_Total_Map@data$M0_services, Prov_Total_Map@data$M1_services,Prov_Total_Map@data$M2_services)
    electricity_Total = switch(mpi_selection, Prov_Total_Map@data$M0_electricity, Prov_Total_Map@data$M1_electricity,Prov_Total_Map@data$M2_electricity)
    cooking_fuel_Total = switch(mpi_selection, Prov_Total_Map@data$M0_cooking_fuel, Prov_Total_Map@data$M1_cooking_fuel,Prov_Total_Map@data$M2_cooking_fuel)
    water_Total = switch(mpi_selection, Prov_Total_Map@data$M0_water, Prov_Total_Map@data$M1_water,Prov_Total_Map@data$M2_water)
    toilet_Total = switch(mpi_selection, Prov_Total_Map@data$M0_toilet, Prov_Total_Map@data$M1_toilet,Prov_Total_Map@data$M2_toilet)
    land_Total = switch(mpi_selection, Prov_Total_Map@data$M0_land, Prov_Total_Map@data$M1_land,Prov_Total_Map@data$M2_land)
    livestock_Total = switch(mpi_selection, Prov_Total_Map@data$M0_livestock, Prov_Total_Map@data$M1_livestock,Prov_Total_Map@data$M2_livestock)
    rural_equip_Total = switch(mpi_selection, Prov_Total_Map@data$M0_rural_equip, Prov_Total_Map@data$M1_rural_equip,Prov_Total_Map@data$M2_rural_equip)
    
    # This allocates the urban MPI Variables
    education_max_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_education_max, Prov_Urban_Map@data$M1_education_max,Prov_Urban_Map@data$M2_education_max)
    education_dropout_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_education_dropout, Prov_Urban_Map@data$M1_education_dropout,Prov_Urban_Map@data$M2_education_dropout)
    health_chronic_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_health_chronic, Prov_Urban_Map@data$M1_health_chronic,Prov_Urban_Map@data$M2_health_chronic)
    health_visit_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_health_visit, Prov_Urban_Map@data$M1_health_visit,Prov_Urban_Map@data$M2_health_visit)
    employment_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_employment, Prov_Urban_Map@data$M1_employment,Prov_Urban_Map@data$M2_employment)
    assets_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_assets, Prov_Urban_Map@data$M1_assets,Prov_Urban_Map@data$M2_assets)
    services_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_services, Prov_Urban_Map@data$M1_services,Prov_Urban_Map@data$M2_services)
    electricity_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_electricity, Prov_Urban_Map@data$M1_electricity,Prov_Urban_Map@data$M2_electricity)
    cooking_fuel_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_cooking_fuel, Prov_Urban_Map@data$M1_cooking_fuel,Prov_Urban_Map@data$M2_cooking_fuel)
    water_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_water, Prov_Urban_Map@data$M1_water,Prov_Urban_Map@data$M2_water)
    toilet_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_toilet, Prov_Urban_Map@data$M1_toilet,Prov_Urban_Map@data$M2_toilet)
    land_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_land, Prov_Urban_Map@data$M1_land,Prov_Urban_Map@data$M2_land)
    livestock_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_livestock, Prov_Urban_Map@data$M1_livestock,Prov_Urban_Map@data$M2_livestock)
    rural_equip_Urban = switch(mpi_selection, Prov_Urban_Map@data$M0_rural_equip, Prov_Urban_Map@data$M1_rural_equip,Prov_Urban_Map@data$M2_rural_equip)
    
    # This allocates the rural MPI variables
    education_max_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_education_max, Prov_Rural_Map@data$M1_education_max,Prov_Rural_Map@data$M2_education_max)
    education_dropout_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_education_dropout, Prov_Rural_Map@data$M1_education_dropout,Prov_Rural_Map@data$M2_education_dropout)
    health_chronic_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_health_chronic, Prov_Rural_Map@data$M1_health_chronic,Prov_Rural_Map@data$M2_health_chronic)
    health_visit_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_health_visit, Prov_Rural_Map@data$M1_health_visit,Prov_Rural_Map@data$M2_health_visit)
    employment_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_employment, Prov_Rural_Map@data$M1_employment,Prov_Rural_Map@data$M2_employment)
    assets_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_assets, Prov_Rural_Map@data$M1_assets,Prov_Rural_Map@data$M2_assets)
    services_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_services, Prov_Rural_Map@data$M1_services,Prov_Rural_Map@data$M2_services)
    electricity_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_electricity, Prov_Rural_Map@data$M1_electricity,Prov_Rural_Map@data$M2_electricity)
    cooking_fuel_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_cooking_fuel, Prov_Rural_Map@data$M1_cooking_fuel,Prov_Rural_Map@data$M2_cooking_fuel)
    water_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_water, Prov_Rural_Map@data$M1_water,Prov_Rural_Map@data$M2_water)
    toilet_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_toilet, Prov_Rural_Map@data$M1_toilet,Prov_Rural_Map@data$M2_toilet)
    land_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_land, Prov_Rural_Map@data$M1_land,Prov_Rural_Map@data$M2_land)
    livestock_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_livestock, Prov_Rural_Map@data$M1_livestock,Prov_Rural_Map@data$M2_livestock)
    rural_equip_Rural = switch(mpi_selection, Prov_Rural_Map@data$M0_rural_equip, Prov_Rural_Map@data$M1_rural_equip,Prov_Rural_Map@data$M2_rural_equip)
    
    
    # We need to select how these variables affect M0, M1 and M2 by selecting
    # which index to look at 1 = Total, 2 = Urban, 3 = Rural
    urban_rural_selection = strtoi(input$UrbRur_Buttons_Prov)
    
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
      palette = c(
        "#ffffff",
        "#fffe88",
        "#fffd38",
        "#ed7e00",
        "#f66324",
        "#fe4747",
        "#df326e",
        "#c5208e",
        "#b112a8",
        "#9214b2",
        "#6d17bd",
        "#4f19c7",
        "#301bd0",
        "#2d15a3",
        "#290c57"
      ),
      domain = c(0, 1),
      reverse = FALSE)
    
    
    index_labels <- get_label(Prov_Total_Map@data$ADM1_EN, paste0("M<sub>", mpi_selection - 1, "</sub>"), index, switch(input$slider_Prov,
                                                                                                                        National_2017$M0_k1[1],
                                                                                                                        National_2017$M0_k2[1],
                                                                                                                        National_2017$M0_k3[1],
                                                                                                                        National_2017$M0_k4[1],
                                                                                                                        National_2017$M0_k5[1],
                                                                                                                        National_2017$M0_k6[1],
                                                                                                                        National_2017$M0_k7[1],
                                                                                                                        National_2017$M0_k8[1],
                                                                                                                        National_2017$M0_k9[1]))
    education_max_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Max. Education", education_max, switch(mpi_selection,
                                                                                                           National_2017$M0_education_max[1],
                                                                                                           National_2017$M1_education_max[1],
                                                                                                           National_2017$M2_education_max[1]))
    education_dropout_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Education Dropout", education_dropout, switch(mpi_selection,
                                                                                                                      National_2017$M0_education_dropout[1],
                                                                                                                      National_2017$M1_education_dropout[1],
                                                                                                                      National_2017$M2_education_dropout[1]))
    health_chronic_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Chronic Illness", health_chronic, switch(mpi_selection,
                                                                                                              National_2017$M0_health_chronic[1],
                                                                                                              National_2017$M1_health_chronic[1],
                                                                                                              National_2017$M2_health_chronic[1]))
    health_visit_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Lack of Health Visit", health_visit, switch(mpi_selection,
                                                                                                               National_2017$M0_health_visit[1],
                                                                                                               National_2017$M1_health_visit[1],
                                                                                                               National_2017$M2_health_visit[1]))
    employment_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Unemployment", employment, switch(mpi_selection,
                                                                                                   National_2017$M0_employment[1],
                                                                                                   National_2017$M1_employment[1],
                                                                                                   National_2017$M2_employment[1]))
    assets_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Household Assets", assets, switch(mpi_selection,
                                                                                               National_2017$M0_assets[1],
                                                                                               National_2017$M1_assets[1],
                                                                                               National_2017$M2_assets[1]))
    services_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Access to Services", services, switch(mpi_selection,
                                                                                                     National_2017$M0_services[1],
                                                                                                     National_2017$M1_services[1],
                                                                                                     National_2017$M2_services[1]))
    electricity_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Lack of Electricity", electricity, switch(mpi_selection,
                                                                                                            National_2017$M0_electricity[1],
                                                                                                            National_2017$M1_electricity[1],
                                                                                                            National_2017$M2_electricity[1]))
    cooking_fuel_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Poor Cooking Fuel", cooking_fuel, switch(mpi_selection,
                                                                                                            National_2017$M0_cooking_fuel[1],
                                                                                                            National_2017$M1_cooking_fuel[1],
                                                                                                            National_2017$M2_cooking_fuel[1]))
    water_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Poor Water Source", water, switch(mpi_selection,
                                                                                              National_2017$M0_water[1],
                                                                                              National_2017$M1_water[1],
                                                                                              National_2017$M2_water[1]))
    toilet_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Lack of Toilet", toilet, switch(mpi_selection,
                                                                                             National_2017$M0_toilet[1],
                                                                                             National_2017$M1_toilet[1],
                                                                                             National_2017$M2_toilet[1]))
    land_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Lack of Land", land, switch(mpi_selection,
                                                                                       National_2017$M0_land[1],
                                                                                       National_2017$M1_land[1],
                                                                                       National_2017$M2_land[1]))
    livestock_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Lack of Livestock", livestock, switch(mpi_selection,
                                                                                                      National_2017$M0_livestock[1],
                                                                                                      National_2017$M1_livestock[1],
                                                                                                      National_2017$M2_livestock[1]))
    rural_equip_labels <- get_label(Prov_Total_Map@data$ADM1_EN, "Lack of Rural Equipment", rural_equip, switch(mpi_selection,
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
      get_polygon(Prov_Total_Map, pal, index, index_labels, "Poverty Index") %>%
      get_polygon(Prov_Total_Map, pal, education_max, education_max_labels, "Max. Education") %>%
      get_polygon(Prov_Total_Map, pal, education_dropout, education_dropout_labels, "Education Dropout") %>%
      get_polygon(Prov_Total_Map, pal, health_chronic, health_chronic_labels, "Chronic Illness") %>%
      get_polygon(Prov_Total_Map, pal, health_visit, health_visit_labels, "Lack of Health Visit") %>%
      get_polygon(Prov_Total_Map, pal, employment, employment_labels, "Unemployment") %>%
      get_polygon(Prov_Total_Map, pal, assets, assets_labels, "Lack of Household Assets") %>%
      get_polygon(Prov_Total_Map, pal, services, services_labels, "Lack of Access to Services") %>%
      get_polygon(Prov_Total_Map, pal, electricity, electricity_labels, "Lack of Electricity") %>%
      get_polygon(Prov_Total_Map, pal, cooking_fuel, cooking_fuel_labels, "Poor Cooking Fuel") %>%
      get_polygon(Prov_Total_Map, pal, water, water_labels, "Poor Water Source") %>%
      get_polygon(Prov_Total_Map, pal, toilet, toilet_labels, "Lack of Toilet") %>%
      get_polygon(Prov_Total_Map, pal, land, land_labels, "Lack of Land") %>%
      get_polygon(Prov_Total_Map, pal, livestock, livestock_labels, "Lack of Livestock") %>%
      get_polygon(Prov_Total_Map, pal, rural_equip, rural_equip_labels, "Lack of Rural Equipment") %>%
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
      addLegend(pal = pal, values = c(0, 1), opacity = 0.7, title = paste0("Legend (with k = ", input$slider_Prov, ")"),
                na.label = "No Data",
                group = c("Poverty Index", "Max. Education"),
                position = "bottomleft") %>%
      htmlwidgets::prependContent(html_fix)
  })
}


# Run the App--------------------------
shinyApp(ui = ui, server = server)