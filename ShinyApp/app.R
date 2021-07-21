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
library(stringr)
library(shinyjs)

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

# user -------------------------------------------------------------
ui <- navbarPage(title = "Hampton Roads",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 # main -----------------------------------------------------------
                 # tabPanel("Home", value = "home",
                 #          fluidRow(style = "margin: 6px;",
                 #                   align = "center",
                 #                   br("", style = "padding-top:10px;"),
                 #                   img(src = "VTDSPG Logo.png", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                 #                   br(""),
                 #                   h2(strong("Addressing and Tracking the Impact of Systematic Issues in Hampton Roads"),
                 #                   br(""),
                 #                   h4("Data Science for the Public Good Program"),
                 #                   h4("Virginia Tech"),
                 #                   h4("Department of Agriculture"),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   p(tags$small(em('Last updated: July 2021')))
                 #                   )
                 #          )
                 # ),
                 
                 # main -----------------------------------------------------------
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

                          # data and methodology ----------------------------------------------------
                 tabPanel("Data & Methodology", value = "dm",
                             tabsetPanel(
                               tabPanel("Data", value = "data"),
                               tabPanel("Methodology", value = "methodology")
                             )),

                          # maps --------------------------------------------------------------------
                 tabPanel("Mapping MPI", value = "maps"
                             ),
                 
                          # team -----------------------------------------------------------
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
  
  
  #educational attainment plots working on it....................
  var_genEducationalAttainment <- reactive({
    input$genEdAttainmentYearDrop
  })
  var_blackEducationalAttainment <- reactive({
    input$blackEdAttainmentYearDrop
  })
  output$genEdAttainmentPlots <- renderPlot({
    if(var_genEducationalAttainment() == "2019") {
      generalEducationalAttainement2019 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment(2019).csv")
      va_tot_education_bar <- generalEducationalAttainement2019  %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(x = NAME, y = pct_tot, fill = NAME)) + geom_col() +
        theme_minimal() + labs(title = "Highest Educational Attainment for General Population 25 years and older: Bachelor's degree or higher",
                               y = "Percent (%)",
                               x = "Counties of Hampton Roads",
                               caption = "Source: ACS 5 Year Estimate Table S1501") + theme(axis.text.x = element_text(angle = 40))
      
      #this is likely not the most efficient way of coloring the scale but it works so using it for now, will hopefully change later...  
      va_tot_education_bar
      
      
    }})
  
  
  # socio plots: done -----------------------------------------------------
  
  var <- reactive({
    input$sociodrop
  })
  #age65
  output$socioplot <- renderLeaflet({
    if(var() == "age65") {
      
      pal <- colorQuantile("Blues", domain = socdem_block$age65, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 65 or over:</strong>",
              round(socdem_block$age65, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$age65), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$age65),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #under18
    }else if(var() == "under18"){
      pal <- colorQuantile("Blues", domain = socdem_block$under18, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 18 or under: </strong>",
              round(socdem_block$under18, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$under18), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$under18),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #population-tract
    }else if(var() == "totalpop_trct"){
      pal <- colorQuantile("Blues", domain = socdem_tract$totalpop_trct, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_tract$totalpop_trct, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$totalpop_trct), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$totalpop_trct),
                  title = "Total Population<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
      #population-block group
    }else if(var() == "totalpop_bgrp"){
      pal <- colorQuantile("Blues", domain = socdem_block$totalpop_bgrp, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_block$totalpop_bgrp, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$totalpop_bgrp), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$totalpop_bgrp),
                  title = "Total Population<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
    }else if(var() == "black"){
      pal <- colorQuantile("Blues", domain = socdem_block$black, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population Black: </strong>",
              round(socdem_block$black, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$black), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$black),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "noba"){
      pal <- colorQuantile("Blues", domain = socdem_block$noba, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without BA degree: </strong>",
              round(socdem_block$noba, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$noba), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$noba),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "unempl"){
      pal <- colorQuantile("Blues", domain = socdem_block$unempl, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population in labor force unemployed: </strong>",
              round(socdem_block$unempl, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$unempl), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$unempl),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "nohealthins2"){
      pal <- colorQuantile("Blues", domain = socdem_block$nohealthins2, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without health insurance: </strong>",
              round(socdem_block$nohealthins2, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$nohealthins2), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$nohealthins2),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "snap"){
      pal <- colorQuantile("Blues", domain = socdem_block$snap, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population receiving public assistance or SNAP benefits: </strong>",
              round(socdem_block$snap, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$snap), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$snap),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "inpov"){
      pal <- colorQuantile("Blues", domain = socdem_tract$inpov, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population in poverty: </strong>",
              round(socdem_tract$inpov, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$inpov), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$inpov),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "hispanic"){
      pal <- colorQuantile("Blues", domain = socdem_tract$hispanic, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population Hispanic or Latino: </strong>",
              round(socdem_tract$hispanic, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$hispanic), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$hispanic),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "privateins"){
      pal <- colorQuantile("Blues", domain = socdem_tract$privateins, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with private health insurance: </strong>",
              round(socdem_tract$privateins, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$privateins), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$privateins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      pal <- colorQuantile("Blues", domain = socdem_tract$publicins, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with public health insurance: </strong>",
              round(socdem_tract$publicins, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$publicins), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$publicins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
  # old plots - snap -----------------------------------------------
  var_old <- reactive({
    input$olddrop
  })
  var_hh <- reactive({
    input$hhdrop
  })
  output$oldplot <- renderLeaflet({
    # healthins wasn't coded properly so it's just all zeroes
    if(var_old() == "visdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$visdiff,
                     "_f" = olderadults$visdiff_f,
                     "_m" = olderadults$visdiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with vision difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ambdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ambdiff,
                     "_f" = olderadults$ambdiff_f,
                     "_m" = olderadults$ambdiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with ambulatory difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "cogdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$cogdiff,
                     "_f" = olderadults$cogdiff_f,
                     "_m" = olderadults$cogdiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with cognitive difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "carediff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$carediff,
                     "_f" = olderadults$carediff_f,
                     "_m" = olderadults$carediff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with self-care difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ildiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ildiff,
                     "_f" = olderadults$ildiff_f,
                     "_m" = olderadults$ildiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with independent living difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "disab") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$disab,
                     "_f" = olderadults$disab_f,
                     "_m" = olderadults$disab_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with any disability: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "inpov") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$inpov,
                     "_f" = olderadults$inpov_f,
                     "_m" = olderadults$inpov_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in poverty: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else 
      # if(var_old() == "labfor")
    {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$labfor,
                     "_f" = olderadults$labfor_f,
                     "_m" = olderadults$labfor_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in the labor force: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  output$householdplot <- renderLeaflet({
    if(var_hh() == "hhsixty_total") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_total,
                     "_f" = olderadults$hhsixty_total,
                     "_m" = olderadults$hhsixty_total)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_total, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_total, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_total),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_total),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_fhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_fhh,
                     "_f" = olderadults$hhsixty_fhh,
                     "_m" = olderadults$hhsixty_fhh)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_fhh, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a female 60+ member:</strong>",
              round(olderadults$hhsixty_fhh, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_fhh),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_fhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_mhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_mhh,
                     "_f" = olderadults$hhsixty_mhh,
                     "_m" = olderadults$hhsixty_mhh)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_mhh, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a male 60+ member: </strong>",
              round(olderadults$hhsixty_mhh, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_mhh),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_mhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_nonfam") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_nonfam,
                     "_f" = olderadults$hhsixty_nonfam,
                     "_m" = olderadults$hhsixty_nonfam)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_nonfam, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Single housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_nonfam, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_nonfam),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_nonfam),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_marr,
                     "_f" = olderadults$hhsixty_marr,
                     "_m" = olderadults$hhsixty_marr)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_marr, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Married households with a 60+ member: </strong>",
              round(olderadults$hhsixty_marr, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_marr),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_marr),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
  # data and measures table: done ----------------------------------------
  var_topic <- reactive({
    input$topic
  })
  output$datatable <- renderDataTable({
    if(var_topic() == "All Measures"){
      table <- as.data.frame(measures_table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
    else{
      data <- switch(input$topic,
                     "Connectivity Measures" = "connectivity",
                     "Sociodemographic Measures" = "demographics",
                     "Food Access Measures" = "food access",
                     "Health Care Access Measures" = "health",
                     "Older Adult Population Measures" = "older adults")
      table <- subset(measures_table, Topic == data)
      table <- as.data.frame(table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
  })
  
  # device: done ---------------------------------------------------------
  
  output$deviceplot <- renderLeaflet({
    data <- switch(input$devicedrop,
                   "nocomputer" = connectivity$nocomputer,
                   "laptop" = connectivity$laptop,
                   "smartphone" = connectivity$smartphone,
                   "tablet" = connectivity$tablet, 
                   "nointernet" = connectivity$nointernet,
                   "satellite" = connectivity$satellite,
                   "cellular" = connectivity$cellular,
                   "broadband" = connectivity$broadband)
    
    device_spec <- switch(input$devicedrop,
                          "nocomputer" = "no computer",
                          "laptop" = "laptop",
                          "smartphone" = "smartphone",
                          "tablet" = "tablet", 
                          "nointernet" = "no internet access",
                          "satellite" = "satellite internet",
                          "cellular" = "cellular internet",
                          "broadband" = "broadband internet")
    
    pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            connectivity$NAME.y,
            "<br />",
            "<strong>% Households with",
            device_spec,
            "access: </strong>",
            round(data, 2)),
      htmltools::HTML
    )
    
    leaflet(data = connectivity, options = leafletOptions(minZoom = 10))%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(data), 
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(data),
                title = "Percent by<br>Quintile Group",
                opacity = 0.7,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  
  # wifi: done -----------------------------------------------------------
  
  # Iso selector
  output$wifiplot <- renderLeaflet({
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    wifi_iso10 <- switch(input$wifidrop,
                         "Meadows of Dan Elementary School" = wifi_iso_10_1,
                         "Woolwine Elementary School" = wifi_iso_10_2,
                         "Patrick Springs Primary School" = wifi_iso_10_3,
                         "Blue Ridge Elementary School" = wifi_iso_10_4,
                         "Patrick County High School" = wifi_iso_10_5,
                         "Stuart Elementary School" = wifi_iso_10_6,
                         "Patrick County Branch Library" = wifi_iso_10_7,
                         "Hardin Reynolds Memorial School" = wifi_iso_10_8,
                         "Stuart Baptist Church" = wifi_iso_10_9,                       
                         "Patrick Henry Community College Stuart Campus" = wifi_iso_10_10)
    
    wifi_iso15 <- switch(input$wifidrop,
                         "Meadows of Dan Elementary School" = wifi_iso_15_1,
                         "Woolwine Elementary School" = wifi_iso_15_2,
                         "Patrick Springs Primary School" = wifi_iso_15_3,
                         "Blue Ridge Elementary School" = wifi_iso_15_4,
                         "Patrick County High School" = wifi_iso_15_5,
                         "Stuart Elementary School" = wifi_iso_15_6,
                         "Patrick County Branch Library" = wifi_iso_15_7,
                         "Hardin Reynolds Memorial School" = wifi_iso_15_8,
                         "Stuart Baptist Church" = wifi_iso_15_9,                       
                         "Patrick Henry Community College Stuart Campus" = wifi_iso_15_10)
    
    data <- switch(input$wifidrop,
                   "Meadows of Dan Elementary School" = 1,
                   "Woolwine Elementary School" = 2,
                   "Patrick Springs Primary School" = 3,
                   "Blue Ridge Elementary School" = 4,
                   "Patrick County High School" = 5,
                   "Stuart Elementary School" = 6,
                   "Patrick County Branch Library" = 7,
                   "Hardin Reynolds Memorial School" = 8,
                   "Stuart Baptist Church" = 9,                       
                   "Patrick Henry Community College Stuart Campus" = 10)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            wifi_latlong[data, ]$name,
            "<br />",
            "<strong>Address:</strong>",
            wifi_latlong[data, ]$fulladdress,
            "<br />",
            "<strong>Notes:</strong>",
            wifi_latlong[data, ]$notes),
      htmltools::HTML
    )
    
    m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = wifi_iso10, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrone") %>%
      addPolygons(data = wifi_iso15,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrone") %>%
      addMarkers(data = wifi_latlong, ~longitude[data], ~latitude[data],
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("10 Minute Isochrone",
                          "15 Minute Isochrone",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
    m1 
  })
  
  # Coverage table
  output$wifitable <- renderTable({
    data <- switch(input$wifidrop,
                   "Meadows of Dan Elementary School" = 1,
                   "Woolwine Elementary School" = 2,
                   "Patrick Springs Primary School" = 3,
                   "Blue Ridge Elementary School" = 4,
                   "Patrick County High School" = 5,
                   "Stuart Elementary School" = 6,
                   "Patrick County Branch Library" = 7,
                   "Hardin Reynolds Memorial School" = 8,
                   "Stuart Baptist Church" = 9,                       
                   "Patrick Henry Community College Stuart Campus" = 10)
    
    table <- read.csv(paste0("data/isochrones/tables/wifi_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # Wifi deserts
  output$allwifi <- renderLeaflet({
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            wifi_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            wifi_latlong$fulladdress,
            "<br />",
            "<strong>Notes:</strong>",
            wifi_latlong$notes),
      htmltools::HTML
    )
    
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .5, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = wifi_iso_10_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_10, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = wifi_latlong,
                 group = "Free Wi-Fi Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("Free Wi-Fi Locations",
                          "Residential Properties"),
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  output$allwifitable <- renderTable({
    table <- read.csv("data/isochrones/tables/wifi_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # ems: done ------------------------------------------------------------
  
  output$emsplot <- renderLeaflet({
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    ems_iso8 <- switch(input$emsdrop,
                       "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_1,
                       "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_2,                                                         
                       "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_8_3,                                                                   
                       "VESTA RESCUE SQUAD" = ems_iso_8_4,                                                                                           
                       "ARARAT RESCUE SQUAD" = ems_iso_8_5,                                                                                          
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_8_6,
                       "JEB STUART RESCUE SQUAD" = ems_iso_8_7,                                                                                      
                       "SMITH RIVER RESCUE SQUAD" = ems_iso_8_8,                                                                                     
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_8_9)
    
    ems_iso10 <- switch(input$emsdrop,
                        "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_1,
                        "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_2,                                                         
                        "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_10_3,                                                                   
                        "VESTA RESCUE SQUAD" = ems_iso_10_4,                                                                                           
                        "ARARAT RESCUE SQUAD" = ems_iso_10_5,                                                                                          
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_10_6,
                        "JEB STUART RESCUE SQUAD" = ems_iso_10_7,                                                                                      
                        "SMITH RIVER RESCUE SQUAD" = ems_iso_10_8,                                                                                     
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_10_9)
    
    ems_iso12 <- switch(input$emsdrop,
                        "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_1,
                        "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_2,                                                         
                        "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_12_3,                                                                   
                        "VESTA RESCUE SQUAD" = ems_iso_12_4,                                                                                           
                        "ARARAT RESCUE SQUAD" = ems_iso_12_5,                                                                                          
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_12_6,
                        "JEB STUART RESCUE SQUAD" = ems_iso_12_7,                                                                                      
                        "SMITH RIVER RESCUE SQUAD" = ems_iso_12_8,                                                                                     
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_12_9)
    
    data <- switch(input$emsdrop,
                   "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                   "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,                                                         
                   "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,                                                                   
                   "VESTA RESCUE SQUAD" = 4,                                                                                           
                   "ARARAT RESCUE SQUAD" = 5,                                                                                          
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                   "JEB STUART RESCUE SQUAD" = 7,                                                                                      
                   "SMITH RIVER RESCUE SQUAD" = 8,                                                                                     
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            str_to_title(ems[data, ]$NAME),
            "<br />",
            "<strong>Address:</strong>",
            str_to_title(ems[data, ]$ADDRESS), ",", str_to_title(ems[data, ]$CITY), ", VA", ems[data, ]$ZIP,
            "<br />",
            "<strong>Type:</strong>",
            str_to_title(ems[data, ]$NAICSDESCR)),
      htmltools::HTML
    )
    
    m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = ems_iso8, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrone") %>%
      addPolygons(data = ems_iso10,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrone") %>%
      addPolygons(data = ems_iso12,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrone") %>%
      addMarkers(data = ems, ~LONGITUDE[data], ~LATITUDE[data],
                 group = "EMS Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("8 Minute Isochrone",
                          "10 Minute Isochrone",
                          "12 Minute Isochrone",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
    m1 
  })
  
  output$emstable <- renderTable({
    data <- switch(input$emsdrop,
                   "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                   "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,                                                         
                   "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,                                                                   
                   "VESTA RESCUE SQUAD" = 4,                                                                                           
                   "ARARAT RESCUE SQUAD" = 5,                                                                                          
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                   "JEB STUART RESCUE SQUAD" = 7,                                                                                      
                   "SMITH RIVER RESCUE SQUAD" = 8,                                                                                     
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)
    
    
    table <- read.csv(paste0("data/isochrones/tables/ems_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # EMS deserts
  output$allems <- renderLeaflet({
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            str_to_title(ems$NAME),
            "<br />",
            "<strong>Address:</strong>",
            paste0(str_to_title(ems$ADDRESS), ", ", str_to_title(ems$CITY), ", VA ", ems$ZIP),
            "<br />",
            "<strong>Type:</strong>",
            str_to_title(ems$NAICSDESCR)),
      htmltools::HTML
    )
    
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .5, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = ems_iso_8_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addMarkers(data = ems,
                 group = "EMS Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("8 Minute Isochrones",
                       "10 Minute Isochrones",
                       "12 Minute Isochrones"),
        overlayGroups = c("EMS Locations",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  output$allemstable <- renderTable({
    table <- read.csv("data/isochrones/tables/ems_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  
  # usda - lahunv10share  -----------------------------------------------------------
  var_usda <- reactive({
    input$usdadrop
  })
  output$usdaplot <- renderLeaflet({
    data <- switch(input$usdadrop,
                   "lakids1share" = usda$lakids1share,
                   "lakids10share" = usda$lakids10share,
                   "lalowi1share" = usda$lalowi1share,
                   "lalowi10share" = usda$lalowi10share,
                   "lapop1share" = usda$lapop1share,  
                   "lapop10share" = usda$lapop10share,
                   "laseniors1share" = usda$laseniors1share,
                   "laseniors10share" = usda$laseniors10share)
    
    usda_spec <- switch(input$usdadrop,
                        "lakids1share" = "low food access for children at 1 mile",
                        "lakids10share" = "low food access for children at 10 miles",
                        "lalowi1share" = "low food access for low income population at 1 mile",
                        "lalowi10share" = "low food access for low income population at 10 miles",
                        "lapop1share" = "low food access at 1 mile",  
                        "lapop10share" = "low food access at 10 miles",
                        "laseniors1share" = "low food access for seniors at 1 mile",
                        "laseniors10share" = "low food access for seniors at 10 miles")
    
    pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            usda$NAME.y,
            "<br />",
            "<strong>% Population with",
            usda_spec,
            round(data, 2)),
      htmltools::HTML
    )
    
    leaflet(data = usda, options = leafletOptions(minZoom = 10))%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(data), 
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(data),
                title = "Percent by<br>Quartile Group",
                opacity = 0.7,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  # grocery --------------------------------------------------------
  
  # Iso selector
  output$grocplot <- renderLeaflet({
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    groc_iso10 <- switch(input$grocdrop,
                         "Mountain Meadow Farm and Craft Market" = grc_iso_10_1,
                         "Lowes Foods of Stuart" = grc_iso_10_2,
                         "Patrick County Local Farmers Market" = grc_iso_10_3,
                         "Stuart Farmers Market" = grc_iso_10_4,                
                         "W & W Produce" = grc_iso_10_5,
                         "Walmart Supercenter" = grc_iso_10_6,
                         "Poor Farmers Farm" = grc_iso_10_7)
    
    groc_iso15 <- switch(input$grocdrop,
                         "Mountain Meadow Farm and Craft Market" = grc_iso_15_1,
                         "Lowes Foods of Stuart" = grc_iso_15_2,
                         "Patrick County Local Farmers Market" = grc_iso_15_3,
                         "Stuart Farmers Market" = grc_iso_15_4,                
                         "W & W Produce" = grc_iso_15_5,
                         "Walmart Supercenter" = grc_iso_15_6,
                         "Poor Farmers Farm" = grc_iso_15_7)
    
    data <- switch(input$grocdrop,
                   "Mountain Meadow Farm and Craft Market" = 1,
                   "Lowes Foods of Stuart" = 2,
                   "Patrick County Local Farmers Market" = 3,
                   "Stuart Farmers Market" = 4,                
                   "W & W Produce" = 5,
                   "Walmart Supercenter" = 6,
                   "Poor Farmers Farm" = 7)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            groceries_latlong[data, ]$name,
            "<br />",
            "<strong>Address:</strong>",
            groceries_latlong[data, ]$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            groceries_latlong[data, ]$type),
      htmltools::HTML
    )
    
    m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = groc_iso10, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrone") %>%
      addPolygons(data = groc_iso15,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrone") %>%
      addMarkers(data = groceries_latlong, ~longitude[data], ~latitude[data],
                 group = "Fresh Food Location",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("15 Minute Isochrone",
                          "10 Minute Isochrone",
                          "Residential Properties",
                          "Fresh Food Location"),
        options = layersControlOptions(collapsed = FALSE))
    m1 
  })
  
  # Grocery table
  output$groctable <- renderTable({
    data <- switch(input$grocdrop,
                   "Mountain Meadow Farm and Craft Market" = 1,
                   "Lowes Foods of Stuart" = 2,
                   "Patrick County Local Farmers Market" = 3,
                   "Stuart Farmers Market" = 4,                
                   "W & W Produce" = 5,
                   "Walmart Supercenter" = 6,
                   "Poor Farmers Farm" = 7)
    
    table <- read.csv(paste0("data/isochrones/tables/grc_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # Food deserts
  output$allgroc <- renderLeaflet({
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            groceries_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            groceries_latlong$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            groceries_latlong$type),
      htmltools::HTML
    )
    
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .5, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = grc_iso_10_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = groceries_latlong,
                 group = "Fresh Food Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        overlayGroups = c("Residential Properties",
                          "Fresh Food Locations"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  # Other food resources
  output$othermap <- renderLeaflet({
    
    pal <- colorFactor(c("#0E879C", "#D9E12B", "#E6A01D"), domain = otherfood$type)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            otherfood$name,
            "<br />",
            "<strong>Address:</strong>",
            otherfood$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            otherfood$type,
            "<br />",
            "<strong>Open to:</strong>",
            otherfood$audience,
            "<br />",
            "<strong>Notes:</strong>",
            otherfood$notes),
      htmltools::HTML
    )
    
    leaflet(data = otherfood,
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = patrickborder, stroke = T, weight = 2, color = "grey", fillOpacity = 0) %>%
      addCircleMarkers(data = otherfood,
                       stroke = FALSE,
                       fillOpacity = 1,
                       color = ~pal(type),
                       radius = 7,
                       opacity = 1,
                       label = labels,
                       labelOptions = labelOptions(direction = "bottom",
                                                   style = list(
                                                     "font-size" = "12px",
                                                     "border-color" = "rgba(0,0,0,0.5)",
                                                     direction = "auto"))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~type,
                title = "Type",
                opacity = 0.9)
  })
  
  output$allgrctable <- renderTable({
    table <- read.csv("data/isochrones/tables/grc_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
}

shinyApp(ui = ui, server = server)