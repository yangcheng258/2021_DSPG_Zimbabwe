# Set Working Directory
setwd("/Users/mattb24/Zimbabwe/2021_DSPG_Zimbabwe/ShinyApp/ShinyAppStuff")

# clean the memory
rm(list=ls())


library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(shinydashboardPlus)
library(leaflet)
library(dashboardthemes)
library(readr)
library(collapsibleTree)
library(tidyverse)
library(viridis)
library(sf)
library(gpclib)
library(maptools)
library(ggpolypath)
library(mapview)
library(dplyr)
library(tidycensus)
library(sp)
library(readxl)
library(tigris)
library(shinyjs)
library(rgdal)
library(broom)
library(fresh)
#gclibPermit()
#library(RColorBrewer)
#library(osmdata)
#library(purrr)
#library(osrm)
#library(rmapzen)
#library(rgdal)
#library(ggplot2)
#library(scales)
#library(nycflights13)


#install.packages("rsconnect")
#library(rsconnect)
#rsconnect::deployApp('~/git/WytheFinalDash/ShinyFinalPresentation', account = "wythecountydash")
#rsconnect::deployApp('path/to/your/app')



# source("theme.R")
# #Get Data


## SETTING UP MPI Data

# Loads in the shapefile
ZimMap <- readOGR(dsn = paste0(getwd(),"/ProvinceShapes"), layer="zwe_admbnda_adm1_zimstat_ocha_20180911")

# Loading the MPI data and combining
id <- ZimMap@data[["ADM1_EN"]]
MPIData = read.csv(file = 'ProvinceData.csv')

# Rename the district to id
colnames(MPIData)[1] <- "id"

### 'fortify' the data to get a dataframe format required by ggplot2  By Yang
ZimMap_fortified <- tidy(ZimMap, region = "ADM1_EN")

# Currently we need to manually merge the two together
datapoly <- merge(ZimMap_fortified, MPIData , by = c("id"))



# Siderbar(LEFT) ----------------------------------------------------------
#sidebar <- dashboardSidebar(
#  sidebarMenu(
#    id = "tabs",
#    menuItem(
#      tabName = "overview",
#      text = "Project Overview",
#      icon = icon("globe-africa")),
#   menuItem(
#      tabName = "data",
#     text = "Data & Methodology",
#    icon = icon("database"), badgeLabel = "data", badgeColor = "green"),
#    menuItem(
#      tabName = "MPI",
#      text = "MPI Data Tables",
#      icon = icon("map-marked-alt"), badgeLabel = "data", badgeColor = "green"),
#    menuItem(
#      "Poverty Index",
#      tabName = 'M0'
#    ),
#    menuItem(
#      "Adjusted Poverty Gap",
#      tabName = "M1"),
#    menuItem(
#      "Adjusted Poverty Severity",
#      tabName = "M2"),
#    menuItem(
#      tabName = "team",
#      text = "Team",
#      icon = icon("user-friends"))
#  )
#)



# Body ----------------------------------------------------------
ui <- navbarPage(title = "DSPG 2021 Zimbabwe Project",
                 theme = shinytheme("lumen"),
                 selected = "overview",
                 useShinyjs(),
  tabPanel("Project Overview", value = "overview",
            fluidRow(style = "margin: 2px;",
                     align = "center",
                     
            # 
            # box(
            #   title = "Introduction to Zimbabwe",
            #   closable = FALSE,
            #   width = NULL,
            #   status = "success",
            #   solidHeader = TRUE,
            #   collapsible = TRUE,
              
              
            # img(src = "Zimbabwe_Flag.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
              br(""),
              h1("Country Overview"),
              p("Nestled in the Southeastern tip of Africa, Zimbabwe neighbors South Africa, Mozambique, Zambia, and Botswana. Zimbabwe gained independence from Great Britain in 1980 and was ruled by Prime Minister and eventually President Robert Mugabe until his resignation in 2017. Presently, Emmerson Mnangagwa holds office. The country is home to roughly 14,830,000 inhabitants, 10% of whom live in the capital city of Harare. Although large agglomerations exist in other major urban areas including Bulawayo and Chitungwiza, population distribution is relatively even otherwise. Zimbabwe’s central government is responsible for regulating its 10 provinces and 59 further subdivided districts. Zimbabwe’s terrain consists mostly of plateau upon which forests thrive and arable land is plenty. Because of this, 67.5% of the labor force works in agriculture growing sugar cane, tobacco, fruits, and vegetables among other things. Another 7.3% of the labor force takes advantage of the Zimbabwe’s rich natural resources and participates in the industry sector mining and exporting coal, gold, platinum copper, and other metals as well as manufacturing wood products, cement, chemicals, fertilizer, and food. Despite being relatively well-educated and extremely literate, the population suffers from both unemployment and severe underemployment in which individuals are overqualified for the jobs they have or are not given adequate work hours. In combination with ubiquitous low wages, this creates an obstacle for both economic growth and poverty reduction efforts. Monetary poverty measures in 2017 revealed roughly 63% of Zimbabwean households lived in poverty. This is reflected in significant income inequality, overall low standards of living, malnourishment, low life expectancy, high rates of infant/maternal mortality, and difficulty accessing health and education resources."),
              h1("Recent History"),
              p("To better situate the state of modern Zimbabwe, it is important to understand the country's recent history. After gaining independence in 1980, there was widespread hope that the economic and labor exploitation Africans suffered at the hands of an imperial Great Britain would diminish. While initial trends were encouraging, this hope dwindled as a multitude of factors sent the Zimbabwean economy into decline. Most prominent among these factors was inconsistent policy put forth by the central government which resulted in vague and evolving strategies on combatting poverty. An initial scientific socialist policy was applied between 1980 and 1990 to address poverty but was ineffective and thus abandoned due to financial downturn and prolonged drought which forced agricultural workers into the cities where they faced even greater poverty due to unemployment. In an attempt to revamp the economy, Zimbabwe sought help from the International Monetary Fund (IMF) and the World Bank (WB) which meant an adoption of more capitalistic policy. The costs of necessities including food, water, and education went up as a result, harming and expanding the already existing poor population. The late 1990’s and 2000’s brought ever greater poverty and financial distress to Zimbabwe as a continuing government budget deficit mixed with a fiscal policy focused on increasing the amount of money in circulation resulted in hyperinflation. In turn, this increased the economic crisis as foreign investment dropped and Zimbabwean currency crashed. During this time, unemployment skyrocketed and a massive informal sector of the economy emerged. In 2009, Zimbabwe adopted the U.S. dollar along with a handful of other currencies. Though this move somewhat stabilized the economy at first, a 2013 shift in government rendered these efforts futile. By 2017, inflation increased significantly as did overall economic crisis and poverty."),
              h4("References:")
              ),
            
            box(
              title = ("Data Science for the Public Good"),
              closable = FALSE,
              width = NULL,
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              h4("Application of a Multidimensional Poverty Index"),  
              p("A brief introduction to Zimbabwe makes clear the severity and the urgency of the poverty situation. Although a money metric approach to measuring poverty is historically prevalent, this sort of strategy is unable to accurately paint an accurate picture of poverty in Zimbabwe. This is most notably due to the extreme hyperinflation the country suffers from. Because the actual value of money is constantly evolving, the importance of monetary wealth accumulation in determining poverty is questionable. Additionally, variations in consumption tendencies, prices of goods and necessities, and household income distribution can make it difficult to provide an accurate account of money metric poverty as the value of money is hardly standardized. This volatility also renders money metric comparisons of poverty over time futile as the modern value of currency is incomparable to that of years past. As the practicality of a monetary poverty measure becomes increasingly suspect, the value of alternative poverty measure methods is revealed. 

An Alkire Foster method, developed by Sabina Alkire and James Foster, will be utilized in this project to measure poverty in Zimbabwe. The AF method first denotes the different kinds of deprivations households experience simultaneously. These deprivations make clear who is impoverished in a population and are then used to construct a non-monetary Multidimensional Poverty Index (MPI). MPI’s are powerful insofar as they provide a picture of non-monetary poverty as it exists in its various manifestations. In this way, an MPI accounts for the hyperinflation in Zimbabwe by defining poverty as the inability to satisfy a certain list of needs or capabilities rather than the accumulation of money that may or may not fulfill such needs. The list, as pictured below, is comprised of variables that indicate deprivation. Each variable corresponds to a broader dimension of poverty. These variables and dimensions are normatively chosen to be applicable in the context of Zimbabwe. The MPI created by the 2021 DSPG Zimbabwe team can be utilized to decompose multidimensional poverty as it exists in different subgroups including the national, provincial, and district level. Additionally, the MPI can be deconstructed to analyze at what strength each deprivation is contributing to poverty within groups. By emulating the work of Stoeffler, et al., this MPI can also be used to track changes in multifaceted poverty over time. The combination of these unique aspects of the MPI allows it to be used not only to accurately measure poverty as it exists today, but to evaluate the effectiveness of policy going forward.
"),
              h4("References:"),
              p("")
            )
            
    ),
  
  tabPanel("Data & Methodology", value = "data",
           fluidRow(style = "margin: 2px;",
                    align = "center",
                    
                    
                    
           )
  ),

  tabPanel("Mapping MPI", value = "maps",
           fluidRow(style = "margin: 2px;",
                    align = "center",
                    
                    
                    
            )
           ),
  
  tabPanel("Findings", value = "findings",
           fluidRow(style = "margin: 2px;",
                    align = "center",
                    
                    
                    
           )
  ),
  
  tabPanel("DSPG Team", value = "team",
           fluidRow(style = "margin: 2px;",
                    align = "center",
                    
                    
                    
           )
  )
  )
#     ## Tab 2 Data & Methodology--------------------------------------------
#     tabItem(tabName = "data",
#             fluidPage(
#             box(
#               title = "Data",
#               closable = FALSE,
#               width = NULL,
#               status = "warning",
#               solidHeader = TRUE,
#               collapsible = TRUE,
#               
#               tabBox(
#                 title = NULL, width = 16, height = "auto",
#                 tabPanel("PICES Overview", 
#                          img(src = "zimstat_logo.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
#                          p("To gather data necessary for MPI construction, the DSPG team utilized the 2017 Poverty, Income, Consumption and Expenditure Survey (PICES) administered by the Zimbabwe National Statistics Agency (ZimStat). The country-wide survey is conducted every five years as a means of collecting comprehensive information regarding demographics, overall standards of living, poverty levels, and disparities between socio-economic groups. This data is categorized along individual, household, district, and country levels. The PICES Questionnaire is comprised of various modules that collect respondent input. The 2017 iteration included modules focused on population and household characteristics, household economy, household incomes, agriculture output and input, informal sector activities, international migration, and disability. These modules, completed by survey respondents in the civilian sector, provide insight on the general state of the population and will be used by our team to understand specific aspects of poverty in Zimbabwe."),
#                          h3("References:"),
#                          p("Zimbabwe National Statistics Agency. “Poverty, Income, Consumption and Expenditure Survey 2017 Report.” (2018): 1-160.")),
#                 tabPanel("Sampling"),
#                 tabPanel("Weights"))
#             ),
#             
#             #Methodology box
#             box(
#               title = "Methodology Overview",
#               closable = FALSE,
#               width = NULL,
#               status = "warning",
#               solidHeader = TRUE,
#               collapsible = TRUE,
#               
#               tabBox(
#                 title = NULL, width = "auto", height = "auto",
#                 tabPanel("Multidimensional Poverty Index",
#                          h2("MPI Overview"),
#                          p("Work produced by the DSPG team will emulate that of Stoeffler, et al. which constructed an MPI and utilized 2001, 2007, and 2011-2012 PICES data to track Zimbabwean poverty over time in a 2015. Following their lead, our MPI will consist of eight dimensions and fourteen variables that indicate a populations status in said dimension. Each dimension and variable is weighted on the grounds of how impactful it is to the wellbeing of either the individual or the household. 
# The relevant dimensions, their respective variables, and the designated weights of the MPI can be found under the ‘Tables’ tab below:
# ")),
#                 tabPanel("Measurements",
#                          h2("Measurements"),
#                          p("A deprivation count, k, falling between k = 1 and k = 4 is used as a threshold for determining poverty. Those who exceed or equal threshold k will be considered poor while those who do not exceed or equal threshold k will be considered not poor and thus will not be included. To achieve this result, an Alkire-Foster  ‘counting approach’ will be employed and poverty will be assigned only to those whose weighted sum deprivation count is greater than k. This creates a double cut-off in which only those who are deprived in multiple dimensions are considered and those who are deprived in a single dimension are non-factors. Once these impoverished populations are determined, three main measurements can be calculated."),
#                          h3("M0"),
#                          p("M0 is the primary method for determining a population’s poverty. It is calculated by multiplying the Headcount Ratio, H, with the Average Deprivation Share. The Headcount Ratio is the number of people who are considered poor based on a threshold, k. The Average Deprivation Share is the number of actual deprivations collected from each dimension divided by the number of potential deprivations that the state could have. In our case, this is the sum of the weights of each dimension multiplied by the population of the country."),
#                          h3("M1"),
#                          p("M1 measures what is called the Adjusted Poverty Gap. This measure is obtained by taking the average of the gaps of poor individuals below some poverty line, k. If the individual is above this threshold, k, their poverty gap is zero. Otherwise, it is always positive. This ensures that the needs of poor people are not skewed by wealthier counterparts."),
#                          h3("M2"),
#                          p("M2 is a measure of the Adjusted Poverty Severity. This measure is obtained by taking the average of the square of the gaps of poor individuals below some poverty line, k. It is very similar to M1, the only difference is that the poverty gap is squared. The quadratic nature of this measurement helps to give more weight to the people who are significantly below the poverty line as opposed to those who fall just beneath it."),
#                          p("The formulas needed to calculate these measures are indicated in the 'Formulas' tab below:")),
#                 tabPanel("Formulas",
#                          img(src = "zimbabwe_formulas.png", height="700", width="500", alt="Image", style="float: middle; margin: 10px 10px 125px 125px; border: 5px solid #000000;"),
#                          p("Source: Source: Stoeffler, Quentin, et al. “Multidimensional poverty in crisis: Lessons from Zimbabwe.” The journal of development studies 52.3 (2016): 428-446."))
#                # tabPanel("Variables and Dimensions",
#                #       datatable(Paper_Tables_1))
#                 
#               ))),
#             
#             ####Methodology references
#             box(
#               title = "References",
#               closable = FALSE,
#               width = NULL,
#               status = "warning",
#               solidHeader = TRUE,
#               collapsible = TRUE,
#               
#               p("Stoeffler, Quentin, et al. “Multidimensional poverty in crisis: Lessons from Zimbabwe.” The journal of development studies 52.3 (2016): 428-446.")
#             ),
#             
#     ),
#     
#     ## Tab 3 MPI--------------------------------------------
#     tabItem(tabName = "MPI",
#             fluidPage(
#             box(
#               title = "Changes Over Time",
#               closable = FALSE,
#               width = NULL,
#               status = "warning",
#               solidHeader = TRUE,
#               collapsible = TRUE
#             
#            # tabBox(
#           #  title = NULL, width = "auto", height = "auto",
#           #    tabPanel("Raw Headcount Raio",
#           #           datatable(Paper_Tables_2)),
#           #   tabPanel("MPI Values",
#           #           datatable(Paper_Tables_3)),
#           #   tabPanel("Headcount Ratio and Average Deprivation Share",
#           #            datatable(Paper_Tables_4)),
#           #   tabPanel("Dimension Contribution",
#           #            datatable(Paper_Tables_5))
#             ))),
#               
#     tabItem(
#       tabName = "M0",
#       # Everything has to be put in a row or column
#       fluidPage(
#         # Change the theme
#         theme = shinytheme('superhero'),
#         # Make a box with a plot inside of it
#         box(
#           title = "Multidimensional Poverty Index (By Province)",
#           plotOutput("M0_plot", height = 600, width = 800),
#           width = 12
#         ),
#         
#         box(
#           
#           title = 'Deprivation Cutoff',
#           sliderInput("slider0", "K-Threshold Value", 1, 9, 3),
#           width = 12))),
#     
#     tabItem(
#       tabName = "M1",
#       
#       fluidPage(
#         box(
#           title = "Adjusted Poverty Gap (By Province)",
#           plotOutput("M1_plot", height = 600, width = 800),
#           width = 12
#         ),
#         box(
#           title = "Deprivation Cutoff",
#           sliderInput("slider1", "K-Threshold Value", 1, 9, 3),
#           width = 12
#         )
#       )
#     ),
#     
#     tabItem(
#       tabName = "M2",
#       
#       fluidPage(
#         box(
#           title = "Adjusted Poverty Severity (By Province)",
#           plotOutput("M2_plot", height = 600, width = 800),
#           width = 12
#         ),
#         box(
#           title = "Deprivation Cutoff",
#           sliderInput("slider2", "K-Threshold Value", 1, 9, 3),
#           width = 12
#         )
#       )
#     ),
#     
#     ## Tab 8 Team--------------------------------------------
#     tabItem(tabName = "team",
#             fluidRow(
#               box(
#                 title = "Team",
#                 closable = FALSE,
#                 width = NULL,
#                 status = "warning",
#                 solidHeader = TRUE,
#                 collapsible = TRUE,
#                 h2("Data Science for the Public Good Program"),
#                 p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at the Biocomplexity Institute’s Social and Decision Analytics Division (SDAD). In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply, and our annual symposium, please visit the official Biocomplexity DSPG website."),
#                 h2("2021 Loudoun County Summer Project"),
#                 p("Our project goal was to identify industries and the code that are expected to grow rapidly in the future. We visualized these code by the skills, education, experience and training needed to do them. We then created measures to visualize and assess the ability of Wythe County and the surrounding region to respond to training the workers of tomorrow. Our team is comprised of talented individuals with a broad range of skills and experience."),
#                 h2("DSPG Team Members"),
#                 img(src = 'Josh.Beverly.VT.jpg', height = "150", width = "140", align = "center"),
#                 img(src = 'Dylan.Glover.VT.jpg', height = "150", width = "140", align = "center"),
#                 img(src = 'Afrina.Tabassum.VT.jpg', height = "150", width = "140", align = "center"),
#                 img(src = 'Adam.Wells.VT.jpg', height = "150", width = "140", align = "center"),
#                 br(),
#                 br(),
#                 p("Yang Cheng, Fellow (Ph.D. Student at Virginia Tech, )"),
#                 p("JaiDa Robinson, Fellow (Ph.D. Student at Virginia State University, )"),
#                 p("Julie Rebstock, Intern (Undergraduate Student at Virginia Tech, Computational Modeling and Data Anaylytics and Economics)"),
#                 p("Austin Burcham, Intern (Undergraduate. Student at Virginia Tech, )"),
#                 p("Kyle Jacobs, Intern (Undergraduate Student at Virginia State University,)"),
#                 h2("Virginia Tech Faculty Team Members"),
#                 img(src = 'Susan.Chen.VT.jpg', height = "150", width = "140", align = "center"),
#                 img(src = 'Conaway.Haskins.VT.jpg', height = "150", width = "140", align = "center"),
#                 img(src = 'Matt.Holt.VT.jpg', height = "150", width = "140", align = "center"),
#                 img(src = 'Ford.Ramsey.VT.jpg', height = "150", width = "140", align = "center"),
#                 br(),
#                 br(),
#                 p("Susan Chen (Associate Professor, Food and Health Economics, DSPG Project Co-Lead)"),
#                 p("Chanita Homles ()"),
#                 p("Isabel Bradburn ()"),
#                 h2("Project Sponsors"),
#                 img(src = 'VCE.Logo.png', height = "150", width = "200", align = "center", style="display: block; margin-left: auto; margin-right: auto;"),
#                 p("Matthew Miller (Unit Coordinator and Extension Agent, Agriculture and Natural Resources - Farm Business Management)"),
#                 
#                 h2("Acknowledgements"),
#                 p("We would like to thank:"),
#                 p(" (),"),
#                 p(" ()"),
#                 p(" ()")
#               )
#             ))
#     
#   )
# )



# UI--------------------------
# ui <- dashboardPage(
#   skin = 'midnight',
#   dashboardHeader(title = "Zimbabwe(Draft)"),
#   sidebar = sidebar,
#   body = body)




# Server------------------------------------------
server <- function(input, output, session) {
  output$myplot <- renderPlot({
    gg <- ggplot(data = mtcars, aes(x = mpg, y = disp)) +
      geom_point() 
    
    idx <- input$mytable_rows_selected
    if (!is.null(idx))
      gg + geom_point(size = 5, data = mtcars %>% slice(idx)) 
    else gg
  })
  
  output$mytable <- DT::renderDT({
    mtcars
  })
  
  output$M0_plot <- renderPlot({
    ggplot(datapoly, aes(x=long, y=lat, group = group)) +  
      geom_polygon(aes(fill = switch(input$slider0, 
                                     M0_k1,
                                     M0_k2,
                                     M0_k3,
                                     M0_k4,
                                     M0_k5,
                                     M0_k6,
                                     M0_k7,
                                     M0_k8,
                                     M0_k9), group = id)) + 
      scale_fill_gradient(low='grey', high = 'maroon') +
      labs(fill = "Poverty Index")
  })
  
  output$M1_plot <- renderPlot({
    ggplot(datapoly, aes(x=long, y=lat, group = group)) +  
      geom_polygon(aes(fill = switch(input$slider1, 
                                     M1_k1,
                                     M1_k2,
                                     M1_k3,
                                     M1_k4,
                                     M1_k5,
                                     M1_k6,
                                     M1_k7,
                                     M1_k8,
                                     M1_k9), group = id)) + 
      scale_fill_gradient(low='grey', high = 'maroon') +
      labs(fill = "Adj. Poverty Gap")
  })
  
  output$M2_plot <- renderPlot({
    ggplot(datapoly, aes(x=long, y=lat, group = group)) +  
      geom_polygon(aes(fill = switch(input$slider2, 
                                     M2_k1,
                                     M2_k2,
                                     M2_k3,
                                     M2_k4,
                                     M2_k5,
                                     M2_k6,
                                     M2_k7,
                                     M2_k8,
                                     M2_k9), group = id)) + 
      scale_fill_gradient(low='grey', high = 'maroon') +
      labs(fill = "Adj. Poverty Severity")
  })
}

# Shiny App------------------------
shinyApp(ui = ui, server = server)

