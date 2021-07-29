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
library(mapview)
library(dplyr)
library(tidycensus)
library(sp)
library(readxl)
library(tigris)
library(shinyjs)
library(rgdal)
library(broom)
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
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem(
      tabName = "overview",
      text = "Project Overview",
      icon = icon("globe-africa")),
    menuItem(
      tabName = "data",
      text = "Data & Methodology",
      icon = icon("database"), badgeLabel = "data", badgeColor = "green"),
    menuItem(
      tabName = "MPI",
      text = "MPI",
      icon = icon("map-marked-alt"), badgeLabel = "data", badgeColor = "green"),
    menuItem(
      "Poverty Index",
      tabName = 'M0'
    ),
    menuItem(
      "Adjusted Poverty Gap",
      tabName = "M1"),
    menuItem(
      "Adjusted Poverty Severity",
      tabName = "M2"),
    menuItem(
      tabName = "team",
      text = "Team",
      icon = icon("user-friends"))
  )
)



# Body ----------------------------------------------------------
body <- dashboardBody(
  tabItems(
    
    ## Tab Introduction to Zimbabwe --------------------------------------------
    tabItem(tabName = "overview",
            #fluidRow(
            box(
              title = "Introduction to Zimbabwe",
              closable = FALSE,
              width = NULL,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              img(src = "Zimbabwe_Flag.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
              
              h2("Country Briefing"),
              br(),
              br(),
              p("Text"),
              br(),
              br(),
              h2("Recent History"),
              p("In the first decade of the 21st century, Zimbabwe suffered from significant hyperinflation resultant of an overall government budget deficit and a simultaneous period of monetary policy that increased the amount of money in circulation. This hyperinflation, in turn, led to economic crisis as foreign investment dropped and Zimbabwean currency eventually crashed. In 2009, Zimbabwe was dollarized in an effort to mitigate inflation. Although this move was relatively successful at stabilizing the economy, the effects of economic strife still linger throughout the country. A money metric approach to defining poverty is understandably insufficient in this case due to the extreme discrepancies between Zimbabwe’s modern currency and its antiquated currency. Additionally, variations in consumption, prices, and household income distribution can make it difficult to provide an accurate account of money metric poverty as the value of money is hardly standardized.")
              
            ),
            
            box(
              title = ("Data Science for the Public Good"),
              closable = FALSE,
              width = NULL,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              h2("Potential Application of a Multidimensional Poverty Index"),
              p("To address these shortcomings of typical poverty analysis, Alkire-Foster developed methodology requisite for Multidimensional Poverty Indices (MPIs). An MPI is designed to account for such discrepancies by interpreting poverty as the inability to satisfy a certain list of needs. In this way, MPIs allow for an encompassing assessment of poverty that is applicable regardless of the predictability, or lack thereof, of money. This feature is especially helpful when measuring poverty in Zimbabwe due to the recent volatility of the country’s economy. Due to the demonstrated utility and applicability of such indexes, the DSPG Zimbabwe team has been tasked with creating an MPI that will accurately measure poverty in Zimbabwe and to map the calculated values across the country’s districts. The final result will include an interactive visualization of poverty in Zimbabwe as it exists in multiple dimensions, incidences, and intensities. ")
            ),

            box(
              title = "References",
              closable = FALSE,
              width = NULL,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p("Alkire, S., & Santos, M. “Measuring Acute Poverty in the Developing World: Robustness and Scope of the Multidimensional Poverty.” Index World Development 59 (2014): 251-274."),
              p("Coomer, J., & Gstraunthaler. “The Hyperinflation in Zimbabwe.” Quarterly journal of Australian economics 14.3 (2011): 311-346.")
              
            ),
            
    ),
    
    ## Tab Data & Methodology--------------------------------------------
    tabItem(tabName = "data",
            #fluidRow(
            box(
              title = "Data",
              closable = FALSE,
              width = NULL,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              tabBox(
                title = NULL, width = 16, height = "auto",
                tabPanel("PICES Overview", 
                         img(src = "zimstat_logo.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                         p("To gather data necessary for MPI construction, the DSPG team utilized the 2017 Poverty, Income, Consumption and Expenditure Survey (PICES) administered by the Zimbabwe National Statistics Agency (ZimStat). The country-wide survey is conducted every five years as a means of collecting comprehensive information regarding demographics, overall standards of living, poverty levels, and disparities between socio-economic groups. This data is categorized along individual, household, district, and country levels. The PICES Questionnaire is comprised of various modules that collect respondent input. The 2017 iteration included modules focused on population and household characteristics, household economy, household incomes, agriculture output and input, informal sector activities, international migration, and disability. These modules, completed by survey respondents in the civilian sector, provide insight on the general state of the population and will be used by our team to understand specific aspects of poverty in Zimbabwe."),
                         h3("References:"),
                         p("Zimbabwe National Statistics Agency. “Poverty, Income, Consumption and Expenditure Survey 2017 Report.” (2018): 1-160.")),
                tabPanel("Sampling"),
                tabPanel("Weights"))
            ),
            
            #Methodology box
            box(
              title = "Methodology Overview",
              closable = FALSE,
              width = NULL,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              tabBox(
                title = NULL, width = 16, height = "auto",
                tabPanel("Multidimensional Poverty Index",
                         h2("MPI Overview"),
                         p("Work produced by the DSPG team will emulate that of Stoeffler, et al. which constructed an MPI and utilized 2001, 2007, and 2011-2012 PICES data to track Zimbabwean poverty over time in a 2015. Following their lead, our MPI will consist of eight dimensions and fourteen variables that indicate a populations status in said dimension. Each dimension and variable is weighted on the grounds of how impactful it is to the wellbeing of either the individual or the household. 
The relevant dimensions, their respective variables, and the designated weights of the MPI can be found under the ‘Tables’ tab below:
")),
                tabPanel("Measurements",
                         h2("Measurements"),
                         p("A deprivation count, k, falling between k = 1 and k = 4 is used as a threshold for determining poverty. Those who exceed or equal threshold k will be considered poor while those who do not exceed or equal threshold k will be considered not poor and thus will not be included. To achieve this result, an Alkire-Foster  ‘counting approach’ will be employed and poverty will be assigned only to those whose weighted sum deprivation count is greater than k. This creates a double cut-off in which only those who are deprived in multiple dimensions are considered and those who are deprived in a single dimension are non-factors. Once these impoverished populations are determined, three main measurements can be calculated."),
                         h3("M0"),
                         p("M0 is the primary method for determining a population’s poverty. It is calculated by multiplying the Headcount Ratio, H, with the Average Deprivation Share. The Headcount Ratio is the number of people who are considered poor based on a threshold, k. The Average Deprivation Share is the number of actual deprivations collected from each dimension divided by the number of potential deprivations that the state could have. In our case, this is the sum of the weights of each dimension multiplied by the population of the country."),
                         h3("M1"),
                         p("M1 measures what is called the Adjusted Poverty Gap. This measure is obtained by taking the average of the gaps of poor individuals below some poverty line, k. If the individual is above this threshold, k, their poverty gap is zero. Otherwise, it is always positive. This ensures that the needs of poor people are not skewed by wealthier counterparts."),
                         h3("M2"),
                         p("M2 is a measure of the Adjusted Poverty Severity. This measure is obtained by taking the average of the square of the gaps of poor individuals below some poverty line, k. It is very similar to M1, the only difference is that the poverty gap is squared. The quadratic nature of this measurement helps to give more weight to the people who are significantly below the poverty line as opposed to those who fall just beneath it."),
                         p("The formulas needed to calculate these measures are indicated in the 'Formulas' tab below:")),
                tabPanel("Formulas",
                         img(src = "zimbabwe_formulas.png", height="700", width="500", alt="Image", style="float: middle; margin: 10px 10px 125px 125px; border: 5px solid #000000;"),
                         p("Source: Source: Stoeffler, Quentin, et al. “Multidimensional poverty in crisis: Lessons from Zimbabwe.” The journal of development studies 52.3 (2016): 428-446.")),
                tabPanel("Variables and Dimensions",
                         img(src = "zimbabwe_var_dim.png", height="700", width="500", alt="Image", style="float: middle; margin: 10px 10px 125px 125px; border: 5px solid #000000;"),
                         p("Source: Source: Stoeffler, Quentin, et al. “Multidimensional poverty in crisis: Lessons from Zimbabwe.” The journal of development studies 52.3 (2016): 428-446."))
                         
            )),
            
            ####Methodology references
            box(
              title = "References",
              closable = FALSE,
              width = NULL,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p("Stoeffler, Quentin, et al. “Multidimensional poverty in crisis: Lessons from Zimbabwe.” The journal of development studies 52.3 (2016): 428-446.")
            ),
            
    ),
           
    ## Tab 3 MPI--------------------------------------------
    tabItem(tabName = "MPI",
            fluidRow(
              h2("MPI"),
              p("To understand the full suite of amenities available to HGBs in Wythe, 
                we used publicly available demographic and infrastructure data to provide 
                an overview of the built capital amenities in Wythe."),
              p("In many respects, Wythe County is uniquely endowed with built amenities 
                attractive to businesses (William and Lamb, 2010).  It is situated at the 
                intersection of two major interstates, and it is within a six-to-eight-hour 
                drive of most of the population in the United States. As the map shows, 
                it also has easy access to rail and other supporting infrastructure (e.g., powerplants) 
                for commerce and manufacturing. From an “access to major markets” perspective, 
                Wythe is an attractive location for both light and heavy industry."),
              box(
                title = "Loudoun County Programs/Services",
                closable = FALSE,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = NULL,
                #enable_dropdown = TRUE,
                #dropdown_icon = "",
                #dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Level of Education","Industry","Home Values","Household Income","Household Size"))),
                #leafletOutput("wythe_infrastructure"),
                
                
                tabBox(
                  title = NULL , width = 16,
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "350px",
                  tabPanel("All", 
                           sidebarLayout(
                              sidebarPanel(
                                selectInput("pillar_variable", "Pillar Variable:",
                                            c("Education", "Employment", "Housing","Insurance","Transportation","Policy and Funding","All")),
                                ### Can add more inputs????
                                # selectInput("time_variable", "Time Variable:",
                                #             c("60 minutes" = "60",
                                #               "45 minutes" = "45",
                                #               "30 minutes" = "30"))
                                             ),
                            # Show a plot of the generated distribution
                            mainPanel(
                              tableOutput("label_1"),
                              leafletOutput("mapplot_1"),
                              #mapview:::plainViewOutput("test")
                                   )
                                          )
                  
                          ),
                  
                  tabPanel("Juvenile",
                           sidebarPanel(
                             selectInput("pillar_variable", "Pillar Variable:",
                                         c("Education", "Employment", "Housing","Insurance","Transportation","Policy and Funding","All"))
                             
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             tableOutput("label_2"),
                             leafletOutput("mapplot_2")
                             #mapview:::plainViewOutput("test")
                           )
                           
                  ),
                  
                  tabPanel("Foster Care",
                           sidebarPanel(
                             selectInput("pillar_variable", "Pillar Variable:",
                                         c("Education", "Employment", "Housing","Insurance","Transportation","Policy and Funding","All"))
                             
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             tableOutput("label_3"),
                             leafletOutput("mapplot_3")
                             #mapview:::plainViewOutput("test")
                           )
                           
                  )                   
                  
                  
                  
                  
                )
                
          
                
                
              ),

            )),
    ## Tab 8 Team--------------------------------------------
     tabItem(tabName = "team",
            fluidRow(
              box(
                title = "Team",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                h2("Data Science for the Public Good Program"),
                p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at the Biocomplexity Institute’s Social and Decision Analytics Division (SDAD). In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply, and our annual symposium, please visit the official Biocomplexity DSPG website."),
                h2("2021 Loudoun County Summer Project"),
                p("Our project goal was to identify industries and the code that are expected to grow rapidly in the future. We visualized these code by the skills, education, experience and training needed to do them. We then created measures to visualize and assess the ability of Wythe County and the surrounding region to respond to training the workers of tomorrow. Our team is comprised of talented individuals with a broad range of skills and experience."),
                h2("DSPG Team Members"),
                img(src = 'Josh.Beverly.VT.jpg', height = "150", width = "140", align = "center"),
                img(src = 'Dylan.Glover.VT.jpg', height = "150", width = "140", align = "center"),
                img(src = 'Afrina.Tabassum.VT.jpg', height = "150", width = "140", align = "center"),
                img(src = 'Adam.Wells.VT.jpg', height = "150", width = "140", align = "center"),
                br(),
                br(),
                p("Yang Cheng, Fellow (Ph.D. Student at Virginia Tech, )"),
                p("JaiDa Robinson, Fellow (Ph.D. Student at Virginia State University, )"),
                p("Julie Rebstock, Intern (Undergraduate Student at Virginia Tech, Computational Modeling and Data Anaylytics and Economics)"),
                p("Austin Burcham, Intern (Undergraduate. Student at Virginia Tech, )"),
                p("Kyle Jacobs, Intern (Undergraduate Student at Virginia State University,)"),
                h2("Virginia Tech Faculty Team Members"),
                img(src = 'Susan.Chen.VT.jpg', height = "150", width = "140", align = "center"),
                img(src = 'Conaway.Haskins.VT.jpg', height = "150", width = "140", align = "center"),
                img(src = 'Matt.Holt.VT.jpg', height = "150", width = "140", align = "center"),
                img(src = 'Ford.Ramsey.VT.jpg', height = "150", width = "140", align = "center"),
                br(),
                br(),
                p("Susan Chen (Associate Professor, Food and Health Economics, DSPG Project Co-Lead)"),
                p("Chanita Homles ()"),
                p("Isabel Bradburn ()"),
                h2("Project Sponsors"),
                img(src = 'VCE.Logo.png', height = "150", width = "200", align = "center", style="display: block; margin-left: auto; margin-right: auto;"),
                p("Matthew Miller (Unit Coordinator and Extension Agent, Agriculture and Natural Resources - Farm Business Management)"),
                
                h2("Acknowledgements"),
                p("We would like to thank:"),
                p(" (),"),
                p(" ()"),
                p(" ()")
              )
            )),
    tabItem(
      tabName = "M0",
      # Everything has to be put in a row or column
      fluidPage(
        # Change the theme
        theme = shinytheme('superhero'),
        # Make a box with a plot inside of it
        box(
          title = "Multidimensional Poverty Index (By Province)",
          plotOutput("M0_plot", height = 300, width = 400),
          width = 12
        ),
        
        box(
          
          title = 'Deprivation Cutoff',
          sliderInput("slider0", "K-Threshold Value", 1, 9, 3),
          width = 12))),
    
    tabItem(
      tabName = "M1",
      
      fluidPage(
        box(
          title = "Adjusted Poverty Gap (By Province)",
          plotOutput("M1_plot", height = 300, width = 440),
          width = 12
        ),
        box(
          title = "Deprivation Cutoff",
          sliderInput("slider1", "K-Threshold Value", 1, 9, 3),
          width = 12
        )
      )
    ),
    
    tabItem(
      tabName = "M2",
      
      fluidPage(
        box(
          title = "Adjusted Poverty Severity (By Province)",
          plotOutput("M2_plot", height = 300, width = 480),
          width = 12
        ),
        box(
          title = "Deprivation Cutoff",
          sliderInput("slider2", "K-Threshold Value", 1, 9, 3),
          width = 12
        )
       )
      )
    )
  )



# UI--------------------------
ui <- dashboardPage(
  skin = 'midnight',
    dashboardHeader(title = "Zimbabwe(Draft)"),
    sidebar = sidebar,
    body = body)




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
