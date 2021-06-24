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



# Siderbar(LEFT) ----------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem(
      tabName = "overview",
      text = "Project Overview",
      icon = icon("location-arrow")),
    menuItem(
      tabName = "Zimbabwe",
      text = "Introduction to Zimbabwe",
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
      tabName = "comparison",
      text = "Comparisons--TBD",
      icon = icon("map-marked-alt"), badgeLabel = "data", badgeColor = "green"),
    menuItem(
      tabName = "conclusions",
      text = "Conclusions",
      icon = icon("chart-pie")),
    menuItem(
      tabName = "code",
      text = "Code--TBD",
      icon = icon("code-branch"), badgeLabel = "data", badgeColor = "green"),
    menuItem(
      tabName = "team",
      text = "Team",
      icon = icon("user-friends"))
  )
)



# Body ----------------------------------------------------------
body <- dashboardBody(
  fluidPage(
    tabItems(
      ## Tab Overview--------------------------------------------
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Project Overview",
                  closable = FALSE,
                  width = NULL,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h1("Industry and Workforce Attraction and Retention in Wythe County"),
                  h2("Project Description"),
                  p("Wythe County is a rural community in Southwest Virginia with a population of 28,684 (2019 estimate). 
                    It was founded in 1790 and sits at the confluence of two major highways, Interstates 77 and 81, 
                    which facilitate easy access to major markets and population centers along the Eastern Seaboard and 
                    in Midwestern and Southern states. In recent years, Wythe County has had success in attracting 
                    manufacturing facilities and travel-related businesses, and it has worked to expand the variety of 
                    tourism and hospitality options available. Like other communities in Appalachia, it has faced a 
                    declining population over the past decade. As a result, county leaders are seeking new ways to 
                    attract and retain companies, workers and residents. "),
                  br(),
                  
                  box(
                    title = "Zimbabwe",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    #enable_dropdown = TRUE,
                    #dropdown_icon = "",
                    #dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Level of Education","Industry","Home Values","Household Income","Household Size"))),
                    leafletOutput("wythe")
                  ),
                  
                  h2("Project Goals"),
                  p("We partnered with Virginia Cooperative Extension to contextualize industry and workforce factors at levels that are actionable for stakeholders and that promote informed policy and investment in Wythe County amenities and infrastructure. We identified industries and particular code within those industries that are expected to grow rapidly in the future. We visualized these code by both the skill set and education-level necessary for vocational success. We then created measures to help stakeholders assess the ability of Wythe County and the surrounding region to train the workforce of tomorrow."),
                  
                  h2("Project Approach"),
                  p("Acemoglu and Autor (2012) demonstrate that investments in human capital have large effects on both the labor market and the economy. They build on work by Goldin and Katz (2007), who also argue that investments in human capital, particularly in the area of education, have far reaching effects on the economy, public policy, and society. Our approach to studying future industry growth potential in Wythe  focused predominantly on human capital; to that end, we examined Wythe’s comparative ability to develop its population of workers to meet  workforce needs for the code of tomorrow, i.e., those code with the most promising outlook for employment and income growth."),
                  p("In their article “Developing High Growth Businesses in Rural Areas: A study of Four States,” William and Lamb (2010) note the key characteristics that a county can use to attract high growth businesses (HGBs). We will focus on three of them: (1) having a skilled and educated workforce, (2) access to research institutions, and (3), access to broadband. We expanded their idea of “access to research institutions” to include community colleges, which provide suitable education and job skills for many high-paying occupations. Job skill enhancements can also be made through workforce training sites.  Additionally, high schools can provide important training certificates in computer science, cyber security, and so on."),
                  p("The US rural economy is diverse and changing. Increasingly, the share of workers in service code is overtaking traditional industries, e.g., manufacturing and agriculture (Laughlin, 2016). As a result, it is important for counties to be nimble in their approach to workforce training.  This also demonstrates to HGBs that counties have the capacity to train a skilled workforce for new opportunities in high-growth areas, like information technology."),
                  p("Rural areas face many unique challenges when trying to attract and retain industry. Their natural amenities, affordable and less dense housing, and access to education and training services make counties like Wythe desirable places to live.  Our approach to studying Wythe was to examine two main drivers of industry attraction: people and business amenities."),
                  p("In our project we provide a visual overview of the built/physical capital available in Wythe County.  Based on this framework, our team combined publicly available demographic, infrastructure, and labor information related to the current and potential workforce in Wythe County. The data were used to identify the human capital in Wythe, assess the potential of Wythe County to train its workforce in various industries, and construct a spatial measure of accessibility to education and workforce training centers."),
                  
                  h3("References:"),
                  p("Acemoglu, D. (2012). 'What does human capital do? A review of Goldin and Katz's The race between education and technology.' Journal of Economic Literature 50(2), 426-63."),
                  p("Goldin, Claudia Dale and Katz, Lawrence F. 2009. The race between education and technology. Harvard University Press."),
                  p("Lamb, William B and Sherman, Hugh (2010). 'Developing high-growth businesses in rural areas: A study of four US States.' New England Journal of Entrepreneurship 12(2).")
                  
                )
              )),
      
      ## Tab Introduction to Zimbabwe --------------------------------------------
      tabItem(tabName = "Zimbabwe",
              #fluidRow(
              box(
                title = "Zimbabwe Project Introduction",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                img(src = "Zimbabwe_Flag.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                
                h2("Poverty in Zimbabwe"),
                p("In the first decade of the 21st century, Zimbabwe suffered from significant hyperinflation resultant of an overall government budget deficit and a simultaneous period of monetary policy that increased the amount of money in circulation. This hyperinflation, in turn, led to economic crisis as foreign investment dropped and Zimbabwean currency eventually crashed. In 2009, Zimbabwe was dollarized in an effort to mitigate inflation. Although this move was relatively successful at stabilizing the economy, the effects of economic strife still linger throughout the country. A money metric approach to defining poverty is understandably insufficient in this case due to the extreme discrepancies between Zimbabwe’s modern currency and its antiquated currency. Additionally, variations in consumption, prices, and household income distribution can make it difficult to provide an accurate account of money metric poverty as the value of money is hardly standardized."),
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
                
                  
                  #### Could try the Flipbox???
                  img(src = "zimstat_logo.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                  p("To gather data necessary for MPI construction, the DSPG team utilized the 2017 Poverty, Income, Consumption and Expenditure Survey (PICES) administered by the Zimbabwe National Statistics Agency (ZimStat). The country-wide survey is conducted every five years as a means of collecting comprehensive informatin regarding demographics, overall standards of living, poverty levels, and disparities between socio-economic groups. This information is categorized along individual, household, district, and country levels. The PICES Questionnaire is comprised of various modules that collect respondent input. The 2017 iteration included modules focused on population and household characteristics, household economy, household incomes, agriculture output and input, informal sector activities, international migration, and disability. These modules, completed by survey respondents in the civilian sector, provide insight on the general state of the country and its population and will be used by our team to understand specific aspects of poverty in Zimbabwe.")
                ),
              
              #Methodology box
              box(
                title = "Methodology",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                h2("Overview"),
                p("Work produced by the DSPG team will emulate that of Stoeffler, et al. which constructed an MPI and utilized 2001, 2007, and 2011-2012 PICES data to track Zimbabwean poverty over time in a 2015. Following their lead, our MPI will consist of eight dimensions and fourteen variables that indicate a populations status in said dimension. Each dimension and variable is weighted on the grounds of how impactful it is to the wellbeing of either the individual or the household. 
The relevant dimensions, their respective variables, and the designated weights of the MPI can be found under the ‘Tables’ tab below:
")
              ),
              
              box(
                title = "Table",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                
                img(src = "zimbabwe_var_dim.png", height="500", width="650", alt="Image", style="float: middle; margin: 10px 10px 125px 125px; border: 5px solid #000000;"),
                p("Source: Stoeffler, Quentin, et al. “Multidimensional poverty in crisis: Lessons from Zimbabwe.” The journal of development studies 52.3 (2016): 428-446.")
              ),
              
              box(
                title = "Methodology",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                h2("Measurements"),
                p("A deprivation count, k, falling between k = 1 and k = 4 is used as a threshold for determining poverty. Those who exceed or equal threshold k will be considered poor while those who do not exceed or equal threshold k will be considered not poor and thus will not be included. To achieve this result, an Alkire-Foster  ‘counting approach’ will be employed and poverty will be assigned only to those whose weighted sum deprivation count is greater than k. This creates a double cut-off in which only those who are deprived in multiple dimensions are considered and those who are deprived in a single dimension are non-factors. Once these impoverished populations are determined, three main measurements can be calculated."),
                h3("M0"),
                p("M0 is the primary method for determining a population’s poverty. It is calculated by multiplying the Headcount Ratio, H, with the Average Deprivation Share. The Headcount Ratio is the number of people who are considered poor based on a threshold, k. The Average Deprivation Share is the number of actual deprivations collected from each dimension divided by the number of potential deprivations that the state could have. In our case, this is the sum of the weights of each dimension multiplied by the population of the country."),
                h3("M1"),
                p("M1 measures what is called the Adjusted Poverty Gap. This measure is obtained by taking the average of the gaps of poor individuals below some poverty line, k. If the individual is above this threshold, k, their poverty gap is zero. Otherwise, it is always positive. This ensures that the needs of poor people are not skewed by wealthier counterparts."),
                h3("M2"),
                p("M2 is a measure of the Adjusted Poverty Severity. This measure is obtained by taking the average of the square of the gaps of poor individuals below some poverty line, k. It is very similar to M1, the only difference is that the poverty gap is squared. The quadratic nature of this measurement helps to give more weight to the people who are significantly below the poverty line as opposed to those who fall just beneath it."),
                p("The formulas needed to calculate these measures are indicated in the 'Formulas' tab below:")
              ),
              
              box(
                title = "Formulas",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                
                img(src = "zimbabwe_formulas.png", height="700", width="500", alt="Image", style="float: middle; margin: 10px 10px 125px 125px; border: 5px solid #000000;"),
                p("Source: Source: Stoeffler, Quentin, et al. “Multidimensional poverty in crisis: Lessons from Zimbabwe.” The journal of development studies 52.3 (2016): 428-446.")
              ),
              
              ####Methodology references
              box(
                title = "References",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                p("Stoeffler, Quentin, et al. “Multidimensional poverty in crisis: Lessons from Zimbabwe.” The journal of development studies 52.3 (2016): 428-446.")
              )
              
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
                    
                    
                    
                    
                  ),
                  
                  
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                  
                  
                  
                )
                
                ,
                # h2("Juvenile Facilities"),
                # p("One of the attractive features of rural America is the cost of land and housing.  Technology companies, for example, which often require land intensive sites for data centers, may find rural locations increasingly appealing. The cost of housing, the low time travel cost, and attractive recreational and natural amenities may also be important attributes to attract and retain employers and employees alike. The cost of housing in Wythe is very reasonable; a large proportion of homes are valued below $150,000. The housing stock in Wythe is, however, aging and limited (see graphs in dashboard below)."),
                # box(
                #   title = "Built Capital in Wythe County",
                #   closable = FALSE,
                #   status = "warning",
                #   solidHeader = TRUE,
                #   collapsible = TRUE,
                #   width = NULL,
                #   enable_dropdown = TRUE,
                #   dropdown_icon = "",
                #   dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Home Age", "Home Values", "Building Permit Trend"))),
                #   plotOutput("MPI")
                # ),
                br()
              )),
      ## Tab 4 comparisons--------------------------------------------
      tabItem(tabName = "comparison",
              fluidRow(
                h2("Comparisons"),
                p("Investing in human capital is yet another way of attracting and retaining HGBs. It can also provide an engine for entrepreneurship. Wythe has a total population of 29,000 with 13,000 workers. The median age of the population in Wythe county is 44, and approximately 42 percent of the population is under the age of 34. Because of its size and rural location, Wythe’s actual labor market pool is much larger than its population; it is within a 60-minute drive of most adjoining counties. The white cloud on the map surrounding Wythe represents all parts of the region that are within a 60-minute drive from the center of the county (the red dot)."),
                br(),
                box(
                  title = "Wythe:60-Minutes Isochrone",
                  closable = FALSE,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  #width = NULL,
                  #enable_dropdown = TRUE,
                  #dropdown_icon = "",
                  #dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Level of Education","Industry","Home Values","Household Income","Household Size"))),
                  leafletOutput("isochrones")
                ),
                br(),
                h2("Characteristics of the Wythe Labor Market"),
                br(),
                box(
                  title = "Human Capital in Wythe County",
                  closable = FALSE,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  enable_dropdown = TRUE,
                  dropdown_icon = "",
                  dropdown_menu = tagList(selectInput("vari","Select a Variable",choices = c("Industry","Level of Education","Household Income","Household Size"))),
                  plotOutput("myplot")
                ),
                br(),
                p("The dashboard above shows important features of Wythe’s labor market with regard to industry concentration, salary, and household size."),
                p("First, the workforce in Wythe County is concentrated in the health, education, manufacturing, and retail industries. Outside of education and healthcare, many workers in Wythe are in the retail sector.  While these types of service code are important, it should be recognized that these are not code that are typically associated with high earning potential. Moreover, e-commerce is continuing to crowd out many of these types of businesses. From an economic growth perspective, it is therefore important to continue diversifying the industrial base. "),
                
                p("Second, the majority of households in Wythe have incomes between $25,000 to $150,000, with the median household income between $50,000 and $75,000."),
                p("Third, households in Wythe County are relatively small. The majority of households have one or two people. Given the median age and proportion of the population under the age of 34, the population of Wythe does not appear to be overly skewed toward older workers and retired individuals (as the prevalence of one-person and two-person households might suggest). "),
                h3("References:"),
                p("[1] https://datausa.io/profile/geo/wythe-county-va#:~:text=In%202017%2C%20the%20median%20age,County%2C%20VA%20residents%20was%2044."),
                br()
              )),
      ## Tab 5 code--------------------------------------------
      tabItem(tabName = "code",
              fluidRow(
                p("One of the key tasks for the VT-DSPG team was to identify some of the best code available over the next several years. To accomplish this, we mined data from the ONet project. ONet databases list categories of occupations by sector and industry and rate them according to future job growth.  The ratings are a standardized index (0-100) created from reported importance and scale factors for each occupation. We call ONet high-growth occupations the “code of tomorrow.” To represent these high-growth occupation categories, we use a collapsible tree diagram, which allows us to compactly present large quantities of data. In this way, we can map careers from broad industries to particular occupation to the skill set and education level typically needed for each occupation. For comparison purposes, the ratings are listed next to each reported education level, job training time period, and experience time period."),
                h3("Highlighted Industry"),
                p("As an example, consider code within the information technology (IT) industry. The user can choose between careers in Information Systems, Network Systems, Programming/Software Development, and Web and Digital Communications. For each career path, the viewer can select specific occupations and find information about the skill set necessary for that occupation. A similar collapsible tree for education, which is also available in the dropdown dashboard, demonstrates the typical education credential necessary for someone in a particular occupation."),
                p("The occupations listed in the tree were selected because they are careers with bright futures; as such, they represent the code of tomorrow. "),
                p("The trees provided actionable information for implementing programs. They also suggest curricula that might be developed to train the future workforce in a desired industry. "),
                box(
                  title = "Information Technology",
                  closable = FALSE,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  #width = "100%",
                  enable_sidebar = FALSE,
                  enable_dropdown = TRUE,
                  dropdown_icon = "",
                  dropdown_menu = tagList(selectInput("varIT","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                  collapsibleTreeOutput("myITtree",width = "100%")
                ),
                # br(),
                # #p("Similar trees are included below for the Agricultural and Manufacturing industries."),
                # boxPlus(
                #   title = "Agriculture",
                #   closable = FALSE,
                #   status = "warning",
                #   solidHeader = TRUE,
                #   collapsible = TRUE,
                #   width = "100%",
                #   enable_sidebar = FALSE,
                #   enable_dropdown = TRUE,
                #   dropdown_icon = "",
                #   dropdown_menu = tagList(selectInput("varAg","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                #   collapsibleTreeOutput("myAgtree",width = "100%")
                # ),
                # br(),
                # boxPlus(
                #   title = "Manufacturing",
                #   closable = FALSE,
                #   status = "warning",
                #   solidHeader = TRUE,
                #   collapsible = TRUE,
                #   width = "100%",
                #   enable_sidebar = FALSE,
                #   enable_dropdown = TRUE,
                #   dropdown_icon = "",
                #   dropdown_menu = tagList(selectInput("varMan","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                #   collapsibleTreeOutput("myMantree",width = "100%")
                # ),
                br(),
                h3("code of Tomorrow"),
                
                br(),
                box(
                  title = "code of Tomorrow.",
                  closable = FALSE,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  #width = "100%",
                  enable_sidebar = FALSE,
                  enable_dropdown = TRUE,
                  dropdown_icon = "",
                  dropdown_menu = tagList(selectInput("var1","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                  collapsibleTreeOutput("mytree",width = "100%")
                ),
                p("Given the sizeable role investments in human capital have on economic growth, detailed knowledge of the industries, careers paths and occupations that are growing in the U.S. is valuable when determining how to allocate resources. This knowledge provides actionable information for preparing, teaching, and training the workforce of tomorrow. "),
                p("The collapsible trees (above) map ONet career and occupation data. Each tree maps industry (the first level) to career pathways (second level) to occupations (third level). This data represents industries, careers and occupations with the highest projected growth or brightest outlook. In addition, the dropdown menu allows for a fourth level. As a whole, the trees map Skills, Education, Experience, On-Site-Training and On-Job-Training needed for each occupation, career and industry."),
                p("Desired industries, as indicated by Wythe County officials, include manufacturing, agriculture, information systems and technology and food and beverage production. These industries are available in the visualizations above."),
                br()
              )),
      ## Tab 6 TBD--------------------------------------------
      tabItem(tabName = "access",
              fluidRow(
                h2("A Regional Comparison of Educational Access"),
                p("An analysis of the trees (see previous tab) shows that many of the high growth careers are in information technology. These code often require training in a host of critical areas, from customer support and client satisfaction to computer science and systems and electrical engineering. Aside from the heavy STEM focus in information technology, there is also a large need for people with skills in communication, education, and training. Additionally, there are many high-paying code in the IT sector that do not require a college degree. IT code such as web developer, computer programmer, systems analyst, cybersecurity analyst, graphic designer, digital marketer, and telecommunications technician can be filled by individuals that have relevant skills but not necessarily a four-year degree. In such cases, local community colleges and high schools can help with IT skill development by providing courses and certification programs that are readily accessible to residents in Wythe County."),
                
                tabBox(
                  title = NULL , width = 16,
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("Educational Institutions", sidebarLayout(
                    sidebarPanel(
                      selectInput("spatial_variable", "Spatial Variable:",
                                  c("Colleges and Universities",
                                    "Community Colleges",
                                    "Workforce Development Centers",
                                    "Colleges - All types")),
                      selectInput("time_variable", "Time Variable:",
                                  c("60 minutes" = "60",
                                    "45 minutes" = "45",
                                    "30 minutes" = "30"))
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                      tableOutput("label_1"),
                      leafletOutput("mapplot_1"),
                      #mapview:::plainViewOutput("test")
                    )
                  )
                  
                  ),
                  tabPanel("Broadband and High School",
                           sidebarPanel(
                             selectInput("variable", "Comparison Variable:",
                                         c("Number of High Schools",
                                           "Percentage of Broadband Access",
                                           "Percentage of People having Computer"))
                             
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             tableOutput("label_2"),
                             leafletOutput("mapplot_2")
                             #mapview:::plainViewOutput("test")
                           )
                           
                  )
                ),  
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                h3("Colleges and Universities"),
                p("The foregoing analysis motivated the team to examine Wythe County’s access to institutions that provide high-quality, high-impact training. The dashboard above shows access measures for education and workforce training for Wythe County and the surrounding region. The workforce training, community college, and four-year college and university measures are counts of these locations within a chosen travel time from the population weighted county centroid."),
                p("For example, Wythe County has three colleges and universities located within a 60-minute drive of most of the population. If, however, one narrows the time window to 30 minutes, there is only one. Further exploration of the dashboard shows that this institution is a community college, which may play an important role in providing workforce training. In summary, Wythe is well positioned in terms of access to continuing and higher education facilities."),
                
                br(),
                h3("Broadband and High Schools"),
                p("High schools also play a large role in training future workers. Trade and vocational programs can offer future career paths for many students who do not pursue additional education beyond a high school diploma. This second dashboard shows that Wythe County has three high schools."),
                p("In this dashboard, we also display the percentage of the population who have access to broadband internet and who have a computer in the home. Online learning is an increasingly important and popular way to acquire additional skills and education. These visualizations allow us to assess Wythe’s ability to train the current and future workforce from home and to serve as a site for telework.  This is particularly important given that so many aspects of education and work have moved online due to Covid-19."),
                br()
              )),
      ## Tab 7 Conclusion--------------------------------------------
      tabItem(tabName = "conclusions",
              fluidRow(
                box(
                  title = "Conclusions",
                  closable = FALSE,
                  width = NULL,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  p("Wythe, like many of its counterparts in Southwest Virginia, 
                    has quality secondary schools and access to outdoor recreational amenities. 
                    These factors are often cited as important to millennials as they think about 
                    potential occupations and locations. To capture and retain workers, we recommend that 
                    Wythe emphasize tech-related code in addition to its current industrial strengths.  
                    These code include cybersecurity, data centers, and so on. There are many high-paying code 
                    in these fields that DO NOT require a college degree. Additionally, this could allow 
                    Wythe to take better advantage of Virginia's Tech Talent Pipeline Initiative. "),
                  p("Wythe has many unique features that make it an attractive place to live: 
                    low population density, reasonably low rates of violent crime, and great natural amenities. 
                    If county leaders can demonstrate through programs at their high schools and community colleges 
                    that the local workforce is prepared for IT-related code and industries, then the county
                    will be in a strong position to promote itself as a vital component of Southwest Virginia's 
                    Technology Corridor. Community colleges and local high schools (resources that are already 
                    available in Wythe) can help to develop workforce talent.")
                )
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
              ))
    )
  )
)



# UI--------------------------
ui <- dashboardPage(
    dashboardHeader(title = "Zimbabwe(Draft)"),
    sidebar = sidebar,
    body = body
)




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
}

# Shiny App------------------------
shinyApp(ui = ui, server = server)
