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
      icon = icon("info circle")),
    menuItem(
      tabName = "Zimbabwe",
      text = "Introduction to Zimbabwe",
      icon = icon("database")),
    menuItem(
      tabName = "data",
      text = "Data & Methodology",
      icon = icon("database"), badgeLabel = "data", badgeColor = "green"),
    menuItem(
      tabName = "builtcapital",
      text = "MPI",
      icon = icon("map-marked-alt"), badgeLabel = "data", badgeColor = "green"),
    menuItem(
      tabName = "humancapital",
      text = "Comparisons--TBD",
      icon = icon("map-marked-alt"), badgeLabel = "data", badgeColor = "green"),
    menuItem(
      tabName = "jobs",
      text = "Code--TBD",
      icon = icon("map-marked-alt"), badgeLabel = "data", badgeColor = "green"),
    # menuItem(
    #   tabName = "access",
    #   text = "Access",
    #   icon = icon("map-marked-alt")),
    menuItem(
      tabName = "conclusions",
      text = "Conclusions",
      icon = icon("chart-pie")),
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
                  p("We partnered with Virginia Cooperative Extension to contextualize industry and workforce factors at levels that are actionable for stakeholders and that promote informed policy and investment in Wythe County amenities and infrastructure. We identified industries and particular jobs within those industries that are expected to grow rapidly in the future. We visualized these jobs by both the skill set and education-level necessary for vocational success. We then created measures to help stakeholders assess the ability of Wythe County and the surrounding region to train the workforce of tomorrow."),
                  
                  h2("Project Approach"),
                  p("Acemoglu and Autor (2012) demonstrate that investments in human capital have large effects on both the labor market and the economy. They build on work by Goldin and Katz (2007), who also argue that investments in human capital, particularly in the area of education, have far reaching effects on the economy, public policy, and society. Our approach to studying future industry growth potential in Wythe  focused predominantly on human capital; to that end, we examined Wythe’s comparative ability to develop its population of workers to meet  workforce needs for the jobs of tomorrow, i.e., those jobs with the most promising outlook for employment and income growth."),
                  p("In their article “Developing High Growth Businesses in Rural Areas: A study of Four States,” William and Lamb (2010) note the key characteristics that a county can use to attract high growth businesses (HGBs). We will focus on three of them: (1) having a skilled and educated workforce, (2) access to research institutions, and (3), access to broadband. We expanded their idea of “access to research institutions” to include community colleges, which provide suitable education and job skills for many high-paying occupations. Job skill enhancements can also be made through workforce training sites.  Additionally, high schools can provide important training certificates in computer science, cyber security, and so on."),
                  p("The US rural economy is diverse and changing. Increasingly, the share of workers in service jobs is overtaking traditional industries, e.g., manufacturing and agriculture (Laughlin, 2016). As a result, it is important for counties to be nimble in their approach to workforce training.  This also demonstrates to HGBs that counties have the capacity to train a skilled workforce for new opportunities in high-growth areas, like information technology."),
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
                title = "Zimbabwe",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                
                
                img(src="ACS.jpg", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                br(),
                p("We used data from the American Community Survey to visualize labor, 
                  housing and infrastructure data to identify built and human capital currently in Wythe County."),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br()
                
              ),
              
              
              box(
                title = "Foster Care",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                p("One of the central aims of this project is to create comparable measures of accessibility to 
                  educational facilities in the county.  In rural areas, metrics such as distance do not have the same 
                  meaning as they do in urban areas. Large distances in rural areas can often be traversed quickly 
                  due to lower traffic density and a lack of other traffic impediments. To create our accessibility 
                  measures, we rely on travel time, which accounts for both distance and traffic flow. We construct 
                  accessibility measures for each county in the region by estimating the travel time between the county
                  population weighted centroid and foci for educational and workforce training (Waldorf and Chen, 2010). 
                  We use a floating catchment area around each county centroid and vary the size of the travel time 
                  window to include 30, 45, and 60 min windows for each catchment area. We then count the services 
                  by type within each of the travel time windows. This allows us, for each county, to construct:")
                ),
              
              
              
              box(
                title = "Juvenile",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                p("Luo, Wei and Wang, Fahui (2003). 'Measures of spatial accessibility to health care in a GIS environment: synthesis and a case study in the Chicago region.' Environment and Planning B: Planning and Design 30(6):865—884."),
                p("Waldorf, B. S., & Chen, S. E. (2010). 'Spatial models of health outcomes and health behaviors: the role of health care accessibility and availability.' In Progress in spatial analysis (339-362). Springer: Berlin, Heidelberg.")
                
              ),
              
              
              
              box(
                title = "References",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                p("Luo, Wei and Wang, Fahui (2003). 'Measures of spatial accessibility to health care in a GIS environment: synthesis and a case study in the Chicago region.' Environment and Planning B: Planning and Design 30(6):865—884."),
                p("Waldorf, B. S., & Chen, S. E. (2010). 'Spatial models of health outcomes and health behaviors: the role of health care accessibility and availability.' In Progress in spatial analysis (339-362). Springer: Berlin, Heidelberg.")
                
              )
              
      ),
      
      ## Tab Data --------------------------------------------
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
                  img(src="ACS.jpg", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                  br(),
                  p("We used data from the American Community Survey to visualize labor, housing and infrastructure data to identify built and human capital currently in Wythe County."),
                  br(),
                  br(),
                  br(),
                  br(),
                  img(src="careerWorks.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                  br(),
                  p("We used Virginia Careerworks Data to identify industries, colleges and universities, and workforce training facilities in Wythe county."),
                  br(),
                  br(),
                  br(),
                  img(src="VEC.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                  br(),
                  p("VirginiaWorks is a collaboration of agencies run by the Virginia Employment Commission."),
                  br(),
                  br(),
                  br(),
                  img(src="onet.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                  p("We used O*Net to gathered industry/occupation data for occupations that have a “bright outlook” which is defined as having an expectation of growth over the next several years. We also gather the skill set, education level, work experience, and training needed for these occupations."),
                  br(),
                  br(),
                  br(),
                  img(src="OSM.jpg", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                  br(),
                  p("We used OpenStreetMap and the VirginiaWorks data to construct time-to-travel measures for access to education and workforce training. With this data, we were able to map floating catchment areas for various time windows in around Wythe County. "),
                  br()
                  
                ),
              
              
              box(
                title = "Methodology",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                p("One of the central aims of this project is to create comparable measures of accessibility to educational facilities in the county.  In rural areas, metrics such as distance do not have the same meaning as they do in urban areas. Large distances in rural areas can often be traversed quickly due to lower traffic density and a lack of other traffic impediments. To create our accessibility measures, we rely on travel time, which accounts for both distance and traffic flow. We construct accessibility measures for each county in the region by estimating the travel time between the county population weighted centroid and foci for educational and workforce training (Waldorf and Chen, 2010). We use a floating catchment area around each county centroid and vary the size of the travel time window to include 30, 45, and 60 min windows for each catchment area. We then count the services by type within each of the travel time windows. This allows us, for each county, to construct:"),
                p("1. No. of workforce training sites within 30 (45, 60) minute driving distance of the county"),
                p("2. No. of community colleges within 30 (45, 60) minute driving distance of the county"),
                p("3. No. of four-year colleges and universities within 30 (45, 60) minute driving distance of the county")
                
              ),
              
              ####Might try the DT output  in this part???
              box(
                title = "References",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                p("Luo, Wei and Wang, Fahui (2003). 'Measures of spatial accessibility to health care in a GIS environment: synthesis and a case study in the Chicago region.' Environment and Planning B: Planning and Design 30(6):865—884."),
                p("Waldorf, B. S., & Chen, S. E. (2010). 'Spatial models of health outcomes and health behaviors: the role of health care accessibility and availability.' In Progress in spatial analysis (339-362). Springer: Berlin, Heidelberg.")
                
              )
              
              ),
      ## Tab 3--------------------------------------------
      tabItem(tabName = "builtcapital",
              fluidRow(
                h2("Service/Program Availability"),
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
                #   plotOutput("builtCapital")
                # ),
                br()
              )),
      ## Tab 4--------------------------------------------
      tabItem(tabName = "humancapital",
              fluidRow(
                h2("Human Capital in Wythe County"),
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
                p("First, the workforce in Wythe County is concentrated in the health, education, manufacturing, and retail industries. Outside of education and healthcare, many workers in Wythe are in the retail sector.  While these types of service jobs are important, it should be recognized that these are not jobs that are typically associated with high earning potential. Moreover, e-commerce is continuing to crowd out many of these types of businesses. From an economic growth perspective, it is therefore important to continue diversifying the industrial base. "),
                
                p("Second, the majority of households in Wythe have incomes between $25,000 to $150,000, with the median household income between $50,000 and $75,000."),
                p("Third, households in Wythe County are relatively small. The majority of households have one or two people. Given the median age and proportion of the population under the age of 34, the population of Wythe does not appear to be overly skewed toward older workers and retired individuals (as the prevalence of one-person and two-person households might suggest). "),
                h3("References:"),
                p("[1] https://datausa.io/profile/geo/wythe-county-va#:~:text=In%202017%2C%20the%20median%20age,County%2C%20VA%20residents%20was%2044."),
                br()
              )),
      ## Tab 5--------------------------------------------
      tabItem(tabName = "jobs",
              fluidRow(
                p("One of the key tasks for the VT-DSPG team was to identify some of the best jobs available over the next several years. To accomplish this, we mined data from the ONet project. ONet databases list categories of occupations by sector and industry and rate them according to future job growth.  The ratings are a standardized index (0-100) created from reported importance and scale factors for each occupation. We call ONet high-growth occupations the “jobs of tomorrow.” To represent these high-growth occupation categories, we use a collapsible tree diagram, which allows us to compactly present large quantities of data. In this way, we can map careers from broad industries to particular occupation to the skill set and education level typically needed for each occupation. For comparison purposes, the ratings are listed next to each reported education level, job training time period, and experience time period."),
                h3("Highlighted Industry"),
                p("As an example, consider jobs within the information technology (IT) industry. The user can choose between careers in Information Systems, Network Systems, Programming/Software Development, and Web and Digital Communications. For each career path, the viewer can select specific occupations and find information about the skill set necessary for that occupation. A similar collapsible tree for education, which is also available in the dropdown dashboard, demonstrates the typical education credential necessary for someone in a particular occupation."),
                p("The occupations listed in the tree were selected because they are careers with bright futures; as such, they represent the jobs of tomorrow. "),
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
                h3("Jobs of Tomorrow"),
                
                br(),
                box(
                  title = "Jobs of Tomorrow.",
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
      ## Tab 6--------------------------------------------
      tabItem(tabName = "access",
              fluidRow(
                h2("A Regional Comparison of Educational Access"),
                p("An analysis of the trees (see previous tab) shows that many of the high growth careers are in information technology. These jobs often require training in a host of critical areas, from customer support and client satisfaction to computer science and systems and electrical engineering. Aside from the heavy STEM focus in information technology, there is also a large need for people with skills in communication, education, and training. Additionally, there are many high-paying jobs in the IT sector that do not require a college degree. IT jobs such as web developer, computer programmer, systems analyst, cybersecurity analyst, graphic designer, digital marketer, and telecommunications technician can be filled by individuals that have relevant skills but not necessarily a four-year degree. In such cases, local community colleges and high schools can help with IT skill development by providing courses and certification programs that are readily accessible to residents in Wythe County."),
                
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
      ## Tab 7--------------------------------------------
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
                    Wythe emphasize tech-related jobs in addition to its current industrial strengths.  
                    These jobs include cybersecurity, data centers, and so on. There are many high-paying jobs 
                    in these fields that DO NOT require a college degree. Additionally, this could allow 
                    Wythe to take better advantage of Virginia's Tech Talent Pipeline Initiative. "),
                  p("Wythe has many unique features that make it an attractive place to live: 
                    low population density, reasonably low rates of violent crime, and great natural amenities. 
                    If county leaders can demonstrate through programs at their high schools and community colleges 
                    that the local workforce is prepared for IT-related jobs and industries, then the county
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
                  p("Our project goal was to identify industries and the jobs that are expected to grow rapidly in the future. We visualized these jobs by the skills, education, experience and training needed to do them. We then created measures to visualize and assess the ability of Wythe County and the surrounding region to respond to training the workers of tomorrow. Our team is comprised of talented individuals with a broad range of skills and experience."),
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
