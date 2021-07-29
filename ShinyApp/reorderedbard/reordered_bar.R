#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinyjs)
library(forcats)

#Pulls required data 
ranked_data <- read_csv("/Users/mattb24/Zimbabwe/2021_DSPG_Zimbabwe/ShinyApp/MappingData/OriginalMPI/2017/2017_District.csv")
prov_ranked <- read_csv("/Users/mattb24/Zimbabwe/2021_DSPG_Zimbabwe/ShinyApp/MappingData/OriginalMPI/2017/2017_Province.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    # Application title
    titlePanel("More graphs"),
    
    # Sidebar with a slider input for number of bins 

        sidebarPanel(
            # menuItem("M0",
            #          tabName = "M0_k"),
            # menuItem("M1", 
            #          tabName = "M1_k"),
            # menuItem("M2", 
            #          tabName = "M2_k")
            
            # menuItem("M0",
            #          tabName = "M0_dec"),
            # menuItem("M1",
            #          tabName = "M1_dec"),
            # menuItem("M2",
            #          tabName = "M2_dec")
            
            ##Province Ranking
            menuItem("M0",
                     tabName = "M0_prov"),
            menuItem("M1",
                     tabName = "M1_prov"),
            menuItem("M2",
                     tabName = "M2_prov")
            
        ),
        
        # Show a plot of the generated distribution
        
            dashboardBody(
                ##K Threshold
                # tabItem(tabName = "M0_k",
                #         plotlyOutput("M0_ranking", height = 750),
                #         sliderInput("M0_k_threshold", "K Threshold", min = 1, max = 9, value = 3)),
                # tabItem(tabName = "M1_k",
                #         plotlyOutput("M1_ranking", height = 750),
                #         sliderInput("M1_k_threshold", "K Threshold", min = 1, max = 9, value = 3)),
                # tabItem(tabName = "M2_k",
                #         plotlyOutput("M2_ranking", height = 750),
                #         sliderInput("M2_k_threshold", "K Threshold", min = 1, max = 9, value = 3))
                # 
                
                ##Decomp
                # tabItem(tabName = "M0_dec",
                #         plotlyOutput("M0_decomp", height = 750),
                #         sliderInput("M0_decomp_k", "K Threshold", min = 1, max = 9, value = 3)),
                # tabItem(tabName = "M1_dec",
                #         plotlyOutput("M1_decomp", height = 750),
                #         sliderInput("M1_decomp_k", "K Threshold", min = 1, max = 9, value = 3)),
                # tabItem(tabName = "M2_dec",
                #         plotlyOutput("M2_decomp", height = 750),
                #         sliderInput("M2_decomp_k", "K Threshold", min = 1, max = 9, value = 3))

                ##Province Level
                tabItem(tabName = "M0_prov",
                        plotlyOutput("M0_prov", height = 750),
                        sliderInput("M0_prov_k", "K Threshold", min = 1, max = 9, value = 3)),
                tabItem(tabName = "M1_prov",
                        plotlyOutput("M1_prov", height = 750),
                        sliderInput("M1_prov_k", "K Threshold", min = 1, max = 9, value = 3)),
                tabItem(tabName = "M2_prov",
                        plotlyOutput("M2_prov", height = 750),
                        sliderInput("M2_prov_k", "K Threshold", min = 1, max = 9, value = 3))
        ))



server <- function(input, output) {
    

# M0,  M1,  M2 by K -------------------------------------------------------


    M0_k_threshold <- reactive({
        input$M0_k_threshold
    })

    output$M0_ranking <- renderPlotly({
        if (M0_k_threshold() == "1") {
        M0_k1_ranking <- ranked_data %>% 
            mutate(District_name = fct_reorder(District_name, M0_k1)) %>% 
            ggplot(aes(x = District_name, y = M0_k1)) +
            geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
            coord_flip() +
            labs(y = "M0 at Threshold K = 1", x = "Districts", title = "District Comparison") +
            theme_minimal()
        
        ggplotly(M0_k1_ranking)
        
        }
    
        else if (M0_k_threshold() == "2") {
            M0_k2_ranking <- ranked_data %>% 
                mutate(District_name = fct_reorder(District_name, M0_k2)) %>% 
                ggplot(aes(x = District_name, y = M0_k2)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 2", x = "Districts", title = "District Comparison") +
                theme_minimal()
        
        ggplotly(M0_k2_ranking)
        
        }
        
        else if (M0_k_threshold() == "3") {
            M0_k3_ranking <- ranked_data %>% 
                mutate(District_name = fct_reorder(District_name, M0_k3)) %>% 
                ggplot(aes(x = District_name, y = M0_k3)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 3", x = "Districts", title = "District Comparison") +
                theme_minimal()
            
            ggplotly(M0_k3_ranking)
            
        }
        
        else if (M0_k_threshold() == "4") {
            M0_k4_ranking <- ranked_data %>% 
                mutate(District_name = fct_reorder(District_name, M0_k4)) %>% 
                ggplot(aes(x = District_name, y = M0_k4)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 4", x = "Districts", title = "District Comparison") +
                theme_minimal()
            
            ggplotly(M0_k4_ranking)
            
        }
        
        else if (M0_k_threshold() == "5") {
            M0_k5_ranking <- ranked_data %>% 
                mutate(District_name = fct_reorder(District_name, M0_k5)) %>% 
                ggplot(aes(x = District_name, y = M0_k5)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 5", x = "Districts", title = "District Comparison") +
                theme_minimal()
            
            ggplotly(M0_k5_ranking)
            
        }
        
        else if (M0_k_threshold() == "6") {
            M0_k6_ranking <- ranked_data %>% 
                mutate(District_name = fct_reorder(District_name, M0_k6)) %>% 
                ggplot(aes(x = District_name, y = M0_k6)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 6", x = "Districts", title = "District Comparison") +
                theme_minimal()
            
            ggplotly(M0_k6_ranking)
            
        }
        
        else if (M0_k_threshold() == "7") {
            M0_k7_ranking <- ranked_data %>% 
                mutate(District_name = fct_reorder(District_name, M0_k7)) %>% 
                ggplot(aes(x = District_name, y = M0_k7)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 7", x = "Districts", title = "District Comparison") +
                theme_minimal()
            
            ggplotly(M0_k7_ranking)
            
        }
        
        else if (M0_k_threshold() == "8") {
            M0_k8_ranking <- ranked_data %>% 
                mutate(District_name = fct_reorder(District_name, M0_k8)) %>% 
                ggplot(aes(x = District_name, y = M0_k8)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 8", x = "Districts", title = "District Comparison") +
                theme_minimal()
            
            ggplotly(M0_k8_ranking)
            
        }
        
        else if (M0_k_threshold() == "9") {
            
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
    

# M1,  M2,  M0 by Variable ------------------------------------------------


    

# Province Level ----------------------------------------------------------
    M0_prov_k_threshold <- reactive({
        input$M0_prov_k
    })
    
    output$M0_prov <- renderPlotly({
        if (M0_prov_k_threshold() == "1") {
            M0_k1_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M0_k1)) %>% 
                ggplot(aes(x = Province_name, y = M0_k1)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 1", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M0_k1_prov_ranking)
            
        }
        
        else if (M0_prov_k_threshold() == "2") {
            M0_k2_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M0_k2)) %>% 
                ggplot(aes(x = Province_name, y = M0_k2)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 2", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M0_k2_prov_ranking)
           
        }
        
        else if (M0_prov_k_threshold() == "3") {
            M0_k3_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M0_k3)) %>% 
                ggplot(aes(x = Province_name, y = M0_k3)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 3", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M0_k3_prov_ranking)
            
        }
        
        else if (M0_prov_k_threshold() == "4") {
            M0_k4_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M0_k4)) %>% 
                ggplot(aes(x = Province_name, y = M0_k4)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 4", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M0_k4_prov_ranking)
            
        }
        
        else if (M0_prov_k_threshold() == "5") {
            M0_k5_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M0_k5)) %>% 
                ggplot(aes(x = Province_name, y = M0_k5)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 5", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M0_k5_prov_ranking)
            
        }
        
        else if (M0_prov_k_threshold() == "6") {
            M0_k6_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M0_k6)) %>% 
                ggplot(aes(x = Province_name, y = M0_k6)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 6", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M0_k6_prov_ranking)
            
        }
        
        else if (M0_prov_k_threshold() == "7") {
            M0_k7_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M0_k7)) %>% 
                ggplot(aes(x = Province_name, y = M0_k7)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 7", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M0_k7_prov_ranking)
            
        }
        
        else if (M0_prov_k_threshold() == "8") {
            M0_k8_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M0_k8)) %>% 
                ggplot(aes(x = Province_name, y = M0_k8)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M0 at Threshold K = 8", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M0_k8_prov_ranking)
            
        }
        
        else if (M0_prov_k_threshold() == "9") {
            M0_k9_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M0_k9)) %>% 
                ggplot(aes(x = Province_name, y = M0_k9)) +
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
    
    output$M1_prov <- renderPlotly({
        if (M1_prov_k_threshold() == "1") {
            M1_k1_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M1_k1)) %>% 
                ggplot(aes(x = Province_name, y = M1_k1)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M1 at Threshold K = 1", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M1_k1_prov_ranking)
            
        }
        
        else if (M1_prov_k_threshold() == "2") {
            M1_k2_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M1_k2)) %>% 
                ggplot(aes(x = Province_name, y = M1_k2)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M1 at Threshold K = 2", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M1_k2_prov_ranking)
            
        }
        
        else if (M1_prov_k_threshold() == "3") {
            M1_k3_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M1_k3)) %>% 
                ggplot(aes(x = Province_name, y = M1_k3)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M1 at Threshold K = 3", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M1_k3_prov_ranking)
            
        }
        
        else if (M1_prov_k_threshold() == "4") {
            M1_k4_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M1_k4)) %>% 
                ggplot(aes(x = Province_name, y = M1_k4)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M1 at Threshold K = 4", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M1_k4_prov_ranking)
            
        }
        
        else if (M1_prov_k_threshold() == "5") {
            M1_k5_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M1_k5)) %>% 
                ggplot(aes(x = Province_name, y = M1_k5)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M1 at Threshold K = 5", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M1_k5_prov_ranking)
            
        }
        
        else if (M1_prov_k_threshold() == "6") {
            M1_k6_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M1_k6)) %>% 
                ggplot(aes(x = Province_name, y = M1_k6)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M1 at Threshold K = 6", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M1_k6_prov_ranking)
            
        }
        
        else if (M1_prov_k_threshold() == "7") {
            M1_k7_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M1_k7)) %>% 
                ggplot(aes(x = Province_name, y = M1_k7)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M1 at Threshold K = 7", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M1_k7_prov_ranking)
            
        }
        
        else if (M1_prov_k_threshold() == "8") {
            M1_k8_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M1_k8)) %>% 
                ggplot(aes(x = Province_name, y = M1_k8)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M1 at Threshold K = 8", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M1_k8_prov_ranking)
            
        }
        
        else if (M1_prov_k_threshold() == "9") {
            M1_k9_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M1_k9)) %>% 
                ggplot(aes(x = Province_name, y = M1_k9)) +
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
    
    output$M2_prov <- renderPlotly({
        if (M2_prov_k_threshold() == "1") {
            M2_k1_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M2_k1)) %>% 
                ggplot(aes(x = Province_name, y = M2_k1)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M2 at Threshold K = 1", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M2_k1_prov_ranking)
            
        }
        
        else if (M2_prov_k_threshold() == "2") {
            M2_k2_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M2_k2)) %>% 
                ggplot(aes(x = Province_name, y = M2_k2)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M2 at Threshold K = 2", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M2_k2_prov_ranking)
            
        }
        
        else if (M2_prov_k_threshold() == "3") {
            M2_k3_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M2_k3)) %>% 
                ggplot(aes(x = Province_name, y = M2_k3)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M2 at Threshold K = 3", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M2_k3_prov_ranking)
            
        }
        
        else if (M2_prov_k_threshold() == "4") {
            M2_k4_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M2_k4)) %>% 
                ggplot(aes(x = Province_name, y = M2_k4)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M2 at Threshold K = 4", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M2_k4_prov_ranking)
            
        }
        
        else if (M2_prov_k_threshold() == "5") {
            M2_k5_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M2_k5)) %>% 
                ggplot(aes(x = Province_name, y = M2_k5)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M2 at Threshold K = 5", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M2_k5_prov_ranking)
            
        }
        
        else if (M2_prov_k_threshold() == "6") {
            M2_k6_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M2_k6)) %>% 
                ggplot(aes(x = Province_name, y = M2_k6)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M2 at Threshold K = 6", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M2_k6_prov_ranking)
            
        }
        
        else if (M2_prov_k_threshold() == "7") {
            M2_k7_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M2_k7)) %>% 
                ggplot(aes(x = Province_name, y = M2_k7)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M2 at Threshold K = 7", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M2_k7_prov_ranking)
            
        }
        
        else if (M2_prov_k_threshold() == "8") {
            M2_k8_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M2_k8)) %>% 
                ggplot(aes(x = Province_name, y = M2_k8)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M2 at Threshold K = 8", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M2_k8_prov_ranking)
            
        }
        
        else if (M2_prov_k_threshold() == "9") {
            M2_k9_prov_ranking <- prov_ranked %>% 
                mutate(Province_name = fct_reorder(Province_name, M2_k9)) %>% 
                ggplot(aes(x = Province_name, y = M2_k9)) +
                geom_bar(stat = "identity", fill = "#f68061", alpha = .6, width = .4, ) +
                coord_flip() +
                labs(y = "M2 at Threshold K = 9", x = "Province", title = "Province Comparison") +
                theme_minimal()
            
            ggplotly(M2_k9_prov_ranking)
            
        }

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
