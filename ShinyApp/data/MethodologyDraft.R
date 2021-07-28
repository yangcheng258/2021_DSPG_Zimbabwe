library(shiny)

ui <- fluidPage(
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
)

server <- function(input, output, session) {
  
}

shinyApp(ui = ui, server = server)