library(shinythemes)
# Use a fluid Bootstrap layout
fluidPage(theme = shinytheme("flatly"),
          
          # Give the page a title
          titlePanel(h1("Zverev Grand Slam Serve Speed Distributions (2018 - 2019)",
                        align = 'center'),
                     windowTitle = "Zverev Grand Slam Serve Speed Distributions (2018 - 2019)"),
          hr(),
          # Generate a row with a sidebar
          sidebarLayout(      
            
            # Define the sidebar with one input
            sidebarPanel(
              selectInput("grand_slam", "Grand Slam:", 
                          choices=c('ausopen', 'usopen', 'frenchopen', 'wimbledon')),
              hr(),
              textOutput('avg_2nd_speed'),
              helpText("StatsOntheT: When two first serves are better than one.")
            ),
            
            # Create a spot for the barplot
            mainPanel(
              plotOutput("speedPlot")  
            )
            
          )
)

