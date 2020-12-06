
library(shinythemes)
library(dplyr)
library(ggiraph)

effects_dataframe <-read.csv('./effects_dataframe.csv')

effects_dataframe <- effects_dataframe %>%
  filter(num_matches > 70)

player_choices <- effects_dataframe$player[order(desc(effects_dataframe$num_matches))]


fluidPage(
  # Give the page a title
  titlePanel(h1("Prototype of Hierarchical Ace Effects (1991 - 2019)",
                align = 'center'),
             windowTitle = "Hello"),
  hr(),
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("player_choice", "Player:", 
                  choices=player_choices,
                  selected = 'Nick Kyrgios',
                  multiple = TRUE),
      hr(),
      helpText("On-the-T")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      
      style = "position:relative",
      girafeOutput("scatter_plot"),
      
      #plotOutput("scatter_plot")  
    )
    
  )
  
  
)
          