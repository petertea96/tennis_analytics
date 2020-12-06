library(dplyr)
library(ggplot2)


effects_dataframe <-read.csv('effects_dataframe.csv')

effects_dataframe <- effects_dataframe %>%
  filter(num_matches > 70)

source("/Users/petertea/tennis_analytics/prototypes/player_height/src/plot_theme.R")

player_names <- c('Rafael Nadal', 'Denis Shapovalov', 'Roger Federer', 'Novak Djokovic', 'Nick Kyrgios')
p = ggplot() +
  
  geom_vline(xintercept = 0, size = 0.75,linetype='dashed') +
  geom_hline(yintercept = 0, size = 0.75, linetype='dashed') +
  
  geom_point(data = effects_dataframe %>% filter(! player %in% player_names), 
             aes(y = avg_ace_allowed_effect,
                 x = avg_ace_hit_effect),
             fill="gray", shape = 21, size = 2, alpha = 0.3) +
  geom_point(data = effects_dataframe %>% filter(player %in% player_names), 
             aes(y = avg_ace_allowed_effect,
                 x = avg_ace_hit_effect),
             fill="#00CCBB", shape = 21, size = 3, alpha = 0.95) +
  labs(title = 'Aces Hit vs. Aces Allowed',
       x='Aces Hit Effect',
       y = 'Aces Allowed Effect',
       caption="On-The-T"
  ) +
  plot_theme(family_font = 'Tahoma') +
  theme(legend.position = c(0.05, -0.15)) 

plotly::ggplotly(p) %>%
  plotly::config(displayModeBar = F)



ggplot() +
  geom_vline(xintercept = 0, size = 0.75,linetype='dashed') +
  geom_hline(yintercept = 0, size = 0.75, linetype='dashed') +
  
  geom_point(data = effects_dataframe %>% filter(! player %in% player_names), 
             aes(y = avg_ace_allowed_effect_pr,
                 x = avg_ace_hit_effect_pr),
             fill="gray", shape = 21, size = 2, alpha = 0.3) +
  geom_point(data = effects_dataframe %>% filter(player %in% player_names), 
             aes(y = avg_ace_allowed_effect_pr,
                 x = avg_ace_hit_effect_pr),
             fill="#00CCBB", shape = 21, size = 3, alpha = 0.95) +
  labs(title = 'Aces Hit vs. Aces Allowed',
       x='Aces Hit Effect',
       y = 'Aces Allowed Effect',
       caption="On-The-T"
  ) +
  plot_theme(family_font = 'Tahoma') +
  theme(legend.position = c(0.05, -0.15))






player_choices <- effects_dataframe$player[order(desc(effects_dataframe$num_matches))]



# -- Shiny App -----
library(shiny)
library(ggplot2)


ui <- fluidPage(
            # Give the page a title
            titlePanel(h1("Prototype Ace Effects (1991 - 2019)",
                          align = 'center'),
                       windowTitle = "HEllo"),
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
                  plotOutput("scatter_plot", 
                             hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                
                #plotOutput("scatter_plot")  
              )
              
            )
  
  
)

# Define a server for the Shiny app
server <- function(input, output) {
  

  # Fill in the spot we created for a plot
  output$scatter_plot <- renderPlot({
    
    ggplot() +
      
      geom_vline(xintercept = 0, size = 0.75,linetype='dashed') +
      geom_hline(yintercept = 0, size = 0.75, linetype='dashed') +
      
      geom_point(data = effects_dataframe %>% filter(! player %in% input$player_choice), 
                 aes(y = avg_ace_allowed_effect_pr,
                     x = avg_ace_hit_effect_pr),
                 fill="gray", shape = 21, size = 2, alpha = 0.3) +
      geom_point(data = effects_dataframe %>% filter(player %in% input$player_choice), 
                 aes(y = avg_ace_allowed_effect_pr,
                     x = avg_ace_hit_effect_pr),
                 fill="#00CCBB", shape = 21, size = 3, alpha = 0.95) +
      labs(title = 'Comparing Aces Hit to Aces Allowed',
           x='Pr(Aces Hit) Above Average',
           y = 'Pr(Aces Allowed) Above Average',
           caption="On-The-T"
      ) +
      plot_theme(family_font = 'Tahoma') +
      theme(legend.position = c(0.05, -0.15))
    
    

  }, height = 500, width = 600)
}

shinyApp(ui = ui, server = server)


library(rsconnect)
rsconnect::deployApp("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/aces_allowed_koval/prototype_shiny")



