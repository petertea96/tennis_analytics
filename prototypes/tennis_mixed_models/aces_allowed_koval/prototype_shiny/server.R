# -- Shiny App -----
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
source("./src/plot_theme.R")


effects_dataframe <-read.csv('./effects_dataframe.csv')

effects_dataframe <- effects_dataframe %>%
  filter(num_matches > 70)

player_choices <- effects_dataframe$player[order(desc(effects_dataframe$num_matches))]


# Define a server for the Shiny app
function(input, output) {
  
  
  # Fill in the spot we created for a plot
  output$scatter_plot <- renderGirafe({
    
    # text_position_right_x <- 0.09
    # text_position_up_y <- 0.085
    # text_position_down_y <- -0.05
    # text_position_left_x <- -0.15
    
    ggriph_plot = ggplot() +
      # annotate(geom="text", #x=172.5, 
      #          x= text_position_right_x,
      #          y=text_position_up_y,
      #          hjust = 0,
      #          size = 3.5,
      #          family = 'Tahoma',
      #          fontface = 'bold',
      #          label="Above Average Server",
      #          color="#058527") +
      # annotate(geom="text", #x=172.5, 
      #          x= text_position_right_x,
      #          y=text_position_up_y - 0.5,
      #          hjust = 0,
      #          size = 3.5,
      #          family = 'Tahoma',
      #          fontface = 'bold',
      #          label="Below Average Returner",
      #          color="red") +
      # annotate("rect", xmin = text_position_right_x - 0.5, 
      #          xmax = Inf, 
      #          ymin = text_position_up_y - 1,
      #          ymax = Inf,
      #          alpha = .1) + 
      # 
      # # -- Manually adding Lower Right Quadrant
      # annotate(geom="text", #x=172.5, 
      #          x= text_position_right_x,
      #          y=text_position_down_y,
      #          hjust = 0,
      #          size = 3.5,
      #          family = 'Tahoma',
      #          fontface = 'bold',
      #          label="Above Average Server",
      #          color="#058527") +
      # annotate(geom="text", #x=172.5, 
      #          x= text_position_right_x,
      #          y=text_position_down_y - 0.5,
      #          hjust = 0,
      #          size = 3.5,
      #          family = 'Tahoma',
      #          fontface = 'bold',
      #          label="Above Average Returner",
      #          color="#058527") +
      # annotate("rect", xmin = text_position_right_x - 0.5, 
      #          xmax = Inf, 
      #          ymin = -Inf, ymax = text_position_down_y +0.5,
      #          alpha = .1) +
      geom_vline(xintercept = 0, size = 0.75,linetype='dashed') +
      geom_hline(yintercept = 0, size = 0.75, linetype='dashed') +
      geom_point(data = effects_dataframe %>% filter(! player %in% input$player_choice), 
                 aes(y = avg_ace_allowed_effect_pr,
                     x = avg_ace_hit_effect_pr),
                 fill="gray", shape = 21, size = 2, alpha = 0.3) +
      geom_point_interactive(data = effects_dataframe %>% filter(player %in% input$player_choice), 
                             aes(y = avg_ace_allowed_effect_pr,
                                 x = avg_ace_hit_effect_pr,
                                 data_id = player, 
                                 tooltip = paste(player,'\n', 
                                                 '(Above Average)\n',
                                                 'Pr(Ace Allowed): ', 
                                                 round(avg_ace_allowed_effect_pr,3), '\n',
                                                 'Pr(Ace Hit): ',
                                                 round(avg_ace_hit_effect_pr,3),
                                                 sep = '')
                                 ),
                             fill="#00CCBB", shape = 21, size = 3, alpha = 0.95) +
      labs(title = '',
           x='Pr(Aces Hit) Above Average',
           y = 'Pr(Aces Allowed)\nAbove Average',
           caption="On-The-T"
      ) +
      # scale_x_continuous(
      #   limits = c(-0.15,0.19),
      #   breaks = c(-0.10, -0.05, 0, 0.05, 0.1, 0.15),
      #   labels=c("-0.1" = "-10 %", '-0.05' ="-5 %", '0' = '0 %', "0.05" = "+5 %", "0.1" = "+10 %",
      #            "0.15" = "+15 %")
      # ) +
      # scale_y_continuous(
      #   limits = c(-0.055,0.085),
      #   breaks = c(-0.05, -0.025, 0, 0.025, 0.05, 0.075),
      #   labels=c('-0.05' ="-5 %", "-0.025" = "-2.5 %","0" = "0 %", "0.025" = "+2.5 %",
      #            '0.05' ="+5 %", '0.075' ="+7.5 %")
      # ) +
      plot_theme(family_font = 'Tahoma') +
      theme(legend.position = c(0.05, -0.15))
    
    tooltip_css <- "background-color:#00CCBB;white;font-style:bold;padding:7px;border-radius:5px;"
    
    girafe(ggobj = ggriph_plot,
           options = list(opts_tooltip(css = tooltip_css),
                          opts_sizing(width = .7)))
    
    
    
    
  })
}


# shinyApp(ui = ui, server = server)
