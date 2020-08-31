library(ggplot2)
library(dplyr)
#getwd()
#write.csv(zverev_grand_slam_data_18_19,
#          "/Users/petertea/Documents/zverev_grandslam_18_19.csv",
#          row.names = FALSE)

zverev_grand_slam_data_18_19 <- read.csv("zverev_grandslam_18_19.csv")

serve_levels <- c('Break Point', 'Game Point', 'First Point', 'Behind','Ahead', 'Even')

zverev_grand_slam_data_18_19 <- zverev_grand_slam_data_18_19 %>%
  mutate(grouped_score = factor(grouped_score, 
                                levels =serve_levels )) %>%
  filter(Speed_KMH > 0)


################################################

plot_jitter_dfs <- function(dataframe, cur_year, point_type){
  geom_jitter(data = dataframe %>% 
                filter(year == cur_year, 
                       grouped_score == point_type) %>%
                filter(server_df == 1), 
              aes(x = as.factor(year), y = Speed_KMH),
              position= position_nudge(x = 0.225),
              color = 'blue',
              fill = 'black',
              alpha = 0.25) 
}

plot_jitter_aces <- function(dataframe, cur_year, point_type, 
                             first_serve = TRUE, nudge_x =  -0.225){
  
  if(first_serve){
    dataframe <- dataframe %>%
      filter(ServeNumber == 1)
  } else{
    dataframe <- dataframe %>%
      filter(ServeNumber == 2)
    nudge_x = 0.225
    
  }
  geom_jitter(data = dataframe %>% 
                filter(year == cur_year, 
                       grouped_score == point_type) %>%
                filter(server_ace == 1), 
              aes(x = as.factor(year), y = Speed_KMH),
              position= position_nudge(x = nudge_x),
              color = 'red',
              fill = 'black',
              alpha = 0.25
  )  
}

zverev_grand_slam_data_18_19 %>%
  group_by(tournament, ServeNumber) %>%
  summarise(avg_speed = mean(Speed_KMH))

zverev_grand_slam_data_18_19 %>%
  filter(ServeNumber == 2) %>%
  select(Speed_KMH) %>%
  colMeans()

###############################################33


# Define a server for the Shiny app
function(input, output) {
  
  
  zverev_grand_slam_data <- reactive({zverev_grand_slam_data_18_19 %>%
      filter(tournament == input$grand_slam)})
  
  avg_2nd_speed <- reactive({
    zverev_grand_slam_data() %>%
      filter(ServeNumber == 2) %>%
      select(Speed_KMH) %>%
      colMeans()
    
  })

  
  
  output$avg_2nd_speed <- renderText({
    avg_2nd_speed <- paste('Average 2nd serve speed: ', round(avg_2nd_speed(),0), 'KM/H')
    
  })
  
  # Fill in the spot we created for a plot
  output$speedPlot <- renderPlot({
    
    ggplot(zverev_grand_slam_data(), aes_string(x = as.factor(zverev_grand_slam_data()$year),
                                               y = zverev_grand_slam_data()$Speed_KMH)) + 
      geom_boxplot(aes_string(fill = as.factor(zverev_grand_slam_data()$ServeNumber)),
                   position = position_dodge(0.9),
                   alpha = 0.65,
                   color = '#6c7a86',
                   outlier.shape = NA
      )  + 
      #facet_grid(rows = vars(grouped_score)) +
      facet_wrap(~ grouped_score, ncol = 3) + 
     # ylim(c(130, 225)) +
      plot_jitter_aces(zverev_grand_slam_data(), 2018, 'Ahead') + 
      plot_jitter_aces(zverev_grand_slam_data(), 2018, 'Break Point') + 
      plot_jitter_aces(zverev_grand_slam_data(), 2018, 'Behind') +
      plot_jitter_aces(zverev_grand_slam_data(), 2018, 'Even') +
      plot_jitter_aces(zverev_grand_slam_data(), 2018, 'First Point') +
      plot_jitter_aces(zverev_grand_slam_data(), 2018, 'Game Point')  + 
      plot_jitter_aces(zverev_grand_slam_data(), 2019, 'Ahead') + 
      plot_jitter_aces(zverev_grand_slam_data(), 2019, 'Break Point') + 
      plot_jitter_aces(zverev_grand_slam_data(), 2019, 'Behind') +
      plot_jitter_aces(zverev_grand_slam_data(), 2019, 'Even') +
      plot_jitter_aces(zverev_grand_slam_data(), 2019, 'First Point') +
      plot_jitter_aces(zverev_grand_slam_data(), 2019, 'Game Point')  +
      plot_jitter_dfs(zverev_grand_slam_data(), 2018, 'Ahead') +
      plot_jitter_dfs(zverev_grand_slam_data(), 2018, 'Break Point') + 
      plot_jitter_dfs(zverev_grand_slam_data(), 2018, 'Behind') +
      plot_jitter_dfs(zverev_grand_slam_data(), 2018, 'Even') +
      plot_jitter_dfs(zverev_grand_slam_data(), 2018, 'First Point') +
      plot_jitter_dfs(zverev_grand_slam_data(), 2018, 'Game Point') +
      plot_jitter_dfs(zverev_grand_slam_data(), 2019, 'Ahead') +
      plot_jitter_dfs(zverev_grand_slam_data(), 2019, 'Break Point') + 
      plot_jitter_dfs(zverev_grand_slam_data(), 2019, 'Behind') +
      plot_jitter_dfs(zverev_grand_slam_data(), 2019, 'Even') +
      plot_jitter_dfs(zverev_grand_slam_data(), 2019, 'First Point') +
      plot_jitter_dfs(zverev_grand_slam_data(), 2019, 'Game Point') +
      # Add aces on 2nd serve
      plot_jitter_aces(zverev_grand_slam_data(), 2019, 'Ahead', first_serve = FALSE) +
      plot_jitter_aces(zverev_grand_slam_data(), 2018, 'Game Point', first_serve = FALSE) +
      plot_jitter_aces(zverev_grand_slam_data(), 2019, 'Game Point', first_serve = FALSE) +
      geom_hline(yintercept = avg_2nd_speed(),
                 linetype = 'dashed',
                 size = 0.5,
                 color = '#6c7a86',
                 alpha = 0.7) +
      #ggtitle('Zverev\'s Australian Open Serve \nSpeed Distributions (2018 - 2019)') + 
      ylab("Serve Speed (KM/H)") + xlab("Year") + labs(fill = "Serve Number:" ) +
      theme_bw() +
      scale_fill_manual(values=c("#FA8072", "#AFEEEE"), labels = c("1st", "2nd") ) +  
      theme(panel.background = element_rect(fill = 'white', # background colour
                                            colour = "black", # border colour
                                            size = 0.5, linetype = "solid"),
            plot.title=element_text(size = rel(1.1),
                                    face = "bold", hjust = 0.5),
            legend.position = "bottom",
            legend.title =  element_text(face = "bold", size = 9),
            legend.background = element_rect(colour = "gray"),
            legend.key = element_rect(fill = "gray90"),
            axis.title = element_text(face = "bold", size = 9),
            # --> Change facet wrap background colour
            strip.background = element_rect(fill="#DEFFDE", color = 'black')
      )
  }, height = 475, width = 650)
}

