# -- Boxplot Serve Speeds
library(dplyr)
library(ggplot2)

# -- Plot jitter points of double faults
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

# -- Plot jitter points of aces
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

# -- Plot entire serve speed boxplot
plot_speed_boxplot <- function(speed_data, title = ''){
  speed_data <- speed_data %>%
    filter(Speed_KMH > 0)
  
  ggplot(zverev_ao_data, aes(x = as.factor(year), y = Speed_KMH)) + 
    geom_boxplot(aes(fill = as.factor(ServeNumber)),
                 position = position_dodge(0.9),
                 alpha = 0.65,
                 color = '#6c7a86',
                 outlier.shape = NA
    ) +
    #facet_grid(rows = vars(grouped_score)) +
    facet_wrap(~ grouped_score, ncol = 3) + 
    ylim(c(130, 225)) + 
    
    plot_jitter_aces(zverev_ao_data, 2018, 'Ahead') + 
    plot_jitter_aces(zverev_ao_data, 2018, 'Break Point') + 
    plot_jitter_aces(zverev_ao_data, 2018, 'Behind') +
    plot_jitter_aces(zverev_ao_data, 2018, 'Even') +
    plot_jitter_aces(zverev_ao_data, 2018, 'First Point') +
    plot_jitter_aces(zverev_ao_data, 2018, 'Game Point')  + 
    plot_jitter_aces(zverev_ao_data, 2019, 'Ahead') + 
    plot_jitter_aces(zverev_ao_data, 2019, 'Break Point') + 
    plot_jitter_aces(zverev_ao_data, 2019, 'Behind') +
    plot_jitter_aces(zverev_ao_data, 2019, 'Even') +
    plot_jitter_aces(zverev_ao_data, 2019, 'First Point') +
    plot_jitter_aces(zverev_ao_data, 2019, 'Game Point')  +
    plot_jitter_dfs(zverev_ao_data, 2018, 'Ahead') +
    plot_jitter_dfs(zverev_ao_data, 2018, 'Break Point') + 
    plot_jitter_dfs(zverev_ao_data, 2018, 'Behind') +
    plot_jitter_dfs(zverev_ao_data, 2018, 'Even') +
    plot_jitter_dfs(zverev_ao_data, 2018, 'First Point') +
    plot_jitter_dfs(zverev_ao_data, 2018, 'Game Point') +
    plot_jitter_dfs(zverev_ao_data, 2019, 'Ahead') +
    plot_jitter_dfs(zverev_ao_data, 2019, 'Break Point') + 
    plot_jitter_dfs(zverev_ao_data, 2019, 'Behind') +
    plot_jitter_dfs(zverev_ao_data, 2019, 'Even') +
    plot_jitter_dfs(zverev_ao_data, 2019, 'First Point') +
    plot_jitter_dfs(zverev_ao_data, 2019, 'Game Point') + 
    
    # Add aces on 2nd serve
    plot_jitter_aces(zverev_ao_data, 2019, 'Ahead', first_serve = FALSE) +
    plot_jitter_aces(zverev_ao_data, 2018, 'Game Point', first_serve = FALSE) +
    plot_jitter_aces(zverev_ao_data, 2019, 'Game Point', first_serve = FALSE) +
    geom_hline(yintercept = 167,
               linetype = 'dashed',
               size = 0.5,
               color = '#6c7a86',
               alpha = 0.7) +
    ggtitle(title) + 
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
  
}
  

