# Double fault analysis functions

tidy_serve_data <- function(dirty_2018_data, dirty_2019_data,
                            double_fault_flag = TRUE){
  
  if(double_fault_flag){
    serve_dat_2018 <- dirty_2018_data %>%
      filter(server_df == 1)
    
    serve_dat_2019 <- dirty_2019_data %>%
      filter(server_df == 1)
  } else {
    serve_dat_2018 <- dirty_2018_data %>%
      filter(server_ace == 1)
    
    serve_dat_2019 <- dirty_2019_data %>%
      filter(server_ace == 1)
  }
  
  # Calculate player proportion of points ending up as double fault/ace
  proportions_2018 <- (table(serve_dat_2018$grouped_score)) / (table(dirty_2018_data$grouped_score)) 
  proportions_2019 <- (table(serve_dat_2019$grouped_score)) / (table(dirty_2019_data$grouped_score)) 
  
  
  serve_proportions_data <- cbind(proportions_2018, 
                                 proportions_2019) %>%
    as.data.frame()
  
  serve_proportions_data <- serve_proportions_data %>% 
    tibble::rownames_to_column("Situation") 
  
  serve_proportions_data_to_plot <- reshape2::melt(serve_proportions_data)
  
  serve_levels <- c('Break Point', 'Behind', 'Game Point', 'Ahead', 'First Point', 'Even')
  
  serve_proportions_data_to_plot <- serve_proportions_data_to_plot %>%
    rename(Year = variable, proportion = value) %>%
    mutate(Year = ifelse(grepl('2018',Year),
                         '2018', 
                         '2019'),
           Situation = factor(Situation, 
                              levels =rev(serve_levels) ))
  
  
  return(serve_proportions_data_to_plot)
}


serve_result_proportions_data <- function(grand_slam_data,
                                     double_fault_flag){
  # Calculate 2018 and 2019 metrics
  #Player's 2018 szn
  grand_slam_2018 <- grand_slam_data %>%
    filter(year ==2018)
  
  #Player's 2019 szn
  grand_slam_2019 <- grand_slam_data %>%
    filter(year ==2019)
  
  double_faults_to_plot <- tidy_serve_data(dirty_2018_data = grand_slam_2018,
                  dirty_2019_data = grand_slam_2019,
                  double_fault_flag = double_fault_flag)
  
  
  return(double_faults_to_plot)
}



peter_theme <- function(bg_colour = "#DBF5F0"){
  theme(panel.background = element_rect(#fill = "#DBF5F0", # background colour
    #light green: ##DBF5E8
    # light yellow: #F8FCCB
    colour = "black", # border colour
    size = 0.2, linetype = "solid"),
    plot.title=element_text(size = rel(1.3),
                            face = "bold", hjust = 0.5),
    axis.text.y = element_text(face="bold", color="black", 
                               size=12),
    axis.text.x = element_text(color="black", 
                               size=10),
    axis.line = element_line(colour = "black", 
                             size = 0.3, linetype = "solid"),
    #axis.title.y=element_text(colour = "black", face = "bold",
    #                          size = 12),
    axis.title.x = element_text(colour = "black", face = "bold",
                                size = 12) ,
    plot.background = element_rect(fill = bg_colour,
                                   colour = "black",size = 1),
    panel.grid.major = element_line(size = 0.025, linetype = 'solid',
                                    colour = "#6c7a86"), 
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "#6c7a86"),
    legend.background = element_rect(#fill = "#DBF5F0",
      colour = "black",size = 0.3),
    legend.title =  element_text(colour = "black", face = "bold",
                                 size = 10)
  )
}

plot_serve_proportions <- function(title_name, my_data,
                                   y_upper_limit = 0.155,
                                   bg_colour = "#DBF5F0",
                                   y_axis_title = "Double Fault Rate"){
  ggplot(data = my_data,
         aes(x = Situation, y = proportion, fill = Year)) +
    geom_bar(stat = "identity", 
             color="#6c7a86", 
             width = 0.8,
             position=position_dodge(0.9)) +
    geom_label(data = my_data %>% filter(Year == '2018'),
               aes(x = Situation,
                   y = proportion + 0.015, 
                   fontface = "bold",
                   label = paste(round(proportion,3), sep = "")),
               position= position_nudge(x = -0.225),
               size = 2.5,
               show.legend = FALSE
    ) +
    geom_label(data = my_data %>% filter(Year == '2019'),
               aes(x = Situation,
                   y = proportion + 0.015, 
                   fontface = "bold",
                   label = paste(round(proportion,3), sep = "")),
               position= position_nudge(x = 0.225),
               size = 2.5,
               show.legend = FALSE
    ) +
    ylim(0, y_upper_limit) +
    coord_flip() + 
    xlab("") +
    theme_bw() + 
    ylab(y_axis_title) + 
    ggtitle(title_name) + 
    scale_fill_manual(values=c("#FCFAF1", "#FACCAD")) +
    peter_theme(bg_colour)
  
  
}
  

