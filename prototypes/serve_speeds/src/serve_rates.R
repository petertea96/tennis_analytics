# Serve Rates

prepare_serve_rates_data <- function(entire_data,
                                     years_list = c(2018,2019), df_flag = TRUE){
  # 2018 szn
  entire_2018 <- entire_data %>%
    filter(year == years_list[1])
  
  # 2019 szn
  entire_2019 <- entire_data %>%
    filter(year == years_list[2])
  
  if(df_flag){
    #2018 double faults
    df_2018 <- entire_2018 %>%
      filter(server_df == 1)
    
    # 2019 double faults
    df_2019 <- entire_2019 %>%
      filter(server_df == 1)
  } else{
    
    #2018 aces
    df_2018 <- entire_2018 %>%
      filter(server_ace == 1)
    
    # 2019 aces
    df_2019 <- entire_2019 %>%
      filter(server_ace == 1)
  }

  
  rates_2018 <- (table(df_2018$grouped_score)) / (table(entire_2018$grouped_score)) 
  rates_2019 <- (table(df_2019$grouped_score)) / (table(entire_2019$grouped_score)) 
  
  rates <- cbind(rates_2018, rates_2019) %>% as.data.frame()
  rates <- rates %>% 
    tibble::rownames_to_column("Situation") 
  rates <- reshape2::melt(rates)
  
  rates <- rates %>%
    rename(Year = variable, proportion = value) %>%
    mutate(Year = ifelse(Year == 'rates_2018',
                         '2018', 
                         '2019'),
           Situation = factor(Situation, 
                              levels =rev(c('Break Point', 'Behind', 'Game Point', 'Ahead',
                                         'First Point', 'Even' )) ))
  
  return(rates)
  
}




# -- Plotting Function

plot_serve_rates <- function(rates_data, df_flag = TRUE){
  
  if(df_flag){
    title = 'Zverev\'s Recent Grand Slams \nDouble Fault Rates'
    y_axis_label = "Double Fault Rate"
    ylim_upper = 0.155
    bg_colour = "#DBF5F0"
  } else{
    title = 'Zverev\'s Recent Grand Slams \nAce Rates'
    y_axis_label = "Ace Rate"
    ylim_upper = 0.2
    bg_colour = '#ffcccc'
  }
  
  ggplot(data = rates_data,
         aes(x = Situation, y = proportion, fill = Year)) +
    geom_bar(stat = "identity", 
             color="#6c7a86", 
             width = 0.8,
             position=position_dodge(0.9)) +
    geom_label(data = rates_data %>% filter(Year == '2018'),
               aes(x = Situation,
                   y = proportion + 0.015, 
                   label = paste(round(proportion,3), sep = "")),
               position= position_nudge(x = -0.225),
               size = 2,
               show.legend = FALSE,
               fontface = 'bold'
    ) +
    geom_label(data = rates_data %>% filter(Year == '2019'),
               aes(x = Situation,
                   y = proportion + 0.015, 
                   label = paste(round(proportion,3), sep = "")),
               position= position_nudge(x = 0.225),
               size = 2,
               show.legend = FALSE,
               fontface = 'bold'
    ) +
    ylim(0, ylim_upper) +
    coord_flip() + 
    xlab("") +
    theme_bw() + 
    ylab(y_axis_label) + 
    ggtitle(title)+ 
    scale_fill_manual(values=c("#FCFAF1", "#FACCAD"))+
    theme(panel.background = element_rect(#fill = "#DBF5F0", # background colour
      #light green: ##DBF5E8
      # light yellow: #F8FCCB
      colour = "black", # border colour
      size = 0.2, linetype = "solid"),
      plot.title=element_text(size = rel(1.1),
                              face = "bold", hjust = 0.5),
      axis.text.y = element_text(face="bold", color="black", 
                                 size=10),
      axis.text.x = element_text(face="bold", color="black", 
                                 size=9),
      axis.line = element_line(colour = "black", 
                               size = 0.3, linetype = "solid"),
      #axis.title.y=element_text(colour = "black", face = "bold",
      #                          size = 12),
      axis.title.x = element_text(colour = "black", face = "bold",
                                  size = 10) ,
      plot.background = element_rect(fill = bg_colour,
                                     colour = "black",size = 1),
      panel.grid.major = element_line(size = 0.025, linetype = 'solid',
                                      colour = "#6c7a86"), 
      panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                      colour = "#6c7a86"),
      legend.background = element_rect(#fill = "#DBF5F0",
        colour = "black",size = 0.1),
      legend.title =  element_text(colour = "black", face = "bold",
                                   size = 10)
      
      
    )
  
}
