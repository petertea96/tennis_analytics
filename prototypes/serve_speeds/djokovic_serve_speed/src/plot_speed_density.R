# --Plot serve speed densities
library(ggplot2)

plot_servespeed_density <- function(my_data, plot_title, tournament, year, y=0.05, x1 = 110, x2=210){
  
  # -- No serve speeds of 0
  my_data <- my_data %>%
    filter(Speed_KMH > 0)
  
  #Calculate the average speeds of 1st serve and 2nd serve
  Avg_speeds <- my_data %>%
    group_by(ServeNumber) %>%
    summarise(mean(Speed_KMH))
  
  Avg_first_serve <- Avg_speeds$`mean(Speed_KMH)`[1]
  Avg_second_serve <- Avg_speeds$`mean(Speed_KMH)`[2]
  
  #if (tournament == "wimbledon"){
  #  background_colour <- "#F0FAE3"
  #} else if (tournament == "usopen") {
  #  background_colour <- "#f3f6fc"
  #} else if (tournament == "ausopen"){
  #  background_colour <- "#E8F7FF"
  #}
  background_colour <- "#f3f6fc"
  
  #if (tournament == "usopen"){
  #  tournament <- "US Open"
  #}
  
  #if (tournament == "ausopen"){
  #  tournament <- "Australian Open"
  #}
  
  
  myplot <- ggplot(my_data, aes(x=Speed_KMH, fill=as.factor(ServeNumber) )) +
    geom_density( aes(y = ..density.., fill=as.factor(ServeNumber)), alpha = 0.6) + 
    #ggtitle(plot_title) + 
    xlab("Speed (KM/H)") + ylab("Density") + labs(fill = "Serve Number" ) +
    theme_classic() +
    
    ##### ----- ##### - Adding vertical lines denoting mean serve speeds - ##### ----- #####
  geom_vline(xintercept = Avg_first_serve, linetype = "dashed", colour = "#25884e") +
    geom_text(aes(x = Avg_first_serve - 6, y = y),
              label=paste( as.character(round(Avg_first_serve)), "KM/H", sep = " "),
              size=3, color = "#25884e") +
    
    geom_vline(xintercept = Avg_second_serve, linetype = "dashed", colour = "#f97148") + 
    geom_text(aes(x = Avg_second_serve  - 6, y = y),
              label=paste(as.character(round(Avg_second_serve)), "KM/H", sep = " "),
              size=3, color = "#f97148") +
    ##### ----- ##### - Adding vertical lines denoting mean serve speeds - ##### ----- #####
  
  ##### ----- ##### - Choose specific colours to fill, also change labels - ##### ----- #####
  scale_fill_manual(values=c("#4be269", "#fc9559"), labels = c("1st", "2nd") ) +  
    theme(panel.background = element_rect(fill = background_colour, # background colour
                                          colour = "black", # border colour
                                          size = 0.5, linetype = "solid"),
          plot.title=element_text(size = rel(1.6),
                                  face = "bold", hjust = 0.5),
          legend.position = "bottom",
          legend.background = element_rect(colour = "gray"),
          legend.key = element_rect(fill = "gray90"),
          axis.title = element_text(face = "bold", size = 13)) #+
  # xlim(c(x1,x2))
  
  return(myplot)
  
}
