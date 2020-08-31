# -- Collect All Djokovic's serve speed data since 2018
# -- load libraries
library(dplyr)
library(ggplot2)
library(reshape2)

#--> Get Sackmann speed and play-by-play data
source("~/Documents/Github/serve_speeds/src/collect_speed_data_functions.R")


tournament_list <- c('ausopen', 'usopen', 'frenchopen', 'wimbledon')
year_list <- c(2018, 2019)

djokovic_data_list <- list()
index = 1
for(year in year_list){
  for(tournament in tournament_list){
    djokovic_data_list[[index]] <- get_slam_data('Novak Djokovic',
                                                 year = year,
                                                 tournament = tournament)
    index = index + 1
  }
  
}

djokovic_data_list[[9]] <- get_slam_data('Novak Djokovic',
                                         year = 2020,
                                         tournament = 'ausopen')

djokovic_data <- do.call("rbind", djokovic_data_list)

#djokovic_data <- djokovic_data %>%
#  filter(Speed_KMH > 0)

write.csv(djokovic_data,
          '~/Documents/Github/serve_speeds/djokovic_serve_speed/shiny/djokovic.csv',
          row.names = FALSE)
