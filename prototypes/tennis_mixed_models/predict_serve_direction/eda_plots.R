##### Exploratory Serve Direction Plots
#### Get simple barcharts illustrating that players have different serve tendencies

# -- Load libraries
library(dplyr)
library(ggplot2)
extrafont::loadfonts()
setwd("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/predict_serve_direction/")
source('./src/plot_theme.R')

filename <- "/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/predict_serve_direction/process_serve_location_pbp/processed_modified_body_serve_slam_pointbypoint.rds"
slam_data <- readRDS(filename)

# -- Keep only interesting variables
training_data <- slam_data %>%
  select(ServeNumber, Speed_KMH, server_won, server_name, serve_location,
         returner_name, court_side, server_pressure, year, 
         tournament)

# training_data <- cbind(training_data,
#                        nnet::class.ind(training_data$server_pressure)
# ) %>%
#   as.data.frame()
# 
# training_data <- training_data %>% 
#   mutate(serve_loc_hand = sapply(serve_loc_hand, toString),
#          player_last_serve2_binary_loc = sapply(player_last_serve2_binary_loc, toString),
#          player_last_serve_binary_loc = sapply(player_last_serve_binary_loc, toString))

table(training_data$serve_location)
training_data <- training_data %>%
  mutate(is_body = ifelse(serve_location == 'B', 'Body', 'Not Body'))
### Makes sense to look at proportions against the same player?
server_names <- c('Rafael Nadal', 'Novak Djokovic', 'Kevin Anderson')

# Data of 4 servers against Federer
four_servers_data <- training_data %>%
  filter(server_name %in% server_names) %>%
  filter(returner_name == 'Roger Federer') %>%
  filter(complete.cases(.))

four_servers_barchart_data <- four_servers_data %>%
  group_by(server_name, is_body) %>%
  summarise(tot = n()) %>% 
  left_join(four_servers_data  %>%
              group_by(server_name) %>%
              summarise(total_serves = n())) %>%
  mutate(freq = tot/total_serves) %>%
  select(server_name, is_body, freq, tot) 
#four_servers_barchart_data$is_body <- as.factor(four_servers_barchart_data$is_body)
#levels(four_servers_barchart_data$is_body) = c('Body', 'Not Body')
# -- Barchart
ggplot(data=four_servers_barchart_data, 
       aes(x=server_name, y=freq, fill = is_body)) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = 'Serve Directions against Roger Federer\n(2016 - 2019)',
       x='',
       y = 'Proportion of Serves',
       fill = "Serve Direction",
       caption="Data: Tennis Abstract"
  ) + 
  plot_theme(family_font = 'Tahoma')



ggsave('serve_directions_against_federer.jpg',
       width=7.25, height=5.5,
       dpi = 450)

# -- Bar chart against all players
four_servers_data_against_all <- training_data %>%
  filter(server_name %in% server_names) %>%
  filter(returner_name != 'Roger Federer') %>%
  filter(complete.cases(.))

four_servers_barchart_data_against_all <- four_servers_data_against_all %>%
  group_by(server_name, is_body) %>%
  summarise(tot = n()) %>% 
  left_join(four_servers_data_against_all  %>%
              group_by(server_name) %>%
              summarise(total_serves = n())) %>%
  mutate(freq = tot/total_serves) %>%
  select(server_name, is_body, freq, tot)

ggplot(data=four_servers_barchart_data_against_all, 
       aes(x=server_name, y=freq, fill = is_body)) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = 'Serve Directions against Everyone Else \n(2016 - 2019)',
       x='',
       y = 'Proportion of Serves',
       fill = "Serve Direction",
       caption="Data: Tennis Abstract"
  ) + 
  plot_theme(family_font = 'Tahoma')


ggsave('serve_directions_against_everyone_else.jpg',
       width=7.25, height=5.5,
       dpi = 450)

# Against Sam Querrey
four_servers_data_querrey <- training_data %>%
  filter(server_name %in% server_names) %>%
  filter(returner_name == 'Sam Querrey') %>%
  filter(complete.cases(.))

four_servers_barchart_data_querrey <- four_servers_data_querrey %>%
  group_by(server_name, is_body) %>%
  summarise(tot = n()) %>% 
  left_join(four_servers_data_querrey  %>%
              group_by(server_name) %>%
              summarise(total_serves = n())) %>%
  mutate(freq = tot/total_serves) %>%
  select(server_name, is_body, freq, tot) 
#four_servers_barchart_data$serve_location <- as.factor(four_servers_barchart_data$serve_location)
#levels(four_servers_barchart_data$serve_location) = c('T', 'Wide', 'Body')
# -- Barchart
ggplot(data=four_servers_barchart_data_querrey, 
       aes(x=server_name, y=freq, fill = is_body)) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = 'Serve Directions against Sam Querrey \n(2016 - 2019)',
       x='',
       y = 'Proportion of Serves',
       fill = "",
       caption="Data: Tennis Abstract"
  ) + 
  plot_theme(family_font = 'Tahoma')


