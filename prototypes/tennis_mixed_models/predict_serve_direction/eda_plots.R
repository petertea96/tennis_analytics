##### Exploratory Serve Direction Plots
#### Get simple barcharts illustrating that players have different serve tendencies

# -- Load libraries
library(dplyr)
library(ggplot2)
extrafont::loadfonts()
source('./src/plot_theme.R')
setwd("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/predict_serve_direction/")

slam_data <- readRDS('./process_serve_location_pbp/processed_slam_pointbypoint.rds')

# -- Keep only interesting variables
training_data <- slam_data %>%
  select(ServeNumber, Speed_KMH, serve_location, server_won, server_name,
         returner_name, court_side, server_pressure, player_last_serve_loc,
         player_last_serve2_loc, year, tournament)

### Makes sense to look at proportions against the same player?
server_names <- c('Rafael Nadal', 'Novak Djokovic', 'Kevin Anderson')

# Data of 4 servers against Federer
four_servers_data <- training_data %>%
  filter(server_name %in% server_names) %>%
  filter(returner_name == 'Roger Federer') %>%
  filter(complete.cases(.))

four_servers_barchart_data <- four_servers_data %>%
  group_by(server_name, serve_location) %>%
  summarise(tot = n()) %>% 
  left_join(four_servers_data  %>%
              group_by(server_name) %>%
              summarise(total_serves = n())) %>%
  mutate(freq = tot/total_serves) %>%
  select(server_name, serve_location, freq, tot) %>%
  mutate(serve_location = ifelse(serve_location == 'B',
                                 'Body', ifelse(serve_location == 'W',
                                                'Wide', ifelse(serve_location=='T',
                                                               'T', 'You messed up Pete'))))

#four_servers_barchart_data$serve_location <- as.factor(four_servers_barchart_data$serve_location)
#levels(four_servers_barchart_data$serve_location) = c('T', 'Wide', 'Body')
# -- Barchart
ggplot(data=four_servers_barchart_data, 
       aes(x=server_name, y=freq, fill = serve_location)) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = 'Serve Locations against Roger Federer',
       x='',
       y = 'Proportion of Serves',
       fill = "",
       caption="Data: Tennis Abstract"
  ) + 
  plot_theme(family_font = 'Tahoma')


# -- Bar chart against all players
four_servers_data_against_all <- training_data %>%
  filter(server_name %in% server_names) %>%
  filter(complete.cases(.))

four_servers_barchart_data_against_all <- four_servers_data_against_all %>%
  group_by(server_name, serve_location) %>%
  summarise(tot = n()) %>% 
  left_join(four_servers_data_against_all  %>%
              group_by(server_name) %>%
              summarise(total_serves = n())) %>%
  mutate(freq = tot/total_serves) %>%
  select(server_name, serve_location, freq, tot) %>%
  mutate(serve_location = ifelse(serve_location == 'B',
                                 'Body', ifelse(serve_location == 'W',
                                                'Wide', ifelse(serve_location=='T',
                                                               'T', 'You messed up Pete'))))

ggplot(data=four_servers_barchart_data_against_all, 
       aes(x=server_name, y=freq, fill = serve_location)) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = 'Serve Locations all 2016 - 2019',
       x='',
       y = 'Proportion of Serves',
       fill = "",
       caption="Data: Tennis Abstract"
  ) + 
  plot_theme(family_font = 'Tahoma')



# Against Sam Querrey
four_servers_data_querrey <- training_data %>%
  filter(server_name %in% server_names) %>%
  filter(returner_name == 'Sam Querrey') %>%
  filter(complete.cases(.))

four_servers_barchart_data_querrey <- four_servers_data_querrey %>%
  group_by(server_name, serve_location) %>%
  summarise(tot = n()) %>% 
  left_join(four_servers_data_querrey  %>%
              group_by(server_name) %>%
              summarise(total_serves = n())) %>%
  mutate(freq = tot/total_serves) %>%
  select(server_name, serve_location, freq, tot) %>%
  mutate(serve_location = ifelse(serve_location == 'B',
                                 'Body', ifelse(serve_location == 'W',
                                                'Wide', ifelse(serve_location=='T',
                                                               'T', 'You messed up Pete'))))

#four_servers_barchart_data$serve_location <- as.factor(four_servers_barchart_data$serve_location)
#levels(four_servers_barchart_data$serve_location) = c('T', 'Wide', 'Body')
# -- Barchart
ggplot(data=four_servers_barchart_data_querrey, 
       aes(x=server_name, y=freq, fill = serve_location)) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = 'Serve Locations against Sam Querrey',
       x='',
       y = 'Proportion of Serves',
       fill = "",
       caption="Data: Tennis Abstract"
  ) + 
  plot_theme(family_font = 'Tahoma')


