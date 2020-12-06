##### Fit Mixed Models (GLMMs & GAMMs)

# -- Load libraries
library(dplyr)
library(ggplot2)
library(mgcv)
library(lme4)

setwd("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/predict_serve_direction/")

# --> Load Data
slam_data <- readRDS('./process_serve_location_pbp/processed_slam_pointbypoint.rds')

# -- Keep only interesting variables
training_data <- slam_data %>%
  select(ServeNumber, Speed_KMH, serve_location, server_won, server_name,
         returner_name, court_side, server_pressure, player_last_serve_loc,
         player_last_serve2_loc, year, tournament)

# -- Add player height & handedness data
player_height_data <- read.csv('/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/official_atp_height_2020.csv')

full_slam_data <- slam_data %>%
  
  left_join(player_height_data %>% select(player_handedness, player_name),
            by = c('server_name' = 'player_name')) %>%
  rename('server_handedness' = 'player_handedness') %>%
  
  left_join(player_height_data %>% select(player_handedness, player_name),
            by = c('returner_name' = 'player_name')) %>%
  rename('returner_handedness' = 'player_handedness') %>%
  filter(complete.cases(.))

