# -- Djokovic Speed

# -- load libraries
library(dplyr)
library(ggplot2)
library(reshape2)

# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
####                GET THE CLEANED DATA                   ####             
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
#--> Get Sackmann speed and play-by-play data
source("~/Documents/Github/serve_speeds/src/collect_speed_data_functions.R")

# collect data from AO 2018 - 2020
djokovic_ao_18 <- get_slam_data('Novak Djokovic',
                                              year = 2018,
                                              tournament = 'ausopen')
djokovic_ao_19 <- get_slam_data('Novak Djokovic',
                                              year = 2019,
                                              tournament = 'ausopen')
djokovic_ao_20 <- get_slam_data('Novak Djokovic',
                                              year = 2020,
                                              tournament = 'ausopen')

# -- Win percentages
djokovic_ao_20 %>%
  group_by(ServeNumber) %>%
  summarise(service_pts = n(),
            service_wins = sum(won_point),
            win_perc = sum(won_point)/n())

djokovic_ao_19 %>%
  group_by(ServeNumber) %>%
  summarise(service_pts = n(),
            service_wins = sum(won_point),
            win_perc = sum(won_point)/n())

table(djokovic_ao_19$match_id)

# -- Plot the Densities
source("~/Documents/Github/serve_speeds/djokovic_serve_speed/src/plot_speed_density.R")

plot_servespeed_density(djokovic_ao_18, 
                        "Novak Djokovic's Serve Speeds \nAustralian Open 2018",
                        tournament = 'ausopen',
                        year = 2018)

plot_servespeed_density(djokovic_ao_19, 
                        "Novak Djokovic's Serve Speeds \nAustralian Open 2019",
                        tournament = 'ausopen',
                        year = 2019)

plot_servespeed_density(djokovic_ao_20, 
                        "Novak Djokovic's Serve Speeds \nAustralian Open 2020",
                        tournament = 'ausopen',
                        year = 2020)

djokovic_data <- do.call("rbind", list(djokovic_ao_18, 
                                       djokovic_ao_19, 
                                       djokovic_ao_20))
djokovic_data <- djokovic_data %>%
  filter(Speed_KMH > 0)


ggplot(djokovic_data, aes(x = Speed_KMH, y = as.factor(year),
                           fill = as.factor(year), alpha = 0.2)) + 
  geom_density_ridges(scale = 1.0,
                      quantile_lines = TRUE, quantiles = 2) +
  facet_wrap(~ServeNumber)



# -- Speed on Ad court vs. Deuce court
source("~/Documents/Github/serve_speeds/src/ad_or_deuce_court.R")
djokovic_data$court_side <- mapply(djokovic_data$server_score,
                                   djokovic_data$returner_score,
                                   FUN = ad_or_deuce)

djokovic_1st_serve <- djokovic_data %>%
  filter(ServeNumber == 1)

djokovic_2nd_serve <- djokovic_data %>%
  filter(ServeNumber == 2)

# -- 2nd serves by score situation
ggplot(djokovic_2nd_serve, aes(x = Speed_KMH, y = as.factor(year),
                          fill = as.factor(year), alpha = 0.2)) + 
  geom_density_ridges(scale = 1.1,
                      quantile_lines = TRUE, quantiles = 2) +
  facet_grid(~grouped_score)

# -- 2nd serves by serve side
ggplot(djokovic_2nd_serve, aes(x = Speed_KMH, y = as.factor(year),
                               fill = as.factor(year), alpha = 0.2)) + 
  geom_density_ridges(scale = 1.1,
                      quantile_lines = TRUE, quantiles = 2) +
  facet_grid(~court_side)



ggplot(djokovic_1st_serve, aes(x = Speed_KMH, y = as.factor(year),
                               fill = as.factor(year), alpha = 0.2)) + 
  geom_density_ridges(scale = 1.1,
                      quantile_lines = TRUE, quantiles = 2) +
  facet_grid(~grouped_score)

# -- 1st serves by serve side
ggplot(djokovic_1st_serve, aes(x = Speed_KMH, y = as.factor(year),
                               fill = as.factor(year), alpha = 0.2)) + 
  geom_density_ridges(scale = 1.1,
                      quantile_lines = TRUE, quantiles = 2) +
  facet_grid(~court_side)





