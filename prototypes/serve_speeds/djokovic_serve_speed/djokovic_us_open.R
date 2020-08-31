# -- US Open Djokovic
library(dplyr)
library(ggplot2)
library(reshape2)

# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
####                GET THE CLEANED DATA                   ####             
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
#--> Get Sackmann speed and play-by-play data
source("~/Documents/Github/serve_speeds/src/collect_speed_data_functions.R")

# collect data from US 2018 - 2020
djokovic_us_18 <- get_slam_data('Novak Djokovic',
                                year = 2018,
                                tournament = 'usopen')
djokovic_us_19 <- get_slam_data('Novak Djokovic',
                                year = 2019,
                                tournament = 'usopen')

# -- Plot the Densities
source("~/Documents/Github/serve_speeds/djokovic_serve_speed/src/plot_speed_density.R")

plot_servespeed_density(djokovic_us_17, 
                        "Novak Djokovic's Serve Speeds \nUS Open 2018",
                        tournament = 'usopen',
                        year = 2017)

plot_servespeed_density(djokovic_us_18, 
                        "Novak Djokovic's Serve Speeds \nUS Open 2018",
                        tournament = 'usopen',
                        year = 2018)

plot_servespeed_density(djokovic_us_19, 
                        "Novak Djokovic's Serve Speeds \nUS Open 2019",
                        tournament = 'usopen',
                        year = 2019)


djokovic_us <- do.call("rbind", list(djokovic_us_18, 
                                     djokovic_us_19 
                                     ))
djokovic_us <- djokovic_us %>%
  filter(Speed_KMH > 0)


ggplot(djokovic_us, aes(x = Speed_KMH, y = as.factor(year),
                          fill = as.factor(year), alpha = 0.2)) + 
  geom_density_ridges(scale = 1.0,
                      quantile_lines = TRUE, quantiles = 2) +
  facet_wrap(~ServeNumber)


# -- Grouped score

djokovic_us_1st_serve <- djokovic_us %>%
  filter(ServeNumber == 1)

djokovic_us_2nd_serve <- djokovic_us %>%
  filter(ServeNumber == 2)


ggplot(djokovic_us_2nd_serve, aes(x = Speed_KMH, y = as.factor(year),
                               fill = as.factor(year), alpha = 0.2)) + 
  geom_density_ridges(scale = 1.1,
                      quantile_lines = TRUE, quantiles = 2) +
  facet_grid(~grouped_score)


ggplot(djokovic_us_1st_serve, aes(x = Speed_KMH, y = as.factor(year),
                               fill = as.factor(year), alpha = 0.2)) + 
  geom_density_ridges(scale = 1.1,
                      quantile_lines = TRUE, quantiles = 2) +
  facet_grid(~grouped_score)

