# -- Analyze & plot the Height Advantage in tennis serves

# -- Set appropriate working directory
setwd("/Users/petertea/tennis_analytics/prototypes/player_height")

# -- Load libraries
library(dplyr)
library(ggplot2)

# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# Identify Which Players to look at
# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# -- Include players who played in 50 matches in the 2010s (2010 - 2019)
# -- Include players who reached a top 20 ranking (in the labels?)
# -- Maybe plot all points from players who played 50 matches,
#    but label only the "interesting" names?

atp_ranking_filename <- "/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/end_of_year_atp_rankings.csv"
atp_rankings <- read.csv(atp_ranking_filename)

# -- Which player labels will we display on the graphic?
# -- Players who held an end-of-year ranking of 20
player_ids_to_keep <- atp_rankings %>%
  filter((year >= 2010) & (year <= 2019)) %>%
  filter(rank <= 20) %>%
  select(player) %>%
  .$player


# -- Load in processed aces data
aces_file_name <- '/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/processed_atp_aces_data_10_19.csv'
atp_aces_20_19_df <- read.csv(aces_file_name)


height_model <- lm(data = atp_aces_20_19_df,
                   formula = avg_ace_rate ~ player_height_cm + I(player_height_cm^2))
summary(height_model)

# Height vs. ace rate
ggplot(data = atp_aces_20_19_df,
       mapping = aes(x = player_height_cm,
                     y = avg_ace_rate)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x + I(x^2))
  # method='loess'


# Weight vs. ace rate
weight_model <- lm(data = atp_aces_20_19_df,
                   formula = avg_ace_rate ~ player_weight_lbs)
summary(weight_model)

ggplot(data = atp_aces_20_19_df,
       mapping = aes(x = player_weight_lbs,
                     y = avg_ace_rate)) +
  geom_point() 





##### Plots by surface type ---- 
# Consistently the same across all surfaces
aces_by_surface_filename <- '/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/processed_atp_aces_data_10_19_by_surface.csv'

atp_aces_by_surface_df <- read.csv(aces_by_surface_filename)


##### Grass ----- 
atp_aces_grass <- atp_aces_by_surface_df %>%
  filter(surface == 'Grass')

ggplot(data = atp_aces_grass,
       mapping = aes(x = player_height_cm,
                     y = avg_ace_rate)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x + I(x^2))

height_grass_model <- lm(data = atp_aces_grass,
                         formula = avg_ace_rate ~ player_height_cm + I(player_height_cm^2))

##### Clay ----- 
atp_aces_clay <- atp_aces_by_surface_df %>%
  filter(surface == 'Clay')

ggplot(data = atp_aces_clay,
       mapping = aes(x = player_height_cm,
                     y = avg_ace_rate)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x + I(x^2))

height_clay_model <- lm(data = atp_aces_clay,
                         formula = avg_ace_rate ~ player_height_cm + I(player_height_cm^2))


##### Hard ----- 
atp_aces_hard <- atp_aces_by_surface_df %>%
  filter(surface == 'Hard')

ggplot(data = atp_aces_hard,
       mapping = aes(x = player_height_cm,
                     y = avg_ace_rate)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x + I(x^2))

height_hard_model <- lm(data = atp_aces_hard,
                        formula = avg_ace_rate ~ player_height_cm + I(player_height_cm^2))

