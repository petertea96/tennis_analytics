# -- Exploratory plots that are nice to see, but didn't make the cut to be 
#    included in final analysis


##### Plots by surface type ---- 
# Ace Rate vs. Height relationship Consistently the same across all surfaces
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

