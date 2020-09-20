## -- Calculate Average career stats from 2010 - 2019 inclusive
# Example: Average aces hit per year, average aces allowed per year

# -- Load libraries
library(dplyr)

# -- Own funcitons
source('/Users/petertea/tennis_analytics/prototypes/player_height/src/process_data.R')
source('/Users/petertea/tennis_analytics/prototypes/player_height/src/abbreviated_name.R')

# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# Process End-Of-Year Rankings data
# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# -- collect the rankings data and save.

# -- ATP end of year rankings data
atp_root_path <- "/Users/petertea/Documents/tennis_data/tennis_atp/atp_rankings_"
atp_rankings <- get_end_of_year_rankings(root_path = atp_root_path)
atp_filename <- "/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/end_of_year_atp_rankings.csv"
write.csv(x = atp_rankings, row.names = FALSE,
          file = atp_filename)

# -- WTA end of year rankings data
wta_root_path <- "/Users/petertea/Documents/tennis_data/tennis_wta/wta_rankings_"
wta_rankings <- get_end_of_year_rankings(root_path = wta_root_path,
                                         wta_flag = TRUE)
wta_filename <- "/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/end_of_year_wta_rankings.csv"
write.csv(x = wta_rankings, row.names = FALSE,
          file = wta_filename)

# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# END -- Process End-Of-Year Rankings data
# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #




# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# Process Match Summary Data from 2010 --> 2019
# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #

# --- Save ATP Data
datalist <- list()
year_vector <- 2010:2019
for(year_index in 1:10){
  year <- year_vector[year_index]
  datalist[[year_index]] <- process_atp_match_data(year = year, 
                                              data_path = "/Users/petertea/Documents/tennis_data/tennis_atp/")
}

atp_match_data_10_19 <- do.call(rbind, datalist)



# -- Convert server and returner columns to characters (currently as factors)
atp_match_data_10_19 <- atp_match_data_10_19 %>%
  mutate(server = as.character(server),
         returner = as.character(returner))

# -- Count how many matches each player played in this time frame
#player_names_wth_sufficient_matches <- atp_match_data_10_19 %>%
#  group_by(server) %>%
#  summarise(matches_played = n()) %>%
#  filter(matches_played > 50) %>%
#  .$server 


# -- Calculate service rates for each match
atp_match_data_10_19_with_rates <- atp_match_data_10_19 %>%
  
  # -- Only consider matches between players who played at least 50 matches in the entire decade
#  filter((server %in% player_names_wth_sufficient_matches) & 
#           (returner %in% player_names_wth_sufficient_matches)) %>%
  
  # --> Add column of df rates and ace rates
  filter((s_svpt) > 0 & (s_2ndIn > 0) ) %>%
  mutate(df_rate = s_df / (s_2ndIn + s_df),
         ace_rate = s_ace/s_svpt)


# -- Calculate averages over entire decade for each player
atp_match_data_grouped_10_19 <- atp_match_data_10_19_with_rates %>%
  group_by(returner, returner_id) %>%
  summarise(matches_played = n(),
            opp_avg_df_rate = mean(df_rate),
            opp_avg_ace_rate = mean(ace_rate)) %>%
  left_join(atp_match_data_10_19_with_rates %>% 
              group_by(server, server_id) %>%
              summarise(avg_df_rate = mean(df_rate),
                        avg_ace_rate = mean(ace_rate)),
            by = c('returner' = 'server', 'returner_id' = 'server_id')
  ) %>%
  arrange(desc(matches_played)) %>%
  rename(player = returner,
         player_id = returner_id)



# -- Modify columns to be +/- above tour average
# look at plotting functions on easydrive (side_projects --> notebook)

# Consider players who have completed 50 matches?
atp_match_data_grouped_10_19_50_matches <- atp_match_data_grouped_10_19 %>%
  filter(matches_played >= 50)

# -- What's the average aces allowed?
mean(atp_match_data_grouped_10_19_50_matches$opp_avg_ace_rate)

# -- What's the average aces hit?
mean(atp_match_data_grouped_10_19_50_matches$avg_ace_rate)

# -- Add columns in terms of above and below tour average
atp_aces <- atp_match_data_grouped_10_19_50_matches %>%
  mutate(opp_ace_rate_above_expected = opp_avg_ace_rate - mean(atp_match_data_grouped_10_19_50_matches$opp_avg_ace_rate),
          ace_rate_above_expected = avg_ace_rate -mean(atp_match_data_grouped_10_19_50_matches$avg_ace_rate) )

# -- Add player name tag
atp_aces$name_tag <- mapply(atp_aces$player,
                            FUN = abbreviated_name)

# -- Add player height data
player_height_data <- read.csv('/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/official_atp_height_2020.csv')


atp_aces <- atp_aces %>%
  left_join(player_height_data,
            by = c('player' = 'player_name'))

View(atp_aces)

write.csv(x = atp_aces,
          row.names = FALSE,
          file = '/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/processed_atp_aces_data_10_19.csv')


# ... # .. # . # .. # ... # ... # .. # . # .. # ... # ... # .. # . # .. #
# ... # .. # . # .. # ... # ... # .. # . # .. # ... # ... # .. # . # .. #
##### Make calculations across surface type -----
# ... # .. # . # .. # ... # ... # .. # . # .. # ... # ... # .. # . # .. #
# ... # .. # . # .. # ... # ... # .. # . # .. # ... # ... # .. # . # .. #

# -- Calculate averages over entire decade for each player
atp_match_data_grouped_10_19_and_by_surface <- atp_match_data_10_19_with_rates %>%
  group_by(returner, returner_id, surface) %>%
  summarise(matches_played = n(),
            opp_avg_df_rate = mean(df_rate),
            opp_avg_ace_rate = mean(ace_rate)) %>%
  left_join(atp_match_data_10_19_with_rates %>% 
              group_by(server, server_id) %>%
              summarise(avg_df_rate = mean(df_rate),
                        avg_ace_rate = mean(ace_rate)),
            by = c('returner' = 'server', 'returner_id' = 'server_id')
  ) %>%
  arrange(desc(matches_played)) %>%
  rename(player = returner,
         player_id = returner_id)



# Consider players who have completed 15 matches?
atp_aces_by_surface <- atp_match_data_grouped_10_19_and_by_surface %>%
  filter(matches_played >= 15)

# -- Add player name tag
atp_aces_by_surface$name_tag <- mapply(atp_aces_by_surface$player,
                                       FUN = abbreviated_name)

# -- Add player height data
player_height_data <- read.csv('/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/official_atp_height_2020.csv')


atp_aces_by_surface<- atp_aces_by_surface %>%
  left_join(player_height_data,
            by = c('player' = 'player_name'))

View(atp_aces_by_surface)

write.csv(x = atp_aces_by_surface,
          row.names = FALSE,
          file = '/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/processed_atp_aces_data_10_19_by_surface.csv')









