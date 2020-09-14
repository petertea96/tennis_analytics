## -- Calculate Average career stats
# Example: Average aces hit per year, average aces allowed per year

# Load libraries
library(dplyr)
source('/Users/petertea/tennis_analytics/prototypes/player_height/src/process_data.R')

# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# Process End-Of-Year Rankings data
# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# -- collect the data

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
# Identify Which Players to look at
# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# -- Include players who played in ___ matches
# -- Include players who reached a top 20 ranking


atp_rankings <- read.csv(atp_filename)

player_ids_to_keep <- atp_rankings %>%
  filter((year >= 2010) & (year <= 2019)) %>%
  filter(rank <= 20) %>%
  select(player) %>%
  .$player


# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# Collect Match Summary Data from 2010 --> 2019
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

all_data <- do.call(rbind, datalist)

atp_full_data <- all_data %>%
  #filter(server_id %in% player_ids_to_keep) %>%
  # --> Add column of df rates and ace rates
  filter((s_svpt) > 0 & (s_2ndIn > 0) ) %>%
  mutate(df_rate = s_df / (s_2ndIn + s_df),
         ace_rate = s_ace/s_svpt)



atp_grouped_results <- atp_full_data %>%
  #filter( (server_rank <= 50) & (returner_rank <= 50)) %>%
  group_by(returner, returner_id) %>%
  summarise(matches_played = n(),
            opp_avg_df_rate = mean(df_rate),
            opp_avg_ace_rate = mean(ace_rate)) %>%
  left_join(atp_full_data %>% 
              group_by(server, server_id) %>%
              summarise(avg_df_rate = mean(df_rate),
                        avg_ace_rate = mean(ace_rate)),
            by = c('returner' = 'server', 'returner_id' = 'server_id')
  )

atp_grouped_results %>%
  filter(returner_id %in% player_ids_to_keep) %>%
  arrange(desc(avg_ace_rate)) %>%
  View()



# look at plotting functions on easydrive (side_projects --> notebook)