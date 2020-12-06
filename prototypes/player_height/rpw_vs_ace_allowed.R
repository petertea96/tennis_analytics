## - Plot RPW% vs. Ace prevented %


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

atp_filename <- "/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/end_of_year_atp_rankings.csv"

atp_rankings <- read.csv(atp_filename)


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


atp_match_data_grouped_10_19_total <-atp_match_data_10_19 %>%
  filter((s_svpt) > 0 & (s_2ndIn > 0) ) %>%
  group_by(returner, returner_id) %>%
  summarise(matches_played = n(),
            total_opponent_df = sum(s_df),
            total_opponent_ace = sum(s_ace),
            total_opponent_serves = sum(s_svpt),
            total_opponent_2nd_serves = sum(s_svpt) - sum(s_1stIn),
            total_rpw = sum(s_svpt) - sum(s_1stWon) - sum(s_2ndWon),
            rpw_perc = total_rpw/total_opponent_serves) %>%
  left_join(atp_match_data_10_19 %>% 
              group_by(server, server_id) %>%
              summarise(total_df = sum(s_df),
                        total_ace = sum(s_ace),
                        total_serves = sum(s_svpt),
                        total_2nd_serves = sum(s_svpt) - sum(s_1stIn)),
            by = c('returner' = 'server', 'returner_id' = 'server_id')
  ) %>%
  arrange(desc(matches_played)) %>%
  rename(player = returner,
         player_id = returner_id) %>%
  mutate(ace_rate = total_ace / total_serves,
         df_rate = total_df / total_2nd_serves,
         opp_ace_rate = total_opponent_ace/ total_opponent_serves,
         opp_df_rate = total_opponent_df/total_opponent_2nd_serves) 

# Consider players who have completed 50 matches?
atp_match_data_grouped_10_19_50_matches <- atp_match_data_grouped_10_19_total %>%
  filter(matches_played >= 50)

atp_match_data_grouped_10_19_50_matches %>%
  arrange(desc(rpw_perc)) %>%
  View()

# -- Plot RPW% vs. Ace allowed rate
# -- Should probably expect a negative association???

ggplot(atp_match_data_grouped_10_19_50_matches,
       aes(x=rpw_perc, y=opp_ace_rate)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x + I(x^2) + I(x^3),
              color = '#83D0E9')






unique(atp_match_data_grouped_10_19_total %>% filter(matches_played > 50) %>% .$player)





