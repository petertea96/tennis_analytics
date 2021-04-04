library(dplyr)

match_data_log <- read.csv("summary_points_avail_df.csv")

player_name <- 'N.DJOKOVIC'

match_data_log <- match_data_log %>%
  mutate(player1 = gsub('\\s+', '', player1),
         player2 = gsub('\\s+', '', player2)) %>%
  filter((player1 == player_name) | (player2==player_name))

match_data_log %>%
  select(filename)
