### Create data frame of player names and associated player IDs

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/collect_data/")

library(dplyr)

match_data_log <- read.csv("matches_in_repo.csv")

match_data_log <- match_data_log %>%
  select(player1, player1_id, player2, player2_id) %>%
  mutate(player1 = gsub('\\s+', '', player1),
         player2 = gsub('\\s+', '', player2))

player1_df <- match_data_log[,c('player1', 'player1_id')]
colnames(player1_df) <- c('name', 'id')

player2_df <- match_data_log[,c('player2', 'player2_id')]
colnames(player2_df) <- c('name', 'id')

player_df <- rbind(player1_df, 
                   player2_df )

player_df <- player_df[!duplicated(player_df),]


write.csv(player_df,
          'player_id.csv',
          row.names = FALSE)
