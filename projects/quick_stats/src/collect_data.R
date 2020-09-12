# ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #
# Access Sackmann data, and save relevant information in
# a .csv file
# This is a data cleaning script.
# ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #
setwd("/Users/petertea/tennis_analytics/projects/quick_stats/src")

#--> Load libraries
library(dplyr)
library(lubridate)


save_atp_match_data <- function(year){
  
  # -- link to Sackmann's data
  atp_root <- 'https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_'
  data_path <- paste(atp_root, year, '.csv', sep = "")
  
  # -- Read in data
  full_data <- read.csv(data_path)
  
  
  # Okay, now get this data in terms of server/returner...
  winner_frame <- data.frame(server =  full_data$winner_name,
                             server_id = full_data$winner_id,
                             returner = full_data$loser_name,
                             returner_id = full_data$loser_id,
                             s_svpt =  full_data$w_svpt,
                             s_1stIn =  full_data$w_1stIn,
                             s_1stWon = full_data$w_1stWon,
                             s_2ndIn = full_data$w_svpt - full_data$w_1stIn - full_data$w_df,
                             s_2ndWon =  full_data$w_2ndWon,
                             s_df = full_data$w_df,
                             s_ace = full_data$w_ace,
                             s_rpw_1st = full_data$l_1stIn - full_data$l_1stWon,
                             s_rpw_2nd = full_data$l_svpt - full_data$l_1stIn - full_data$l_2ndWon,
                             s_bp = full_data$l_bpFaced - full_data$l_bpSaved,
                             s_bp_opportunities = full_data$l_bpFaced,
                             server_won = 1,
                             tournament_name = full_data$tourney_name,
                             tournament_date = full_data$tourney_date,
                             tournament_level = full_data$tourney_level,
                             year = year,
                             score = full_data$score,
                             surface = full_data$surface,
                             stringsAsFactors = FALSE
  )
  
  loser_frame <- data.frame(server =  full_data$loser_name,
                            server_id = full_data$loser_id,
                            returner = full_data$winner_name,
                            returner_id = full_data$winner_id,
                            s_svpt =  full_data$l_svpt,
                            s_1stIn =  full_data$l_1stIn,
                            s_1stWon = full_data$l_1stWon,
                            s_2ndIn = full_data$l_svpt - full_data$l_1stIn - full_data$l_df,
                            s_2ndWon =  full_data$l_2ndWon,
                            s_df = full_data$l_df,
                            s_ace = full_data$l_ace,
                            s_rpw_1st = full_data$w_1stIn - full_data$w_1stWon,
                            s_rpw_2nd = full_data$w_svpt - full_data$w_1stIn - full_data$w_2ndWon,
                            s_bp = full_data$w_bpFaced - full_data$w_bpSaved,
                            s_bp_opportunities = full_data$w_bpFaced,
                            server_won = 0,
                            tournament_name = full_data$tourney_name,
                            tournament_date = full_data$tourney_date,
                            tournament_level = full_data$tourney_level,
                            year = year,
                            score = full_data$score,
                            surface = full_data$surface,
                            stringsAsFactors = FALSE
  )
  
  # -- Combine winner and loser server data into a single dataframe
  atp_dat <- rbind(winner_frame, loser_frame)  
  
  print(paste('Year is: ', year, sep = ''))
  print(paste('Warning, number of incomplete entries is: ', sum(!complete.cases(atp_dat)), sep = '' ))
  
  grand_slam_murray <- atp_dat %>%
    filter(server == 'Andy Murray') %>%
    filter(tournament_level == 'G')
  
  
  return(grand_slam_murray)
  
  
}




murray_data_list <- list()
murray_career_years <- 2005:2020

for(year_index in 1:length(murray_career_years)){
  year = murray_career_years[year_index]
  murray_data_list[[year_index]] <- save_atp_match_data(year = year)
}

murray_all_career_grand_slam_results <- do.call(rbind,murray_data_list)

# -- DONE

View(murray_all_career_grand_slam_results)


murray_all_career_grand_slam_results %>%
  arrange(s_bp_opportunities) %>%
  View()
