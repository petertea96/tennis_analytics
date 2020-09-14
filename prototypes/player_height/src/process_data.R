# ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #
# Access Sackmann data, and save relevant information in
# a .csv file
# This is a data cleaning script.
# ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #

#--> Load libraries
library(dplyr)
library(lubridate)

#year <- 2019
#data_path <- "/Users/petertea/Documents/tennis_data/tennis_atp/"
process_atp_match_data <- function(year, data_path){
  
  # Process ATP match data for a given year
  
  # Args:
  # ______
  # year [int]
  # data_path [str]: path to data
  
  # Returns:
  # _______
  # DataFrame
  
  # -- link to Sackmann's data
  full_data_path <- paste(data_path, 'atp_matches_', year, '.csv', sep = "")
  
  # -- Read in data
  full_data <- read.csv(full_data_path)
  
  
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
  
  message(paste('Year is: ', year, sep = ''))
  #print(paste('Year is: ', year, sep = ''))
  message(paste('Warning, number of incomplete entries is: ', sum(!complete.cases(atp_dat)), sep = '' ))
  
  #print(paste('Warning, number of incomplete entries is: ', sum(!complete.cases(atp_dat)), sep = '' ))
  
  atp_dat <- atp_dat %>%
    filter(complete.cases(atp_dat))
  
  message(paste('Keeping ', nrow(atp_dat), ' rows!', sep = ''))
  #print(paste('Keeping ', nrow(atp_dat), ' rows!', sep = ''))
  
  
  
  atp_dat_full <- atp_dat %>%
    
    mutate(
      # --> Pr(1st serve in)
      pr_1stin = s_1stIn/s_svpt,
      
      # --> Pr(2nd serve in)
      pr_2ndin = s_2ndIn/(s_2ndIn + s_df),
      
      # --> Pr(Win service point | 1st serve in)
      pr_w1_giv_1in = s_1stWon/s_1stIn,
      
      # --> Pr(Win service point | 2nd serve in)
      pr_w2_giv_2in = s_2ndWon/s_2ndIn,
      
      # --> Pr(win on 1st serve)
      pr_win_on_1st_serve = pr_w1_giv_1in * pr_1stin,
      
      # --> Pr(win on 2nd serve)
      pr_win_on_2nd_serve = s_2ndWon/ (s_svpt - s_1stIn),
      
      #-->Pr(win on serve)
      pr_win_on_serve = pr_1stin*pr_w1_giv_1in + ((1- pr_1stin)*pr_2ndin*pr_w2_giv_2in) ,
      
      # --> Pr(win w/ 2 first serves)
      
      pr_win_two_first_serves = pr_1stin*pr_w1_giv_1in + ((1- pr_1stin)* pr_1stin*pr_w1_giv_1in )
    )
  
  
  
  return(atp_dat_full)
  
  
}



# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # 
# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== #
# -- Collect WTA data

process_wta_match_data <- function(year, data_path){
  
  # Process WTA match data for a given year
  
  # Args:
  # _____
  # year [int]
  # data_path [str]: path to data
  
  # Returns:
  # _/_/_/_/
  # DataFrame
  
  # -- link to Sackmann's data
  full_data_path <- paste(data_path, 'wta_matches_', year, '.csv', sep = "")
  
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
  wta_dat <- rbind(winner_frame, loser_frame)  
  

  
  wta_dat_full <- wta_dat %>%
    
    mutate(
      # --> Pr(1st serve in)
      pr_1stin = s_1stIn/s_svpt,
      
      # --> Pr(2nd serve in)
      pr_2ndin = s_2ndIn/(s_2ndIn + s_df),
      
      # --> Pr(Win service point | 1st serve in)
      pr_w1_giv_1in = s_1stWon/s_1stIn,
      
      # --> Pr(Win service point | 2nd serve in)
      pr_w2_giv_2in = s_2ndWon/s_2ndIn,
      
      # --> Pr(win on 1st serve)
      pr_win_on_1st_serve = pr_w1_giv_1in * pr_1stin,
      
      # --> Pr(win on 2nd serve)
      pr_win_on_2nd_serve = s_2ndWon/ (s_svpt - s_1stIn),
      
      #-->Pr(win on serve)
      pr_win_on_serve = pr_1stin*pr_w1_giv_1in + ((1- pr_1stin)*pr_2ndin*pr_w2_giv_2in) ,
      
      # --> Pr(win w/ 2 first serves)
      
      pr_win_two_first_serves = pr_1stin*pr_w1_giv_1in + ((1- pr_1stin)* pr_1stin*pr_w1_giv_1in )
    )
  
  return(wta_dat_full)

  
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

get_end_of_year_rankings <- function(root_path, wta_flag = FALSE){
  
  # Function: For each year, get the end of year ranking
  # Raw data gives the weekly rankings for each year
  
  # Args:
  #-/-/-/
  # root_path [str]
  # wta_flag [boolean]
  
  # Returns
  #-/-/-/-/
  # DataFrame
  
  decade_list <- c('90s', '00s', '10s')
  
  datalist <- list()
  
  for(index in 1:length(decade_list)){
    decade <- decade_list[index]
    ranking_data_path <- paste(root_path, decade, '.csv',
                               sep = '')    
    if(wta_flag){
      ranking_data <- read.csv(ranking_data_path, header = FALSE)
      ranking_data <- ranking_data[, 1:4]
      colnames(ranking_data) <- c('ranking_date', 'rank', 'player', 'points')
      
    } else{
      
      ranking_data <- read.csv(ranking_data_path)
      
    }
    
    # Create Dataframe with end-of-year rankings
    ranking_end_of_year <- ranking_data %>%
      mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d"),
             year = year(ranking_date)) %>%
      group_by(year) %>%
      filter(ranking_date == max(ranking_date)) %>%
      filter(rank <= 500)
    
    datalist[[index]] <- ranking_end_of_year
    
  }
  
  entire_ranking_data <- do.call(rbind, datalist)
  
  return(entire_ranking_data)
  
}





