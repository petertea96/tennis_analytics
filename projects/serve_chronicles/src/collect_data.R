# ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #
# Access Sackmann data, and save relevant information in
# a .csv file
# This is a data cleaning script.
# ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #
setwd("C:/Users/peter.tea/projects/tennis/")

#--> Load libraries
library(dplyr)
library(lubridate)
 

save_atp_match_data <- function(year, save_path){
  
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
  
  print(paste('Year is: ', year, sep = ''))
  print(paste('Warning, number of incomplete entries is: ', sum(!complete.cases(atp_dat)), sep = '' ))
                          
  atp_dat <- atp_dat %>%
    filter(complete.cases(atp_dat))
  
  print(paste('Keeping ', nrow(atp_dat), ' rows!', sep = ''))
  
  
  
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
  
  
  
  filename = paste(save_path, 'atp_', year, '.csv', sep = '')
  
  write.csv(x = atp_dat_full, row.names = FALSE,
            file = filename)
  
  
}

# --- Save ATP DAta

saving_path <- "C:/Users/peter.tea/projects/tennis/data/atp_data/"
save_atp_match_data(year = 2019,save_path = saving_path)


for(year in 1991:2019){
  save_atp_match_data(year = year,save_path = saving_path)
}

# -- DONE


# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # 
# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== #
# -- Collect WTA data

save_wta_match_data <- function(year, save_path){
  
  # -- link to Sackmann's data
  wta_root <- 'https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_'
  data_path <- paste(wta_root, year, '.csv', sep = "")
  
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
  
  print(paste('Year is: ', year, sep = ''))
  print(paste('Warning, number of incomplete entries is: ', sum(!complete.cases(wta_dat)), sep = '' ))
  
  #wta_dat %>%
  #  filter(!complete.cases(wta_dat)) %>%
  #  View()
  
  wta_dat <- wta_dat %>%
    filter(complete.cases(wta_dat))
  
  print(paste('Keeping ', nrow(wta_dat), ' rows!', sep = ''))
  
  
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
  
  
  
  filename = paste(save_path, 'wta_', year, '.csv', sep = '')
  
  write.csv(x = wta_dat_full, row.names = FALSE,
            file = filename)
  
  
}


# --- Save WTA DAta

saving_path <- "C:/Users/peter.tea/projects/tennis/data/wta_data/"
save_wta_match_data(year = 2003,save_path = saving_path)


for(year in 2003:2019){
  save_wta_match_data(year = year,save_path = saving_path)
}

# -- DONE

