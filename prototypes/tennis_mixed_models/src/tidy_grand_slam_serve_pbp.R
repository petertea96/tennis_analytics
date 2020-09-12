# -- Date: Sep 6th, 2020
# -- Purpose: Tidy Sackmann's grand slam pbp
# -- DataFrame for a single player with the following columns:
# -- Tournament info: Year, Surface
# -- Opponent Name
# -- Adv. / Deuce court, Serve speed, Serve number flag, Serve Location, Set No., Game No.

# || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- ||
# -- Load libraries
library(dplyr)

# -- Source own functions
# --> Add Deuce/Ad. court side column
source("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/src/ad_or_deuce.R")
source("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/src/classify_pressure_score.R")
# || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- ||

get_tidy_serve_pbp_data <- function(server_match_id, 
                                    year, 
                                    tournament, 
                                    server_name,
                                    data_path = "/Users/petertea/Documents/tennis_data/tennis_slam_pointbypoint/"){
  
  # Function Purpose:
  # ----------------
  # Given a match ID and a server, carefully determine whether the server is coded as 'player 1' or 
  # 'player 2'. Then, collect columns for their recorded serve speed, score prior to serve, set number, game number
  # outcome of serve (win/loss), 1st/2nd serve flag,
  
  # Args:
  # -----
  # server_match_id [str]:   
  # year [int]: tournament year
  # tournament [str]: tournament name
  # server_name [str]: name of server
  # data_path [str]: directory path to data
  
  # Returns:
  # --------
  # DataFrame


  # -- Read in match pbp data + meta-data
  pbp_file_name <- paste(data_path, 
                         as.character(year),
                         "-", 
                         as.character(tournament), 
                         "-points.csv", 
                         sep="")
  
  metadata_file_name <- paste(data_path, 
                              as.character(year), 
                              "-", 
                              as.character(tournament), 
                              "-matches.csv", 
                              sep=""  )
  
  all_pbp_data <- read.csv(pbp_file_name)
  meta_data <- read.csv(metadata_file_name)
  
  # -- Get row of the specified match ID.
  # Use this row to get the opponent name, and whether server is coded as 'player1' or
  # as 'player2'
  server_match_meta_data <- meta_data %>%
    dplyr::filter(match_id == server_match_id)
  
  server_is_player_1_or_2 <- ifelse(server_match_meta_data$player1 == server_name, 1, 2)
  
  opponent_name = ifelse(server_match_meta_data$player1 == server_name,
                         as.character(server_match_meta_data$player2), 
                         as.character(server_match_meta_data$player1))
  
  
  # -- Filter the pbp data to only include match ID
  # Select variables of interest
  match_pbp_data <- all_pbp_data %>%
    dplyr::filter(match_id %in% server_match_id) %>%
    dplyr::select(match_id, PointWinner, SetNo, GameNo, PointNumber,
                  PointServer, Speed_KMH, Speed_MPH,
                  P1DoubleFault, P2DoubleFault, P1Ace, P2Ace,
                  ServeNumber, P1Score, P2Score,
                  RallyCount, P1DistanceRun, P2DistanceRun, 
                  ServeWidth, ServeDepth, ReturnDepth)
  
  # -- Number of serves for both players in this match ID
  tot_num_serves <- nrow(match_pbp_data)
  
  # -- Get scores (score at beginning of serve point) relative to the server
  if(server_is_player_1_or_2 == 1){
     current_score <- paste(match_pbp_data$P1Score, '-', match_pbp_data$P2Score, sep = ' ')
     
     # Nudge score 
     current_score <- current_score[c(tot_num_serves, 1:(tot_num_serves -1 ) )]
     
     server_score <- match_pbp_data$P1Score[c(tot_num_serves, 1:(tot_num_serves -1 ))]
     returner_score <- match_pbp_data$P2Score[c(tot_num_serves, 1:(tot_num_serves -1 ))]
     
  } else{
    current_score <- paste(match_pbp_data$P2Score, '-', match_pbp_data$P1Score, sep = ' ')
    
    # Nudge score
    current_score <- current_score[c(tot_num_serves, 1:( tot_num_serves -1 ) )]
    
    server_score <- match_pbp_data$P2Score[c(tot_num_serves, 1:(tot_num_serves -1 ))]
    returner_score <- match_pbp_data$P1Score[c(tot_num_serves, 1:(tot_num_serves -1 ))]
  }
  
  # -- Add score column to DataFrame
  match_pbp_data$current_score <- current_score
  match_pbp_data$server_score <- server_score
  match_pbp_data$returner_score <- returner_score 
  
  # -- Filter DataFrame to include only the server's service points
  if(server_is_player_1_or_2 == 1){
    server_pbp_data <- match_pbp_data %>%
      dplyr::select(-P2DoubleFault, -P1Score, -P2Score, -P2Ace,
                   ) %>%
      dplyr::filter(PointServer == 1) %>%
      dplyr::mutate(won_point = ifelse(PointWinner == 1, 1, 0),
                    returner = opponent_name) %>%
      rename(server_df = P1DoubleFault,
             server_ace = P1Ace,
             server_distance_run = P1DistanceRun,
             returner_distance_run = P2DistanceRun)
  } else{
    server_pbp_data <- match_pbp_data %>%
      dplyr::select(-P1DoubleFault, -P1Score, -P2Score, -P1Ace,
                   ) %>%
      dplyr::filter(PointServer == 2) %>%
      dplyr::mutate(won_point = ifelse(PointWinner == 2, 1, 0),
                    returner = opponent_name)%>%
      rename(server_df = P2DoubleFault, server_ace = P2Ace,
             server_distance_run = P2DistanceRun,
             returner_distance_run = P1DistanceRun 
             )
  }
  

  
  # Some grand slams don''t have serve speed recorded in KMH (ex: frenchopen)
  #--> Convert Frenchopen serve speeds to KMH
  #--> Convert ServeNumber '0' to 2
  server_pbp_data <- server_pbp_data %>%
    mutate(Speed_KMH = ifelse(is.na(Speed_KMH),
                              Speed_MPH*1.60934,
                              Speed_KMH),
           ServeNumber = ifelse(ServeNumber == 0, 2, ServeNumber))
  
  return(server_pbp_data)
  
}
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 

# zverev_match1_dat <- get_tidy_serve_pbp_data(server_match_id = "2019-usopen-1149",
#                                              year = 2019,
#                                              tournament = 'usopen',
#                                              server_name = 'Alexander Zverev')

##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End test function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Collect data for ENTIRE tournament
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####

get_tidy_slam_pbp_data <- function(server_name, 
                                   year, 
                                   tournament,
                                   data_path = "/Users/petertea/Documents/tennis_data/tennis_slam_pointbypoint/"){
  
  # Function Purpose:
  # ----------------
  # Given a slam & year, collect all pbp data for a particular player
  
  # Args:
  # -----
  # server_name [str]:   
  # year [int]: tournament year
  # tournament [str]: tournament name

  # Returns:
  # --------
  # Data Frame
  
  
  # Read match data based on year and grand-slam tourney
  metadata_file_name <- paste(data_path, 
                              as.character(year), 
                              "-", 
                              as.character(tournament), 
                              "-matches.csv", 
                              sep=""  )
  meta_data <- read.csv(metadata_file_name)
  
  # Select matches with the Player involved
  which_match_id <- which(meta_data$player1 == server_name | meta_data$player2 == server_name)
  
  # --Sometimes data coded with an abbreviated name
  # Ex: 'N. Djokovic' instead of 'Novak Djokovic'
  if ( length(which_match_id) == 0) {
    
    last_name <- strsplit(server_name, " ")[[1]][2]
    first_letter <- substr(server_name,1,1)
    abbreviated_name <- paste(first_letter, '. ', last_name, sep = "")
    
    which_match_id <- which(meta_data$player1 == abbreviated_name | meta_data$player2 == abbreviated_name)
    
    # -- Reassign server name to abbreviated name (will be recycled in get_tidy_serve_pbp_data() call)
    server_name <- abbreviated_name
  }
  
  if( length(which_match_id) == 0){
    return(NULL)
  } else{
    
    # -- All match IDs server played in
    match_ids <- as.character(meta_data[which_match_id,1])
    
    # -- For each match ID, fill in the list
    complete_grand_slam_list = list()
    for(i in 1:length(match_ids)){
      complete_grand_slam_list[[i]] = get_tidy_serve_pbp_data(server_match_id = match_ids[i],
                                                              year = year,
                                                              tournament = tournament,
                                                              server_name = server_name)
    }
    
    complete_grand_slam_dataframe <- do.call(rbind, complete_grand_slam_list)
    
    # -- Add year, tournament, match ID as columns
    complete_grand_slam_dataframe <- complete_grand_slam_dataframe %>%
      mutate(year = year,
             match_id = as.character(match_id),
             tournament = tournament)
    
    # -- Add grouped score
    complete_grand_slam_dataframe$pressure_score <- mapply(complete_grand_slam_dataframe$server_score,
                                                           complete_grand_slam_dataframe$returner_score,
                                                          FUN = classify_pressure_score)
    
    # Add side of court served
    complete_grand_slam_dataframe$court_side <- mapply(complete_grand_slam_dataframe$server_score,
                                                       complete_grand_slam_dataframe$returner_score,
                                                       FUN = ad_or_deuce)
    
    return(complete_grand_slam_dataframe)
    
  }
  
  

  
}


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End Data collection
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# zverev_test_dat_2 <- get_tidy_slam_pbp_data(server_name = 'Alexander Zverev',
#                                             year = 2018,
#                                             tournament = 'ausopen')
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End test function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 



##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Collect data for ENTIRE year
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####

collect_all_grand_slam_data <- function(server_name, year, tournament_vector){
  
  # tournament_vector <- c('ausopen', 'frenchopen', 'wimbledon', 'usopen')
  # -- Only US Open & Wimbledon provide serve location?
  complete_dat = list()
  for(tournament_ind in 1:(length(tournament_vector))){
    complete_dat[[tournament_ind]] <- get_tidy_slam_pbp_data(server_name = server_name,
                                                             year = year,
                                                             tournament = tournament_vector[tournament_ind])
  }
  
  all_data = do.call(rbind, complete_dat)

  
  
  
  return(all_data)
  
}

##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# zverev_match3_dat = collect_all_grand_slam_data(server_name = 'Alexander Zverev',
#                                                 year = 2018)
# see=collect_all_grand_slam_data(server_name = 'Rafael Nadal',
#                                 year = 2018)
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End test function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


