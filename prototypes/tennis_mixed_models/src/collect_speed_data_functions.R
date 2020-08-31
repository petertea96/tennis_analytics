#################################
library(dplyr)

# --> Add Deuce/Ad. court side column
source("~/Documents/Github/serve_speeds/src/ad_or_deuce_court.R")


collect_match_stats <- function(server_match_id, year, tournament, server_name){
  # Given a match ID, carefully re-state player1 and player2 as simply server and returner
  # collect: serve speed, score prior to serve, set number, game number
  # outcome of serve (win/loss), 1st/2nd serve flag,
  
  
  wd = "/Users/petertea/Documents/Sports-Analytics/Sackmann/tennis_slam_pointbypoint/"
  # --> Directory to where data files are located
  
  points_file_name <- paste(wd, as.character(year), "-", as.character(tournament), "-points.csv", sep="")
  matches_file_name <- paste(wd, as.character(year), "-", as.character(tournament), "-matches.csv", sep=""  )
  
  points_data <- read.csv(points_file_name)
  matches_data <- read.csv(matches_file_name)
  
  # Get info on who opponent is, and whether server is coded as player1 or player2
  server_matches_data <- matches_data %>%
    dplyr::filter(match_id == server_match_id)
  
  player_1_or_2 = ifelse(server_matches_data$player1 == server_name, 1, 2)
  
  opponent_name = ifelse(server_matches_data$player1 == server_name,
                         as.character(server_matches_data$player2), 
                         as.character(server_matches_data$player1))
  
  # Mould the data...
  # Keep only interesting variables
  player_points_data <- points_data %>%
    dplyr::filter(match_id %in% server_match_id) %>%
    dplyr::select(match_id, PointWinner, SetNo, GameNo, PointNumber,
                  PointServer, Speed_KMH, Speed_MPH,
                  P1DoubleFault, P2DoubleFault, P1Ace, P2Ace,
                  ServeNumber, P1Score, P2Score,
                  RallyCount, P1DistanceRun, P2DistanceRun, 
                  ServeWidth, ServeDepth, ReturnDepth)
  
  n <- nrow(player_points_data)
  # Get scores (score prior to serve) relative to the server
  if(player_1_or_2 == 1){
    current_score <- paste(player_points_data$P1Score, '-', player_points_data$P2Score, sep = ' ')
    current_score <- current_score[c(n, 1:(n -1 ) )]
    server_score <- player_points_data$P1Score[c(n, 1:(n -1 ))]
    returner_score <- player_points_data$P2Score[c(n, 1:(n -1 ))]
  } else{
    current_score <- paste(player_points_data$P2Score, '-', player_points_data$P1Score, sep = ' ')
    current_score <- current_score[c(n, 1:( n -1 ) )]
    server_score <- player_points_data$P2Score[c(n, 1:(n -1 ))]
    returner_score <- player_points_data$P1Score[c(n, 1:(n -1 ))]
  }
  
  # Add score column
  player_points_data$current_score <- current_score
  player_points_data$server_score <- server_score
  player_points_data$returner_score <- returner_score 
  
  if(player_1_or_2 == 1){
    server_points_data <- player_points_data %>%
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
    server_points_data <- player_points_data %>%
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
  
  # --> Change coding of ServeWidth?
  #server_points_data <- server_points_data %>%
  #  mutate(ServeWidth = ifelse(ServeWidth == ))
  
  
  #--> Convert Frenchopen serve speeds to KMH
  #--> Convert ServeNumber '0' to 2
  server_points_data <- server_points_data %>%
    mutate(Speed_KMH = ifelse(is.na(Speed_KMH),
                              Speed_MPH*1.60934,
                              Speed_KMH),
           ServeNumber = ifelse(ServeNumber == 0, 2, ServeNumber))
  
  return(server_points_data)
  
}
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
#zverev_match1_dat = collect_match_stats(server_match_id = "2018-ausopen-1148",
#                                        year = 2018,
#                                        tournament = 'ausopen',
#                                        server_name = 'A. Zverev')

zverev_match1_dat = collect_match_stats(server_match_id = "2019-usopen-1149",
                                        year = 2019,
                                        tournament = 'usopen',
                                        server_name = 'Alexander Zverev')
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End test function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Collect data for ENTIRE tournament
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####

get_slam_data <- function(Player, year, tournament){
  
  # Read match data based on year and grand-slam tourney
  matches_file_name <- paste("/Users/petertea/Documents/Sports-Analytics/Sackmann/tennis_slam_pointbypoint/",
                             as.character(year),
                             "-",
                             as.character(tournament),
                             "-matches.csv", sep=""  )
  
  matches_data <- read.csv(matches_file_name)
  
  #Select matches with the Player involved
  which_match_id <- which(matches_data$player1 == Player | matches_data$player2 == Player)
  
  #Sometimes data coded as N. Djokovic
  if ( length(which_match_id) == 0) {
    
    last_name <- strsplit(Player, " ")[[1]][2]
    first_letter <- substr(Player,1,1)
    short_name <- paste(first_letter, '. ', last_name, sep = "")
    
    Player = short_name
    
    which_match_id <- which(matches_data$player1 == short_name | matches_data$player2 == short_name)
  }
  
  if( length(which_match_id) == 0){
    return(NULL)
  } else{
    
    match_ids <- as.character(matches_data[which_match_id,1])
    
    complete_dat = list()
    for(i in 1:length(match_ids)){
      complete_dat[[i]] =  collect_match_stats(server_match_id = match_ids[i],
                                               year=year,
                                               tournament=tournament,
                                               server_name=Player)
    }
    
    all_data = do.call(rbind, complete_dat)
    
    # Add year of match...
    all_data <- all_data %>%
      mutate(year = as.numeric(substr(match_id,1,4)),
             match_id = as.character(match_id),
             tournament = tournament)
    
    # Add grouped score
    all_data$grouped_score <- mapply(all_data$server_score,
                                     all_data$returner_score,
                                     FUN = get_grouped_score)
    
    # Add side of court served
    all_data$court_side <- mapply(all_data$server_score,
                                  all_data$returner_score,
                                  FUN = ad_or_deuce)
    
    return(all_data)
    
  }
  
  

  
}


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End Data collection
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
#zverev_match2_dat = get_slam_data(Player = 'Alexander Zverev',
#                                  year = 2018,
#                                  tournament = 'ausopen')
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End test function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 



##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Collect data for ENTIRE year
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####

collect_all_grand_slam_data <- function(player, year){
  
  tournament_list <- c('ausopen', 'frenchopen', 'wimbledon', 'usopen')
  
  complete_dat = list()
  for(tournament_ind in 1:(length(tournament_list))){
    complete_dat[[tournament_ind]] <- get_slam_data(Player = player,
                                                    year = year,
                                                    tournament = tournament_list[tournament_ind])
  }
  
  all_data = do.call(rbind, complete_dat)

  
  
  
  return(all_data)
  
}

##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
#zverev_match3_dat = collect_all_grand_slam_data(player = 'Alexander Zverev',
#                                  year = 2018)
#see=collect_all_grand_slam_data(player = 'Rafael Nadal',
#                  year = 2018)
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End test function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


### 
#Calculate proportion of success (wins) in 1st service games vs 2nd service games
serve_success_rates <- function(mydata){
  library(dplyr)
  total <- mydata %>%
    group_by(ServeNumber) %>%
    tally()
  
  wins <- mydata %>%
    group_by(ServeNumber) %>%
    summarise(num_wins =sum(PointWinner==PointServer))
  
  result <- total %>%
    left_join(wins, by = "ServeNumber") %>%
    mutate(win_percentage = num_wins / n)
  
  colnames(result) = c("Serve Number", "Service Games", "Service Wins", "Win Percentage")
  
  return(result)
}
