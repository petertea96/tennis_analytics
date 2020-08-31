#################################
library(dplyr)

collect_match_stats <- function(server_match_id, year,
                                tournament, server_name){
  
  # Given a match ID, carefully re-state player1 and player2 as simply server and returner
  # collect: serve speed, score prior to serve, set number, game number
  # outcome of serve (win/loss), 1st/2nd serve flag,
  
  # Args:
  # ----
  # server_match_id: str of match id
  
  
  wd = "/Users/petertea/Documents/Sports-Analytics/Sackmann/tennis_slam_pointbypoint"
  # Set to appropriate working directory
  setwd(wd)
  
  points_file_name <- paste(as.character(year), "-", as.character(tournament), "-points.csv", sep="")
  matches_file_name <- paste(as.character(year), "-", as.character(tournament), "-matches.csv", sep=""  )
  
  points_data <- read.csv(points_file_name)
  matches_data <- read.csv(matches_file_name)
  
  # Get info on who opponent is, and whether server is coded as player1 or player2
  server_matches_data <- matches_data %>%
    dplyr::filter(match_id == server_match_id)
  
  if(server_matches_data$player1 != server_name & server_matches_data$player2 != server_name){
    print('WARNING: PLAYER NOT FOUND...NEED TO CHECK THIS OUT!!!!!!')
    
  }
  
  player_1_or_2 = ifelse(server_matches_data$player1 == server_name,
                         1, 2)

  
  opponent_name = ifelse(player_1_or_2 == 1,
                         as.character(server_matches_data$player2), 
                         as.character(server_matches_data$player1))
  
  # Mould the data...
  # Keep only interesting variables
  player_points_data <- points_data %>%
    dplyr::filter(match_id == server_match_id) %>%
    dplyr::select(match_id, PointWinner, SetNo, GameNo, PointNumber,
                  PointServer, Speed_KMH, Speed_MPH,
                  P1DoubleFault, P2DoubleFault, 
                  ServeNumber, P1Score, P2Score, P1Ace, P2Ace)
  
  #french open does not code speed in kmh
  # convert mph --> kmh
  if (all(is.na(player_points_data$Speed_KMH))){
    player_points_data$Speed_KMH = player_points_data$Speed_MPH*1.60934
  }
  
  # Get scores (score prior to serve) relative to the server
  if(player_1_or_2 == 1){
    server_score <- player_points_data$P1Score
    returner_score <- player_points_data$P2Score
    current_score <- paste(player_points_data$P1Score, '-', player_points_data$P2Score, sep = ' ')
    
  } else{
    server_score = player_points_data$P2Score
    returner_score = player_points_data$P1Score
    current_score <- paste(player_points_data$P2Score, '-', player_points_data$P1Score, sep = ' ')
  }
  
  server_score <- server_score[c(length(server_score), 1:( length(server_score) -1 ) )]
  returner_score <- returner_score[c(length(returner_score), 1:( length(returner_score) -1 ) )]
  current_score <- current_score[c(length(current_score), 1:( length(current_score) -1 ) )]
  
  # Add score column
  player_points_data$current_score = current_score
  player_points_data$server_score = server_score
  player_points_data$returner_score = returner_score
  
  if(player_1_or_2 == 1){
    server_points_data <- player_points_data %>%
      dplyr::select(-P2DoubleFault, -P1Score, -P2Score, -P2Ace) %>%
      dplyr::filter(PointServer == 1) %>%
      dplyr::mutate(won_point = ifelse(PointWinner == 1, 1, 0),
                    returner = opponent_name) %>%
      rename(server_df = P1DoubleFault, server_ace = P1Ace)
  } else{
    server_points_data <- player_points_data %>%
      dplyr::select(-P1DoubleFault, -P1Score, -P2Score, -P1Ace) %>%
      dplyr::filter(PointServer == 2) %>%
      dplyr::mutate(won_point = ifelse(PointWinner == 2, 1, 0),
                    returner = opponent_name)%>%
      rename(server_df = P2DoubleFault, server_ace = P2Ace)
  }
  
  return(server_points_data)
  
}
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# zverev_match1_dat = collect_match_stats(server_match_id = "2018-ausopen-1148",
#                                        year = 2018,
#                                        tournament = 'ausopen',
#                                        server_name = 'A. Zverev')
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End test function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Collect data for ENTIRE tournament
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####

get_slam_serve <- function(Player, year, tournament){
  
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
    first_letter <- substr(Player, start = 1, stop =  1)
    short_name = paste(first_letter, '. ', last_name, sep = '')
    
    which_match_id <- which(matches_data$player1 == short_name | matches_data$player2 == short_name)
    
    Player <- short_name

  }
  
  match_ids <- as.character(matches_data[which_match_id,1])
  
  if( length(match_ids) == 0 ){
    return(NULL) 
  }
  
  complete_dat = list()
  for(i in 1:length(match_ids)){
    complete_dat[[i]] =  collect_match_stats(server_match_id = match_ids[i],
                        year=year,
                        tournament=tournament,
                        server_name=Player)
  }
  
  all_data = do.call(rbind, complete_dat)
  
  # Add year of match...
  all_data$year = as.numeric(substr(all_data$match_id,1,4))
  
  return(all_data)
  
}
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
# ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# zverev_match2_dat = get_slam_serve(Player =  'Alexander Zverev',
#                                    year = 2018,
#                                    tournament = 'frenchopen')


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End Data collection
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####

collect_all_grand_slam_data = function(player_name,
                                        possible_years = c(2018, 2019),
                                        grand_slam_list = c('ausopen', 'wimbledon', 'frenchopen', 'usopen')
                                        ){
  entire_dat = list()
  for(year in possible_years){
    
    for(tournament in grand_slam_list){
      
      grand_slam_data <- get_slam_serve(Player = player_name, 
                                        year = year, tournament = tournament)
      
      if(is.null(grand_slam_data)){
        print(paste('This data is empty:', tournament, year, sep = ' '))
        next
      }
      
      grand_slam_data$tournament = tournament
      
      gs_label <- paste(tournament, year, sep = '')
      entire_dat[[gs_label]]  <- grand_slam_data
      
    }
    
  }
  
  grand_slam_data_18_19 = do.call(rbind, entire_dat)
  
  # --> ServeNumber 0 actually represents a double fault (some coded at wimbledon
  # and usopen). Will code these as ServeNumber 2...
  
  grand_slam_data_18_19 <- grand_slam_data_18_19 %>%
    mutate(ServeNumber = ifelse(ServeNumber == 0,
                                2, ServeNumber))
  
  
  return(grand_slam_data_18_19)
  
}
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
# ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# zverev_match3_dat = collect_all_grand_slam_data(player_name =  'Alexander Zverev')





  
  
  

