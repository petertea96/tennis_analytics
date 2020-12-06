#################################
library(dplyr)

source("/Users/petertea/tennis_analytics/prototypes/process_serve_location_pbp/ad_or_deuce.R")
source("/Users/petertea/tennis_analytics/prototypes/process_serve_location_pbp/categorise_score.R")
source("/Users/petertea/tennis_analytics/prototypes/process_serve_location_pbp/categorise_serve_location.R")


# server_match_id = "2019-wimbledon-1102"
# year = 2019
# tournament = 'wimbledon'
process_match_stats <- function(server_match_id, year,
                                tournament){
  
  # Given a match ID, carefully re-state player1 and player2 as simply server and returner
  # collect: serve speed, score prior to serve, set number, game number
  # outcome of serve (win/loss), 1st/2nd serve flag,
  
  # Args:
  # ----
  # server_match_id: str of match id
  
  wd = "/Users/petertea/Documents/tennis_data/tennis_slam_pointbypoint/"

  points_file_name <- paste(wd, as.character(year), "-", as.character(tournament), "-points.csv", sep="")
  matches_file_name <- paste(wd, as.character(year), "-", as.character(tournament), "-matches.csv", sep=""  )
  
  points_data <- read.csv(points_file_name)
  matches_data <- read.csv(matches_file_name)
  
  # Get info on who opponent is, and whether server is coded as player1 or player2
  server_matches_data <- matches_data %>%
    dplyr::filter(match_id == server_match_id)
  
  player_1_name <- as.character(server_matches_data$player1)
  player_2_name <- as.character(server_matches_data$player2)
  
  names_to_change <- c('Jo Wilfried Tsonga', 'Juan Martin Del Potro', 'Alex de Minaur',
                       'Albert Ramos Vinolas', 'Cristian Garin', 'Inigo Cervantes Huegun')
  new_names <- c('Jo-Wilfried Tsonga', 'Juan Martin del Potro', 'Alex De Minaur',
                 'Albert Ramos', 'Christian Garin', 'Inigo Cervantes')
  
  if(player_1_name %in% names_to_change){
    name_index <- player_1_name == names_to_change
    player_1_name <- new_names[name_index]
  }
  if(player_2_name %in% names_to_change){
    name_index <- player_2_name == names_to_change
    player_2_name <- new_names[name_index]
  }
  



  # Keep only interesting variables
  player_points_data <- points_data %>%
    dplyr::filter(match_id == server_match_id) %>%
    dplyr::filter(PointServer != 0) %>%
    dplyr::select(match_id, ElapsedTime, SetNo, GameNo, PointNumber,
                  P1Score, P2Score, ServeNumber,ServeWidth, ServeDepth,
                  PointWinner, PointServer, Speed_KMH, Speed_MPH,
                  P1DoubleFault, P2DoubleFault, P1Ace, P2Ace)
  
  player_points_data$serve_location <- mapply(player_points_data$ServeWidth,
                                              FUN = categorise_serve_location)
  
  
  if ( sum(is.na(player_points_data$serve_location)) == nrow(player_points_data) ){
    return(NULL)
  }
  
  #french open does not code speed in kmh
  # convert mph --> kmh
  if (all(is.na(player_points_data$Speed_KMH))){
    player_points_data$Speed_KMH = player_points_data$Speed_MPH*1.60934
  }
  
  # Get scores (score prior to serve) relative to the server
  player_points_data <- player_points_data %>%
    mutate(server_score = ifelse(PointServer == 1, as.character(P1Score),
                                 as.character(P2Score)),
           returner_score = ifelse(PointServer == 1, as.character(P2Score),
                                   as.character(P1Score)),
           current_score = paste(server_score, '-', returner_score, sep = ' ')
           )
  
  # Need to shift the score column down one
  num_points <- nrow(player_points_data)
  player_points_data[, c('server_score', 'returner_score', 'current_score')] <-  player_points_data[c(num_points, 1:( num_points -1 ) ), c('server_score', 'returner_score', 'current_score')]

  # Define Ace/ Double fault flag in terms of the server
  player_points_data <- player_points_data %>%
    mutate(server_ace = ifelse(PointServer == 1, P1Ace, P2Ace),
           server_df = ifelse(PointServer == 1, P1DoubleFault, P2DoubleFault)
    )
  
  # Add whether server won the point
  player_points_data <- player_points_data %>%
    mutate(server_won = ifelse(PointServer == PointWinner, 1, 0)
    )
  
  # Add server name and returner name
  player_points_data <- player_points_data %>%
    mutate(server_name = ifelse(PointServer==1, player_1_name, player_2_name),
           returner_name = ifelse(PointServer==1, player_2_name, player_1_name))
  
  # Convert ServeNumber 0 to 2 (0 represents a double fault)
  player_points_data <- player_points_data %>%
    mutate(ServeNumber = ifelse(ServeNumber==0, 2, ServeNumber))
  
  # Remove unnecessary columns
  player_points_data <- player_points_data %>%
    dplyr::select(-P1DoubleFault, -P2DoubleFault, -P1Ace, -P2Ace,
                  -P1Score, -P2Score, Speed_MPH)
  
  # Add Adv. / Deuce serve side
  player_points_data$court_side <- mapply(player_points_data$server_score,
                                          player_points_data$returner_score,
                                          FUN = ad_or_deuce)
  player_points_data$server_pressure <- mapply(player_points_data$server_score,
                                        player_points_data$returner_score,
                                        FUN = categorise_score)
  
  # Add previous serve location
  # Add previous serve location x2
  # --> Convert PointNumber to numeric
  # player_points_data <- player_points_data %>%
  #   mutate(PointNumber = as.numeric(levels(PointNumber))[PointNumber]) %>% 
  #   arrange(server_name, PointNumber) 
  player_points_data <- player_points_data %>%
    mutate(PointNumber = as.numeric(paste(PointNumber))) %>% 
    arrange(factor(server_name, levels = c(player_1_name, player_2_name)), PointNumber) 
  
  player1_last_serve_index <- which.min(player_points_data$server_name == player_1_name) - 1
  player2_last_serve_index <- nrow(player_points_data) - player1_last_serve_index
  
  player1_last_serve <- c(NA, player_points_data[1:player1_last_serve_index, 'serve_location'][-player1_last_serve_index])
  player1_last2_serve <- c(NA, NA, player_points_data[1:player1_last_serve_index, 'serve_location'][-c(player1_last_serve_index, player1_last_serve_index-1)])
  
  player2_last_serve <- c(NA, player_points_data[(player1_last_serve_index+1):nrow(player_points_data), 'serve_location'][-player2_last_serve_index])
  player2_last2_serve <- c(NA, NA, player_points_data[(player1_last_serve_index+1):nrow(player_points_data), 'serve_location'][-c(player2_last_serve_index, player2_last_serve_index-1)])
  
  player_points_data <- player_points_data %>%
    mutate(player_last_serve_loc = c(player1_last_serve,player2_last_serve),
           player_last_serve2_loc = c(player1_last2_serve, player2_last2_serve)
           )
  

  return(player_points_data)
  
}
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
test1 = process_match_stats(server_match_id = "2019-wimbledon-1106",
                            year = 2019,
                            tournament = 'wimbledon')
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End test function
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Collect data for ENTIRE tournament
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####

get_slam_serve <- function(year, tournament, atp_flag = TRUE){
  
  wd = "/Users/petertea/Documents/tennis_data/tennis_slam_pointbypoint/"
  
  # Read match data based on year and grand-slam tourney
  matches_file_name <- paste(wd, as.character(year), "-", as.character(tournament), "-matches.csv", sep=""  )
  matches_data <- read.csv(matches_file_name)
  
  #Select match IDs
  match_ids <- as.character(matches_data$match_id)
  
  if(atp_flag){
    tot_str_length <- nchar(match_ids)
    match_ids <- match_ids[which(as.integer(substr(match_ids, tot_str_length-3,tot_str_length)) < 2000 )]
  }
  
  if( length(match_ids) == 0 ){
    return(NULL) 
  }
  
  complete_dat = list()
  for(i in 1:length(match_ids)){
    print(match_ids[i])
    complete_dat[[i]] =  process_match_stats(server_match_id = match_ids[i],
                                             year=year,
                                             tournament=tournament)
  }
  
  all_data = do.call(rbind, complete_dat)
  
  # Add year of match...
  all_data$year = as.numeric(substr(all_data$match_id,1,4))
  
  return(all_data)
  
}
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
# ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
test2 = get_slam_serve(year = 2019,tournament = 'wimbledon')

test2[!complete.cases(test2),] %>% View()

look_at_data <- test2[complete.cases(test2),]

summarise_data <- look_at_data %>%
  group_by(server_name, court_side, server_pressure, serve_location) %>%
  summarise(num = n())

##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# End Data collection
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####

collect_all_grand_slam_data = function(possible_years = c(2019),
                                       grand_slam_list = c('wimbledon','usopen')){
  entire_dat = list()
  for(year in possible_years){
    
    for(tournament in grand_slam_list){
      
      grand_slam_data <- get_slam_serve(year = year, tournament = tournament)
      
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
  
  
  
  return(grand_slam_data_18_19)
  
}
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
# ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# test3 = collect_all_grand_slam_data(possible_years = c(2019),
#                                     grand_slam_list = c('wimbledon','usopen'))
# 
# 
# 
# test3[!complete.cases(test3),] %>% View()
# 
# look_at_data <- test3[complete.cases(test3),]
# 
# summarise_data <- look_at_data %>%
#   group_by(server_name, court_side, server_pressure, serve_location) %>%
#   summarise(num = n())


recent_slam_data <- collect_all_grand_slam_data(possible_years = c(2016:2019),
                                                grand_slam_list = c('wimbledon','usopen'))


getwd()
saveRDS(recent_slam_data, 
        file = "/Users/petertea/tennis_analytics/prototypes/process_serve_location_pbp/processed_slam_pointbypoint.rds")


# -- See if we have everyone's height/handedness

player_names <- recent_slam_data %>%
  filter(complete.cases(.)) %>%
  group_by(server_name) %>%
  summarise(num_serves = n()) %>%
  arrange(desc(num_serves))


# -- Add player height data
player_height_data <- read.csv('/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/official_atp_height_2020.csv')

# Some names to change (player_names --> height):
# Jo Wilfried Tsonga --> Jo-Wilfried Tsonga
# Juan Martin Del Potro --> Juan Martin del Potro
# Alex de Minaur --> Alex De Minaur
# Albert Ramos Vinolas --> Albert Ramos
# Alexander Ward --> (added manually)
# Brian Baker --> (added manually)
# Matthew Barton --> (added manually)
# Cristian Garin --> Christian Garin
# Inigo Cervantes Huegun --> Inigo Cervantes
# Nicolas Kicker --> (added manually)
# Marcus Willis--> (added manually)


test <- player_names  %>%
  left_join(player_height_data %>% dplyr::select(player_name, player_handedness, player_height_cm),
            by = c('server_name' = 'player_name'))



