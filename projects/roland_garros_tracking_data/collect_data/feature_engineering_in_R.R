### -- After scraping and preliminary data processing steps in Python,
### -- now it's time to apply some feature extraction steps in R.

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
###             Check list of engineered features            --------
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

## * Add player handedness
## * Add point importance
## * Remove matches with insufficient amount of tracking points
## * Impute targeted serve direction for net faults
## * Figure out why splitting data into right/left side still leaves
##     some highly negative x coordinate values.
## * Repeat for Australian Open Data


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

load(file = "/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/src/importance.RData")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/src/importance.R")

atp_rolandgarros_pbp_df <- read.csv('./collect_data/data/atp_roland_garros_19_20.csv')
wta_rolandgarros_pbp_df <- read.csv('./collect_data/data/wta_roland_garros_19_20.csv')

playerid_df <- read.csv('collect_data/data/roland_garros_player_id.csv',
                        stringsAsFactors = FALSE)
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
### --              Add Player Handedness                       -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
playerid_df$id <- as.integer(playerid_df$id)

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Select variables we care about, and mutate variable types -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

atp_rolandgarros_training_data <- atp_rolandgarros_pbp_df %>%
  select(point_ID, set_num, game_num, point_num, serve_num,
         server_id, returner_id, point_winner_id, 
         court_side, serve_speed_kph, fault_distance_missed_m,
         x_ball_at_serve, y_ball_at_serve, z_ball_at_serve,
         rally_length, point_end_type, error_type,
         trapped_by_net,
         is_break_point, is_break_point_converted, is_track_avail,
         serveBounceCordinate_x, serveBounceCordinate_y,
         serve_dir, z_net_serve, is_fault, is_doublefault, 
         is_prev_doublefault, is_ace, is_prev_ace,
         is_tiebreak, server_score, returner_score, 
         player1, player2, p1_cum_games, p2_cum_games, 
         p1_cum_sets, p2_cum_sets, match_id, year, 
         cruciality
  ) %>%
  mutate(
    server_dist_from_center = abs(y_ball_at_serve),
    is_break_point = ifelse(is_break_point == 'True',
                            1, 0),
    is_break_point_converted = ifelse(is_break_point_converted == 'True',
                                      1, 0),
    is_track_avail = ifelse(is_track_avail == 'True',
                            TRUE, FALSE),
    is_tiebreak = ifelse(is_tiebreak == 1,
                         TRUE, FALSE),
    
    serve_speed_kph = as.numeric(gsub("([0-9]+).*$", "\\1", serve_speed_kph)),
    which_side = ifelse(x_ball_at_serve > 0, 'right', 'left'),
    fault_distance_missed_m = ifelse(is.na(fault_distance_missed_m),
                                     '0.0 Metre', fault_distance_missed_m),
    error_type = ifelse(is.na(error_type),
                        'None', error_type)
    
    
  ) %>%
  # -- Add player names
  left_join(playerid_df, by = c('server_id' = 'id')) %>%
  rename(server_name = name,
         server_hand = player_handedness) %>%
  left_join(playerid_df, by = c('returner_id' = 'id')) %>%
  rename(returner_name = name,
         returner_hand = player_handedness) %>%
  
  # -- Add player names again denoting player1 or player2 (confusing, I know)
  left_join(playerid_df, by = c('player1' = 'id')) %>%
  rename(p1_name = name,
         p1_hand = player_handedness) %>%
  left_join(playerid_df, by = c('player2' = 'id')) %>%
  rename(p2_name = name,
         p2_hand = player_handedness) %>%
  
  # -- Configure p1 & p2 cumulative games/sets into server & returner
  mutate(
    s_cum_games = ifelse(server_name == p1_name,
                         p1_cum_games, p2_cum_games),
    r_cum_games = ifelse(returner_name == p1_name,
                         p1_cum_games, p2_cum_games),
    s_cum_sets = ifelse(server_name == p1_name,
                        p1_cum_sets, p2_cum_sets),
    r_cum_sets = ifelse(returner_name == p1_name,
                        p1_cum_sets, p2_cum_sets)
  )


# -- Do the same for WTA
wta_rolandgarros_training_data <- wta_rolandgarros_pbp_df %>%
  select(point_ID, set_num, game_num, point_num, serve_num,
         server_id, returner_id, point_winner_id, 
         court_side, serve_speed_kph, fault_distance_missed_m,
         x_ball_at_serve, y_ball_at_serve, z_ball_at_serve,
         rally_length, point_end_type, error_type,
         trapped_by_net,
         is_break_point, is_break_point_converted, is_track_avail,
         serveBounceCordinate_x, serveBounceCordinate_y,
         serve_dir, z_net_serve, is_fault, is_doublefault, 
         is_prev_doublefault, is_ace, is_prev_ace,
         is_tiebreak, server_score, returner_score, 
         player1, player2, p1_cum_games, p2_cum_games, 
         p1_cum_sets, p2_cum_sets, match_id, year, 
         cruciality
  ) %>%
  mutate(
    server_dist_from_center = abs(y_ball_at_serve),
    is_break_point = ifelse(is_break_point == 'True',
                            1, 0),
    is_break_point_converted = ifelse(is_break_point_converted == 'True',
                                      1, 0),
    is_track_avail = ifelse(is_track_avail == 'True',
                            TRUE, FALSE),
    is_tiebreak = ifelse(is_tiebreak == 1,
                         TRUE, FALSE),
    
    serve_speed_kph = as.numeric(gsub("([0-9]+).*$", "\\1", serve_speed_kph)),
    which_side = ifelse(x_ball_at_serve > 0, 'right', 'left'),
    fault_distance_missed_m = ifelse(is.na(fault_distance_missed_m),
                                     '0.0 Metre', fault_distance_missed_m),
    error_type = ifelse(is.na(error_type),
                        'None', error_type)
    
  ) %>%
  # -- Add player names
  left_join(playerid_df, by = c('server_id' = 'id')) %>%
  rename(server_name = name,
         server_hand = player_handedness) %>%
  left_join(playerid_df, by = c('returner_id' = 'id')) %>%
  rename(returner_name = name,
         returner_hand = player_handedness) %>%
  
  # -- Add player names again denoting player1 or player2 (confusing, I know)
  left_join(playerid_df, by = c('player1' = 'id')) %>%
  rename(p1_name = name,
         p1_hand = player_handedness) %>%
  left_join(playerid_df, by = c('player2' = 'id')) %>%
  rename(p2_name = name,
         p2_hand = player_handedness) %>%
  
  # -- Configure p1 & p2 cumulative games/sets into server & returner
  mutate(
    s_cum_games = ifelse(server_name == p1_name,
                         p1_cum_games, p2_cum_games),
    r_cum_games = ifelse(returner_name == p1_name,
                         p1_cum_games, p2_cum_games),
    s_cum_sets = ifelse(server_name == p1_name,
                        p1_cum_sets, p2_cum_sets),
    r_cum_sets = ifelse(returner_name == p1_name,
                        p1_cum_sets, p2_cum_sets)
  )


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
### --             Apply Point Importance                       -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Start with ATP
# ***************************
# -- Point Importance Functions accepts deuce scores as 3 - 3 
# So for example, we can't have an input score of 4 - 4

atp_s_score <- vector()
atp_r_score <- vector()

for(i in 1:nrow(atp_rolandgarros_training_data)){
  is_tiebreak = atp_rolandgarros_training_data[i, 'is_tiebreak']
  server_score = atp_rolandgarros_training_data[i, 'server_score']
  returner_score = atp_rolandgarros_training_data[i, 'returner_score']
  
  if(is_tiebreak) {
    if((server_score >= 7) & (returner_score >=7)){
      if( (server_score - returner_score) == -1 ){
        atp_s_score[i] <- 6
        atp_r_score[i] <- 7}
      else if( server_score == returner_score ){
        atp_s_score[i] <- 6
        atp_r_score[i] <- 6}
      else if((server_score - returner_score) == 1){
        atp_s_score[i] <- 7
        atp_r_score[i] <- 6}
      # else{
      #   atp_s_score[i] <- NA
      #   atp_r_score[i] <- NA}
    }
    else{
      atp_s_score[i] <- server_score
      atp_r_score[i] <- returner_score
      
    }
  }
  else{
    if(((server_score >= 4) & (returner_score >=4))){
      if( (server_score - returner_score) == -1 ){
        atp_s_score[i] <- 3
        atp_r_score[i] <- 4}
      else if( server_score == returner_score ){
        atp_s_score[i] <- 3
        atp_r_score[i] <- 3}
      else if((server_score - returner_score) == 1){
        atp_s_score[i] <- 4
        atp_r_score[i] <- 3}
      else{
        atp_s_score[i] <- NA
        atp_r_score[i] <- NA}}
    else{
      atp_s_score[i] <- server_score
      atp_r_score[i] <- returner_score}
  }
}

atp_rolandgarros_training_data$s_score <- atp_s_score
atp_rolandgarros_training_data$r_score <- atp_r_score

# Now, add point importance
atp_rolandgarros_training_data_with_importance <-
  atp_rolandgarros_training_data %>%
  #filter(complete.cases(.)) %>%
  #Can't add 5th set tiebreak in roland garros
  filter(!is.na(s_cum_games)) %>%
  rowwise() %>%
  mutate(
    point_importance = importance(point_x = s_score, 
                                  point_y = r_score,
                                  game_x = s_cum_games, 
                                  game_y = r_cum_games, 
                                  set_x = s_cum_sets, 
                                  set_y = r_cum_sets,
                                  tiebreak = is_tiebreak, 
                                  bestof3 = FALSE)
  )



# -- Now repeat for WTA
wta_s_score <- vector()
wta_r_score <- vector()

for(i in 1:nrow(wta_rolandgarros_training_data)){
  is_tiebreak = wta_rolandgarros_training_data[i, 'is_tiebreak']
  server_score = wta_rolandgarros_training_data[i, 'server_score']
  returner_score = wta_rolandgarros_training_data[i, 'returner_score']
  
  if(is_tiebreak) {
    if((server_score >= 7) & (returner_score >=7)){
      if( (server_score - returner_score) == -1 ){
        wta_s_score[i] <- 6
        wta_r_score[i] <- 7}
      else if( server_score == returner_score ){
        wta_s_score[i] <- 6
        wta_r_score[i] <- 6}
      else if((server_score - returner_score) == 1){
        wta_s_score[i] <- 7
        wta_r_score[i] <- 6}
      # else{
      #   wta_s_score[i] <- NA
      #   wta_r_score[i] <- NA}
    }
    else{
      wta_s_score[i] <- server_score
      wta_r_score[i] <- returner_score
    }
  }
  else{
    if(((server_score >= 4) & (returner_score >=4))){
      if( (server_score - returner_score) == -1 ){
        wta_s_score[i] <- 3
        wta_r_score[i] <- 4}
      else if( server_score == returner_score ){
        wta_s_score[i] <- 3
        wta_r_score[i] <- 3}
      else if((server_score - returner_score) == 1){
        wta_s_score[i] <- 4
        wta_r_score[i] <- 3}
      else{
        wta_s_score[i] <- NA
        wta_r_score[i] <- NA}}
    else{
      wta_s_score[i] <- server_score
      wta_r_score[i] <- returner_score}
  }
}

wta_rolandgarros_training_data$s_score <- wta_s_score
wta_rolandgarros_training_data$r_score <- wta_r_score

# Now, add point importance
wta_rolandgarros_training_data_with_importance <-
  wta_rolandgarros_training_data %>%
  #filter(complete.cases(.)) %>%
  #Can't add 5th set tiebreak in roland garros
  filter(!is.na(s_cum_games)) %>%
  rowwise() %>%
  mutate(
    point_importance = importance(point_x = s_score, 
                                  point_y = r_score,
                                  game_x = s_cum_games, 
                                  game_y = r_cum_games, 
                                  set_x = s_cum_sets, 
                                  set_y = r_cum_sets,
                                  tiebreak = is_tiebreak, 
                                  bestof3 = FALSE)
  )

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Remove matches that don't include atleast 70% of the tracking information -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
cutoff_tracking_available <- 0.7

atp_ineligible_match_ids <- atp_rolandgarros_training_data_with_importance %>%
  group_by(match_id) %>% 
  summarise(tot_serves = n(),
            pts_avail = sum(is_track_avail),
            percent_pts_available = pts_avail / tot_serves) %>%
  filter(percent_pts_available < cutoff_tracking_available) %>%
  .$match_id


atp_rolandgarros_training_data_with_importance <- atp_rolandgarros_training_data_with_importance %>%
  filter( !(match_id %in% atp_ineligible_match_ids) )

write.csv(atp_rolandgarros_training_data_with_importance,
          'atp_processed_roland_garros_tracking_data.csv',
          row.names = FALSE)


# -- Finish with WTA
wta_ineligible_match_ids <- wta_rolandgarros_training_data_with_importance %>%
  group_by(match_id) %>% 
  summarise(tot_serves = n(),
            pts_avail = sum(is_track_avail),
            percent_pts_available = pts_avail / tot_serves) %>%
  filter(percent_pts_available < cutoff_tracking_available) %>%
  .$match_id

wta_rolandgarros_training_data_with_importance <- wta_rolandgarros_training_data_with_importance %>%
  filter( !(match_id %in% wta_ineligible_match_ids) )

write.csv(wta_rolandgarros_training_data_with_importance,
          'wta_processed_roland_garros_tracking_data.csv',
          row.names = FALSE)


