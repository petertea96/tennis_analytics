### -- After scraping and preliminary data processing steps in Python,
### -- now it's time to apply some feature extraction steps in R.

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
###             Check list of engineered features            --------
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

## [*] Add player handedness
## [*] Add point importance
## [*] Remove matches with insufficient amount of tracking points
## [*] Impute targeted serve direction for net faults
## [*] Re-categorise the 'intended' serve directions
## [*] Add previous serve location (lag 1 and lag 2)
## [*] Add scaled point importance within a match (min-max normalization and z normalization)
## [*] Serve direction targets backhand
## [*] Transform ball toss and ball lateral position (ad vs. deuce & left vs right courts)
##      x --> Distance inside the court
##      y --> lateral position
## [ ] Figure out why splitting data into right/left side still leaves
##     some highly negative x coordinate values.
## [ ] Repeat for Australian Open Data


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_project/")

load(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project//src/importance.RData")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/importance.R")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/impute_serve_location.R")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/categorise_serve_direction.R")
#source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/helper_functions.R")


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
         serve_dir, 
         x_net_serve, y_net_serve, z_net_serve, 
         is_fault, is_doublefault, 
         is_prev_doublefault, is_ace, is_prev_ace,
         is_tiebreak, server_score, returner_score, 
         player1, player2, p1_cum_games, p2_cum_games, 
         p1_cum_sets, p2_cum_sets, match_id, year, 
         cruciality,
         serve_return_impact_x, serve_return_impact_y,
         serve_return_impact_z, 
         serve_return_net_x, serve_return_net_y,
         serve_return_net_z,
         serve_return_bounce_x, serve_return_bounce_y,
         serve_return_bounce_z,
         serve_plus1_bounce_x, serve_plus1_bounce_y,
         serve_plus1_bounce_z, match_id
  ) %>%
  mutate(
    serve_impact_from_center = abs(y_ball_at_serve),
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
    
    # -- Error Type
    error_type = ifelse(is.na(error_type),
                        'None', error_type),
    # # -- Classify Net Fault Error types
    # error_type = ifelse( ((is_fault == 1) & (z_net_serve <= get_net_height(y_coordinate= y_net_serve))),
    #                      'Net Error',
    #                      error_type),
    
    # intended_serve_bounce_x = ifelse(error_type == 'Net Error',
    #                                  get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
    #                                                                y_ball_at_serve = y_ball_at_serve,
    #                                                                z_ball_at_serve = z_ball_at_serve,
    #                                                                y_net_serve = y_net_serve,
    #                                                                z_net_serve = z_net_serve)[1],
    #                                  serveBounceCordinate_x),
    # intended_serve_bounce_y = ifelse(error_type == 'Net Error',
    #                                  get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
    #                                                                y_ball_at_serve = y_ball_at_serve,
    #                                                                z_ball_at_serve = z_ball_at_serve,
    #                                                                y_net_serve = y_net_serve,
    #                                                                z_net_serve = z_net_serve)[2],
    #                                  serveBounceCordinate_y),
    
    # -- Transform all coordinates to lie on the right part of the court.
    # Is it appropriate to take the absolute value?
    # On the right side of court, x_coord is always positive. However, y coord can be
    # (+) on Deuce or (-) on AdCourt
    # x_coord = ifelse( (which_side == 'right'), 
    #                   abs(intended_serve_bounce_x),
    #                   intended_serve_bounce_x),
    # # Something funky with some left side serves (all faults) being recorded as highly (-).
    # x_coord = ifelse( ((which_side == 'left') & (x_coord < 0)),
    #                   abs(x_coord),
    #                   x_coord),
    # y_coord = ifelse( (which_side == 'right'),
    #                   -1*(intended_serve_bounce_y),
    #                   intended_serve_bounce_y)
    
    
    
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


# -- Check that imputed serve directions actually make sense...
# -- Something messed up with dplyr::mutate() when you're using your own functions...

atp_rolandgarros_training_data <- atp_rolandgarros_training_data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(    
    # -- Classify Net Fault Error types
    error_type = ifelse( ((is_fault == 1) & (z_net_serve <= get_net_height(y_coordinate= y_net_serve))),
                         'Net Error',
                         error_type),
    intended_serve_bounce_x = ifelse(error_type == 'Net Error',
                                 get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
                                                               y_ball_at_serve = y_ball_at_serve,
                                                               z_ball_at_serve = z_ball_at_serve,
                                                               y_net_serve = y_net_serve,
                                                               z_net_serve = z_net_serve)[1],
                                 serveBounceCordinate_x),
         intended_serve_bounce_y = ifelse(error_type == 'Net Error',
                                 get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
                                                               y_ball_at_serve = y_ball_at_serve,
                                                               z_ball_at_serve = z_ball_at_serve,
                                                               y_net_serve = y_net_serve,
                                                               z_net_serve = z_net_serve)[2],
                                 serveBounceCordinate_y))

# Convert your column lists into a dataframe
atp_rolandgarros_training_data <- as.data.frame(lapply(atp_rolandgarros_training_data,unlist)) 

#atp_rolandgarros_training_data %>% filter(error_type == 'Net Error') %>% View()


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
         serve_dir,
         x_net_serve, y_net_serve, z_net_serve, 
         is_fault, is_doublefault, 
         is_prev_doublefault, is_ace, is_prev_ace,
         is_tiebreak, server_score, returner_score, 
         player1, player2, p1_cum_games, p2_cum_games, 
         p1_cum_sets, p2_cum_sets, match_id, year, 
         cruciality,
         serve_return_impact_x, serve_return_impact_y,
         serve_return_impact_z, 
         serve_return_net_x, serve_return_net_y,
         serve_return_net_z,
         serve_return_bounce_x, serve_return_bounce_y,
         serve_return_bounce_z,
         serve_plus1_bounce_x, serve_plus1_bounce_y,
         serve_plus1_bounce_z, match_id
  ) %>%
  mutate(
    serve_impact_from_center = abs(y_ball_at_serve),
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
                        'None', error_type),
    # -- Classify Net Fault Error types
    # error_type = ifelse( ((is_fault == 1) & (z_net_serve <= get_net_height(y_coordinate= y_net_serve))),
    #                      'Net Error',
    #                      error_type),
    # 
    # intended_serve_bounce_x = ifelse(error_type == 'Net Error',
    #                                  get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
    #                                                                y_ball_at_serve = y_ball_at_serve,
    #                                                                z_ball_at_serve = z_ball_at_serve,
    #                                                                y_net_serve = y_net_serve,
    #                                                                z_net_serve = z_net_serve)[1],
    #                                  serveBounceCordinate_x),
    # intended_serve_bounce_y = ifelse(error_type == 'Net Error',
    #                                  get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
    #                                                                y_ball_at_serve = y_ball_at_serve,
    #                                                                z_ball_at_serve = z_ball_at_serve,
    #                                                                y_net_serve = y_net_serve,
    #                                                                z_net_serve = z_net_serve)[2],
    #                                  serveBounceCordinate_y),
    
    
    # -- Transform all coordinates to lie on the right part of the court.
    # Is it appropriate to take the absolute value?
    # On the right side of court, x_coord is always positive. However, y coord can be
    # (+) on Deuce or (-) on AdCourt
    # x_coord = ifelse( (which_side == 'right'), 
    #                   abs(intended_serve_bounce_x),
    #                   intended_serve_bounce_x),
    # # Something funky with some left side serves (all faults) being recorded as highly (-).
    # x_coord = ifelse( ((which_side == 'left') & (x_coord < 0)),
    #                   abs(x_coord),
    #                   x_coord),
    # y_coord = ifelse( (which_side == 'right'),
    #                   -1*(intended_serve_bounce_y),
    #                   intended_serve_bounce_y)
    
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

wta_rolandgarros_training_data <- wta_rolandgarros_training_data %>%
  rowwise() %>%
  mutate(    
    # -- Classify Net Fault Error types
    error_type = ifelse( ((is_fault == 1) & (z_net_serve <= get_net_height(y_coordinate= y_net_serve))),
                         'Net Error',
                         error_type),
    intended_serve_bounce_x = ifelse(error_type == 'Net Error',
                                     get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
                                                                   y_ball_at_serve = y_ball_at_serve,
                                                                   z_ball_at_serve = z_ball_at_serve,
                                                                   y_net_serve = y_net_serve,
                                                                   z_net_serve = z_net_serve)[1],
                                     serveBounceCordinate_x),
    intended_serve_bounce_y = ifelse(error_type == 'Net Error',
                                     get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
                                                                   y_ball_at_serve = y_ball_at_serve,
                                                                   z_ball_at_serve = z_ball_at_serve,
                                                                   y_net_serve = y_net_serve,
                                                                   z_net_serve = z_net_serve)[2],
                                     serveBounceCordinate_y))

wta_rolandgarros_training_data <- as.data.frame(lapply(wta_rolandgarros_training_data,unlist)) 
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
                                  bestof3 = TRUE)
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


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- recategorise intended serve direction -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

# -- Check that all serve direction changes only happen on Net Errors
atp_rolandgarros_training_data_with_importance %>%
  rowwise() %>% 
  mutate(
    intended_serve_dir = categorise_serve_direction(intended_serve_bounce_y)) %>%
  group_by(error_type) %>%
  summarise(num_changes = sum(!(serve_dir == intended_serve_dir), na.rm = T),
            num_no_changes = sum((serve_dir == intended_serve_dir), na.rm = T))

# -- Add intended serve direction column
atp_rolandgarros_training_data_with_importance <- 
atp_rolandgarros_training_data_with_importance %>%
  rowwise() %>% 
  mutate(
    # -- Sometimes, Tracking data is available... except for net coordinates. In these cases
    #    use the original serve direction.
    intended_serve_dir = ifelse(!is.na(categorise_serve_direction(intended_serve_bounce_y)),
                                categorise_serve_direction(intended_serve_bounce_y),
                                serve_dir)
    ) 

# atp_rolandgarros_training_data_with_importance %>%
#   select(intended_serve_dir, serve_dir) %>%
#   View()


# -- Do the same for WTA
wta_rolandgarros_training_data_with_importance <- 
  wta_rolandgarros_training_data_with_importance %>%
  rowwise() %>% 
  mutate(
    # -- Sometimes, Tracking data is available... except for net coordinates. In these cases
    #    use the original serve direction.
    intended_serve_dir = ifelse(!is.na(categorise_serve_direction(intended_serve_bounce_y)),
                                categorise_serve_direction(intended_serve_bounce_y),
                                serve_dir)
  ) 


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Add returner's backhand location -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
#colnames(atp_rolandgarros_training_data_with_importance)
atp_rolandgarros_training_data_with_importance <- 
atp_rolandgarros_training_data_with_importance %>%
  mutate(
    player_hands_match = ifelse(server_hand == returner_hand, 1, 0),
    returner_backhand_loc = ifelse(  ((court_side == 'AdCourt') & (returner_hand == 'right-handed')),
                                     'Wide', 
                                     ifelse( ((court_side == 'DeuceCourt') & (returner_hand == 'right-handed')),
                                             'T',
                                             ifelse( ((court_side == 'AdCourt') & (returner_hand == 'left-handed')),
                                                     'T',
                                                     ifelse( ((court_side == 'DeuceCourt') & (returner_hand == 'left-handed')), 
                                                             'Wide', NA) )))
  )

wta_rolandgarros_training_data_with_importance <- 
  wta_rolandgarros_training_data_with_importance %>%
  mutate(
    player_hands_match = ifelse(server_hand == returner_hand, 1, 0),
    returner_backhand_loc = ifelse(  ((court_side == 'AdCourt') & (returner_hand == 'right-handed')),
                                     'Wide', 
                                     ifelse( ((court_side == 'DeuceCourt') & (returner_hand == 'right-handed')),
                                             'T',
                                             ifelse( ((court_side == 'AdCourt') & (returner_hand == 'left-handed')),
                                                     'T',
                                                     ifelse( ((court_side == 'DeuceCourt') & (returner_hand == 'left-handed')), 
                                                             'Wide', NA) )))
  )


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Add Server's previous intended serve location -----
# -- Also transform point importance within a match -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Perform the dreaded for loop...
atp_match_processed_list <- list()

atp_match_ids <- unique(atp_rolandgarros_training_data_with_importance$match_id)
for (ID_index in 1:length(atp_match_ids)){
  # -- Get Match ID in Dataframe
  ID <- atp_match_ids[ID_index]
  
  # -- Subset data to only include the one match 
  atp_match_df <- atp_rolandgarros_training_data_with_importance %>%
    filter(match_id == ID) %>%
    dplyr::distinct() %>% # Some rows seem to repeat (eg: Fed vs Ruud)
    arrange(server_name, set_num, game_num, point_num, serve_num)
  
  # -- Get player names involved in match
  p1_name <- unique(atp_match_df$server_name)[1]
  p2_name <- unique(atp_match_df$server_name)[2]

  # -- Player 1's match DataFrame
  p1_match_df <- atp_match_df %>%
    filter(server_name == p1_name) 
  
  # -- Player 2's match DataFrame
  p2_match_df <- atp_match_df %>%
    filter(server_name == p2_name) 
  
  # -- Get previous serve locations
  p1_prev_intended_serve_loc1 <- c(NA, p1_match_df$intended_serve_dir[1:(nrow(p1_match_df)-1)])
  p1_prev_intended_serve_loc2 <- c(NA, NA, p1_match_df$intended_serve_dir[1:(nrow(p1_match_df)-2)])
  p2_prev_intended_serve_loc1 <- c(NA, p2_match_df$intended_serve_dir[1:(nrow(p2_match_df)-1)])
  p2_prev_intended_serve_loc2 <- c(NA, NA, p2_match_df$intended_serve_dir[1:(nrow(p2_match_df)-2)])
  
  
  if( ( length(p1_prev_intended_serve_loc1) != nrow(p1_match_df)) | ( length(p1_prev_intended_serve_loc2) != nrow(p1_match_df)) ){
    print(ID)
    break
  }
  
  if( ( length(p2_prev_intended_serve_loc1) != nrow(p2_match_df)) | ( length(p2_prev_intended_serve_loc2) != nrow(p2_match_df)) ){
    print(ID)
    break
  }
  
  # -- Add previous serve locations to DataFrame
  p1_match_df$prev_intended_serve_loc1 <- p1_prev_intended_serve_loc1
  p1_match_df$prev_intended_serve_loc2 <- p1_prev_intended_serve_loc2
  p2_match_df$prev_intended_serve_loc1 <- p2_prev_intended_serve_loc1
  p2_match_df$prev_intended_serve_loc2 <- p2_prev_intended_serve_loc2
 
  # -- Scale Point importance
  # -- This is fishy since you can't scale point importance when making predictions on a 
  #    a test set. I.e. you wouldn't know beforehand what the most important points would be.
  p1_match_df$minmax_scaled_point_importance <- (p1_match_df$point_importance - min(p1_match_df$point_importance)) / (max(p1_match_df$point_importance) - min(p1_match_df$point_importance))
  p2_match_df$minmax_scaled_point_importance <- (p2_match_df$point_importance - min(p2_match_df$point_importance)) / (max(p2_match_df$point_importance) - min(p2_match_df$point_importance))
  p1_match_df$z_scaled_point_importance <- scale(p1_match_df$point_importance)
  p2_match_df$z_scaled_point_importance <- scale(p2_match_df$point_importance)
  
  atp_match_processed_list[[ID_index]] <- rbind(p1_match_df, p2_match_df)
  
}

atp_match_processed <- do.call(rbind, atp_match_processed_list)

# -- Update the scaling of point importance
atp_match_processed$minmax_scaled_point_importance <- (atp_match_processed$point_importance - min(atp_match_processed$point_importance)) / (max(atp_match_processed$point_importance) - min(atp_match_processed$point_importance))
atp_match_processed$z_scaled_point_importance <- scale(atp_match_processed$point_importance)

# -- Now repeat for WTA...
wta_match_processed_list <- list()

wta_match_ids <- unique(wta_rolandgarros_training_data_with_importance$match_id)
for (ID_index in 1:length(wta_match_ids)){
  # -- Get Match ID in Dataframe
  ID <- wta_match_ids[ID_index]
  
  # -- Subset data to only include the one match 
  wta_match_df <- wta_rolandgarros_training_data_with_importance %>%
    filter(match_id == ID) %>%
    dplyr::distinct() %>%
    arrange(server_name, set_num, game_num, point_num, serve_num)
  
  # -- Get player names involved in match
  p1_name <- unique(wta_match_df$server_name)[1]
  p2_name <- unique(wta_match_df$server_name)[2]
  
  # -- Player 1's match DataFrame
  p1_match_df <- wta_match_df %>%
    filter(server_name == p1_name) 
  
  # -- Player 2's match DataFrame
  p2_match_df <- wta_match_df %>%
    filter(server_name == p2_name) 
  
  # -- Get previous serve locations
  p1_prev_intended_serve_loc1 <- c(NA, p1_match_df$intended_serve_dir[1:(nrow(p1_match_df)-1)])
  p1_prev_intended_serve_loc2 <- c(NA, NA, p1_match_df$intended_serve_dir[1:(nrow(p1_match_df)-2)])
  p2_prev_intended_serve_loc1 <- c(NA, p2_match_df$intended_serve_dir[1:(nrow(p2_match_df)-1)])
  p2_prev_intended_serve_loc2 <- c(NA, NA, p2_match_df$intended_serve_dir[1:(nrow(p2_match_df)-2)])
  
  
  if( ( length(p1_prev_intended_serve_loc1) != nrow(p1_match_df)) | ( length(p1_prev_intended_serve_loc2) != nrow(p1_match_df)) ){
    print(ID)
    break
  }
  
  if( ( length(p2_prev_intended_serve_loc1) != nrow(p2_match_df)) | ( length(p2_prev_intended_serve_loc2) != nrow(p2_match_df)) ){
    print(ID)
    break
  }
  
  # -- Add previous serve locations to DataFrame
  p1_match_df$prev_intended_serve_loc1 <- p1_prev_intended_serve_loc1
  p1_match_df$prev_intended_serve_loc2 <- p1_prev_intended_serve_loc2
  p2_match_df$prev_intended_serve_loc1 <- p2_prev_intended_serve_loc1
  p2_match_df$prev_intended_serve_loc2 <- p2_prev_intended_serve_loc2
  
  # -- Scale Point importance
  p1_match_df$minmax_scaled_point_importance <- (p1_match_df$point_importance - min(p1_match_df$point_importance)) / (max(p1_match_df$point_importance) - min(p1_match_df$point_importance))
  p2_match_df$minmax_scaled_point_importance <- (p2_match_df$point_importance - min(p2_match_df$point_importance)) / (max(p2_match_df$point_importance) - min(p2_match_df$point_importance))
  p1_match_df$z_scaled_point_importance <- scale(p1_match_df$point_importance)
  p2_match_df$z_scaled_point_importance <- scale(p2_match_df$point_importance)
  
  wta_match_processed_list[[ID_index]] <- rbind(p1_match_df, p2_match_df)
  
}

wta_match_processed <- do.call(rbind, wta_match_processed_list)
# -- Update the scaling of point importance
wta_match_processed$minmax_scaled_point_importance <- (wta_match_processed$point_importance - min(wta_match_processed$point_importance)) / (max(wta_match_processed$point_importance) - min(wta_match_processed$point_importance))
wta_match_processed$z_scaled_point_importance <- scale(wta_match_processed$point_importance)



# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Save the data -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
write.csv(atp_match_processed,
          './collect_data/data/atp_processed_roland_garros_project.csv',
          row.names = FALSE)

write.csv(wta_match_processed,
          './collect_data/data/wta_processed_roland_garros_project.csv',
          row.names = FALSE)


