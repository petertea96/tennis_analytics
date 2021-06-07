# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# R Processing 2021 RG Data

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/")

load(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/importance.RData")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/importance.R")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/impute_serve_location.R")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/categorise_serve_direction.R")
#source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/helper_functions.R")

rolandgarros_pbp_df <- read.csv('./roland_garros_2021.csv')

# -- Sojourn in Player IDs
match_data_log <- read.csv('./rg_2021_matches_in_repo.csv',
                        stringsAsFactors = FALSE)

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

# -- Some names still repeat
n_occur <- data.frame(table(player_df$id))
n_occur[n_occur$Freq > 1,]
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
### --              Add Player Handedness                       -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
player_df$id <- as.integer(player_df$id)

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Select variables we care about, and mutate variable types -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

rolandgarros_training_data <- rolandgarros_pbp_df %>%
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
         is_wta,
         serve_return_impact_x, serve_return_impact_y,
         serve_return_impact_z,
         serve_return_net_x, serve_return_net_y,
         serve_return_net_z, serve_return_bounce_x,
         serve_return_bounce_y, serve_return_bounce_z
         
        
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

  ) %>%
  # -- Add player names
  left_join(player_df, by = c('server_id' = 'id')) %>%
  rename(server_name = name) %>%
  left_join(player_df, by = c('returner_id' = 'id')) %>%
  rename(returner_name = name) %>%
  
  # -- Add player names again denoting player1 or player2 (confusing, I know)
  left_join(player_df, by = c('player1' = 'id')) %>%
  rename(p1_name = name) %>%
  left_join(player_df, by = c('player2' = 'id')) %>%
  rename(p2_name = name) %>%
  
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

rolandgarros_training_data <- rolandgarros_training_data %>%
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
rolandgarros_training_data <- as.data.frame(lapply(rolandgarros_training_data,unlist)) 

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
### --             Apply Point Importance                       -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Point Importance Functions accepts deuce scores as 3 - 3 
# So for example, we can't have an input score of 4 - 4

s_score <- vector()
r_score <- vector()

for(i in 1:nrow(rolandgarros_training_data)){
  is_tiebreak = rolandgarros_training_data[i, 'is_tiebreak']
  server_score = rolandgarros_training_data[i, 'server_score']
  returner_score = rolandgarros_training_data[i, 'returner_score']
  
  if(is_tiebreak) {
    if((server_score >= 7) & (returner_score >=7)){
      if( (server_score - returner_score) == -1 ){
        s_score[i] <- 6
        r_score[i] <- 7}
      else if( server_score == returner_score ){
        s_score[i] <- 6
        r_score[i] <- 6}
      else if((server_score - returner_score) == 1){
        s_score[i] <- 7
        r_score[i] <- 6}
      # else{
      #   atp_s_score[i] <- NA
      #   atp_r_score[i] <- NA}
    }
    else{
      s_score[i] <- server_score
      r_score[i] <- returner_score
      
    }
  }
  else{
    if(((server_score >= 4) & (returner_score >=4))){
      if( (server_score - returner_score) == -1 ){
        s_score[i] <- 3
        r_score[i] <- 4}
      else if( server_score == returner_score ){
        s_score[i] <- 3
        r_score[i] <- 3}
      else if((server_score - returner_score) == 1){
        s_score[i] <- 4
        r_score[i] <- 3}
      else{
        s_score[i] <- NA
        r_score[i] <- NA}}
    else{
      s_score[i] <- server_score
      r_score[i] <- returner_score}
  }
}

rolandgarros_training_data$s_score <- s_score
rolandgarros_training_data$r_score <- r_score

# Now, add point importance
rolandgarros_training_data_with_importance <-
  rolandgarros_training_data %>%
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
                                  bestof3 = is_wta))

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- recategorise intended serve direction -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

# -- Check that all serve direction changes only happen on Net Errors
rolandgarros_training_data_with_importance %>%
  rowwise() %>% 
  mutate(
    intended_serve_dir = categorise_serve_direction(intended_serve_bounce_y)) %>%
  group_by(error_type) %>%
  summarise(num_changes = sum(!(serve_dir == intended_serve_dir), na.rm = T),
            num_no_changes = sum((serve_dir == intended_serve_dir), na.rm = T))

# -- Add intended serve direction column
rolandgarros_training_data_with_importance <- 
  rolandgarros_training_data_with_importance %>%
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
# atp_rolandgarros_training_data_with_importance <- 
#   atp_rolandgarros_training_data_with_importance %>%
#   mutate(
#     player_hands_match = ifelse(server_hand == returner_hand, 1, 0),
#     returner_backhand_loc = ifelse(  ((court_side == 'AdCourt') & (returner_hand == 'right-handed')),
#                                      'Wide', 
#                                      ifelse( ((court_side == 'DeuceCourt') & (returner_hand == 'right-handed')),
#                                              'T',
#                                              ifelse( ((court_side == 'AdCourt') & (returner_hand == 'left-handed')),
#                                                      'T',
#                                                      ifelse( ((court_side == 'DeuceCourt') & (returner_hand == 'left-handed')), 
#                                                              'Wide', NA) )))
#   )
# 

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Add Server's previous intended serve location -----
# -- Also transform point importance within a match -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Perform the dreaded for loop...
# atp_match_processed_list <- list()
# 
# atp_match_ids <- unique(atp_rolandgarros_training_data_with_importance$match_id)
# for (ID_index in 1:length(atp_match_ids)){
#   # -- Get Match ID in Dataframe
#   ID <- atp_match_ids[ID_index]
#   
#   # -- Subset data to only include the one match 
#   atp_match_df <- atp_rolandgarros_training_data_with_importance %>%
#     filter(match_id == ID) %>%
#     dplyr::distinct() %>% # Some rows seem to repeat (eg: Fed vs Ruud)
#     arrange(server_name, set_num, game_num, point_num, serve_num)
#   
#   # -- Get player names involved in match
#   p1_name <- unique(atp_match_df$server_name)[1]
#   p2_name <- unique(atp_match_df$server_name)[2]
#   
#   # -- Player 1's match DataFrame
#   p1_match_df <- atp_match_df %>%
#     filter(server_name == p1_name) 
#   
#   # -- Player 2's match DataFrame
#   p2_match_df <- atp_match_df %>%
#     filter(server_name == p2_name) 
#   
#   # -- Get previous serve locations
#   p1_prev_intended_serve_loc1 <- c(NA, p1_match_df$intended_serve_dir[1:(nrow(p1_match_df)-1)])
#   p1_prev_intended_serve_loc2 <- c(NA, NA, p1_match_df$intended_serve_dir[1:(nrow(p1_match_df)-2)])
#   p2_prev_intended_serve_loc1 <- c(NA, p2_match_df$intended_serve_dir[1:(nrow(p2_match_df)-1)])
#   p2_prev_intended_serve_loc2 <- c(NA, NA, p2_match_df$intended_serve_dir[1:(nrow(p2_match_df)-2)])
#   
#   
#   if( ( length(p1_prev_intended_serve_loc1) != nrow(p1_match_df)) | ( length(p1_prev_intended_serve_loc2) != nrow(p1_match_df)) ){
#     print(ID)
#     break
#   }
#   
#   if( ( length(p2_prev_intended_serve_loc1) != nrow(p2_match_df)) | ( length(p2_prev_intended_serve_loc2) != nrow(p2_match_df)) ){
#     print(ID)
#     break
#   }
#   
#   # -- Add previous serve locations to DataFrame
#   p1_match_df$prev_intended_serve_loc1 <- p1_prev_intended_serve_loc1
#   p1_match_df$prev_intended_serve_loc2 <- p1_prev_intended_serve_loc2
#   p2_match_df$prev_intended_serve_loc1 <- p2_prev_intended_serve_loc1
#   p2_match_df$prev_intended_serve_loc2 <- p2_prev_intended_serve_loc2
#   
#   # -- Scale Point importance
#   # -- This is fishy since you can't scale point importance when making predictions on a 
#   #    a test set. I.e. you wouldn't know beforehand what the most important points would be.
#   p1_match_df$minmax_scaled_point_importance <- (p1_match_df$point_importance - min(p1_match_df$point_importance)) / (max(p1_match_df$point_importance) - min(p1_match_df$point_importance))
#   p2_match_df$minmax_scaled_point_importance <- (p2_match_df$point_importance - min(p2_match_df$point_importance)) / (max(p2_match_df$point_importance) - min(p2_match_df$point_importance))
#   p1_match_df$z_scaled_point_importance <- scale(p1_match_df$point_importance)
#   p2_match_df$z_scaled_point_importance <- scale(p2_match_df$point_importance)
#   
#   atp_match_processed_list[[ID_index]] <- rbind(p1_match_df, p2_match_df)
#   
# }
# 
# atp_match_processed <- do.call(rbind, atp_match_processed_list)
# 
# # -- Update the scaling of point importance
# atp_match_processed$minmax_scaled_point_importance <- (atp_match_processed$point_importance - min(atp_match_processed$point_importance)) / (max(atp_match_processed$point_importance) - min(atp_match_processed$point_importance))
# atp_match_processed$z_scaled_point_importance <- scale(atp_match_processed$point_importance)
# 
# # -- Now repeat for WTA...
# wta_match_processed_list <- list()
# 
# wta_match_ids <- unique(wta_rolandgarros_training_data_with_importance$match_id)
# for (ID_index in 1:length(wta_match_ids)){
#   # -- Get Match ID in Dataframe
#   ID <- wta_match_ids[ID_index]
#   
#   # -- Subset data to only include the one match 
#   wta_match_df <- wta_rolandgarros_training_data_with_importance %>%
#     filter(match_id == ID) %>%
#     dplyr::distinct() %>%
#     arrange(server_name, set_num, game_num, point_num, serve_num)
#   
#   # -- Get player names involved in match
#   p1_name <- unique(wta_match_df$server_name)[1]
#   p2_name <- unique(wta_match_df$server_name)[2]
#   
#   # -- Player 1's match DataFrame
#   p1_match_df <- wta_match_df %>%
#     filter(server_name == p1_name) 
#   
#   # -- Player 2's match DataFrame
#   p2_match_df <- wta_match_df %>%
#     filter(server_name == p2_name) 
#   
#   # -- Get previous serve locations
#   p1_prev_intended_serve_loc1 <- c(NA, p1_match_df$intended_serve_dir[1:(nrow(p1_match_df)-1)])
#   p1_prev_intended_serve_loc2 <- c(NA, NA, p1_match_df$intended_serve_dir[1:(nrow(p1_match_df)-2)])
#   p2_prev_intended_serve_loc1 <- c(NA, p2_match_df$intended_serve_dir[1:(nrow(p2_match_df)-1)])
#   p2_prev_intended_serve_loc2 <- c(NA, NA, p2_match_df$intended_serve_dir[1:(nrow(p2_match_df)-2)])
#   
#   
#   if( ( length(p1_prev_intended_serve_loc1) != nrow(p1_match_df)) | ( length(p1_prev_intended_serve_loc2) != nrow(p1_match_df)) ){
#     print(ID)
#     break
#   }
#   
#   if( ( length(p2_prev_intended_serve_loc1) != nrow(p2_match_df)) | ( length(p2_prev_intended_serve_loc2) != nrow(p2_match_df)) ){
#     print(ID)
#     break
#   }
#   
#   # -- Add previous serve locations to DataFrame
#   p1_match_df$prev_intended_serve_loc1 <- p1_prev_intended_serve_loc1
#   p1_match_df$prev_intended_serve_loc2 <- p1_prev_intended_serve_loc2
#   p2_match_df$prev_intended_serve_loc1 <- p2_prev_intended_serve_loc1
#   p2_match_df$prev_intended_serve_loc2 <- p2_prev_intended_serve_loc2
#   
#   # -- Scale Point importance
#   p1_match_df$minmax_scaled_point_importance <- (p1_match_df$point_importance - min(p1_match_df$point_importance)) / (max(p1_match_df$point_importance) - min(p1_match_df$point_importance))
#   p2_match_df$minmax_scaled_point_importance <- (p2_match_df$point_importance - min(p2_match_df$point_importance)) / (max(p2_match_df$point_importance) - min(p2_match_df$point_importance))
#   p1_match_df$z_scaled_point_importance <- scale(p1_match_df$point_importance)
#   p2_match_df$z_scaled_point_importance <- scale(p2_match_df$point_importance)
#   
#   wta_match_processed_list[[ID_index]] <- rbind(p1_match_df, p2_match_df)
#   
# }
# 
# wta_match_processed <- do.call(rbind, wta_match_processed_list)
# # -- Update the scaling of point importance
# wta_match_processed$minmax_scaled_point_importance <- (wta_match_processed$point_importance - min(wta_match_processed$point_importance)) / (max(wta_match_processed$point_importance) - min(wta_match_processed$point_importance))
# wta_match_processed$z_scaled_point_importance <- scale(wta_match_processed$point_importance)



# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Save the data -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
write.csv(rolandgarros_training_data_with_importance,
          '/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/processed_roland_garros_2021.csv',
          row.names = FALSE)




