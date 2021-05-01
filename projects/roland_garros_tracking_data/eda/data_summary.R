# -- Data summary
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data")
atp_pbp_df <- read.csv('./collect_data/data/atp_processed_roland_garros_tracking_data.csv',
                       na.strings=c("","NA"))

wta_pbp_df <- read.csv('./collect_data/data/wta_processed_roland_garros_tracking_data.csv',
                       na.strings=c("","NA"))

# -- number of matches
unique(atp_pbp_df$match_id) %>% length()
unique(wta_pbp_df$match_id) %>% length()

# -- Net errors
sum(atp_pbp_df$is_fault ==1)/ nrow(atp_pbp_df)
table(atp_pbp_df$error_type)
sum(atp_pbp_df$error_type == 'Net Error', na.rm=TRUE) /sum(atp_pbp_df$is_fault ==1)

sum(wta_pbp_df$is_fault ==1)/ nrow(wta_pbp_df)
sum(wta_pbp_df$error_type == 'Net Error', na.rm=TRUE) /sum(wta_pbp_df$is_fault ==1)

atp_training_data <- atp_pbp_df %>%
  distinct() %>%
  mutate( 
    # -- Remember, the last level is the reference level in the STAN model
    y = ifelse(intended_serve_dir == 'Body', 3, 
               ifelse(intended_serve_dir == 'T', 1, 
                      ifelse(intended_serve_dir == 'Wide', 2, NA))),
    is_first_serve = ifelse(serve_num == 1, 1,0) ) %>%
  select(y, server_name, returner_name, 
         is_break_point, x_ball_at_serve,
         is_first_serve,
         court_side,
         is_prev_doublefault, is_prev_ace, 
         serve_impact_from_center,
         server_hand, returner_hand,
         point_importance, 
         returner_backhand_loc,
         prev_intended_serve_loc1,
         prev_intended_serve_loc2,
         player_hands_match,
         z_ball_at_serve,
         player_hands_match
  ) %>%
  mutate(## --model.matrix() not cooperating with factors...I'll do this manually
    prev_intended_serve_loc1T = ifelse(prev_intended_serve_loc1 == 'T',1,0),
    prev_intended_serve_loc1Wide = ifelse(prev_intended_serve_loc1 == 'Wide',1,0),
    prev_intended_serve_loc2T = ifelse(prev_intended_serve_loc2 == 'T',1,0),
    prev_intended_serve_loc2Wide = ifelse(prev_intended_serve_loc2 == 'Wide',1,0),
    is_second_serve = ifelse(is_first_serve == 1,0,1),
    court_side_ad = ifelse(court_side == 'DeuceCourt', 0,1),
    returner_backhand_locT = ifelse(returner_backhand_loc == 'T', 1,0),
    server_handL = ifelse(server_hand == 'left-handed', 1,0),
    distance_inside_serve = 11.89 - abs(x_ball_at_serve),
    interaction_s_hand_court_side = server_handL * court_side_ad,
    interaction_ss_bh_T = is_second_serve*returner_backhand_locT
  ) %>%
  filter(complete.cases(.))

atp_pbp_df %>% filter(is_track_avail) %>% distinct() %>% nrow()

