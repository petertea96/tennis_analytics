### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    PLOT MODEL RESULTS          =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")
library(dplyr)
library(rstan)

atp_data <- read.csv('./collect_data/data/atp_processed_roland_garros_tracking_data.csv',
                     stringsAsFactors = FALSE,
                     na.strings=c("","NA"))

atp_training_data <- atp_data %>%
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
         z_scaled_point_importance,
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

atp_training_data$server_index <- as.numeric(factor(atp_training_data$server_name))


# -- WTA
wta_data <- read.csv('./collect_data/data/wta_processed_roland_garros_tracking_data.csv',
                     stringsAsFactors = FALSE)


wta_training_data <- wta_data %>%
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
         z_scaled_point_importance,
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

wta_training_data$server_index <- as.numeric(factor(wta_training_data$server_name))




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    (ATP) PLOT MEDIAN T PREFERENCE VS WIDE PREFERENCE    =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#stanfit4 <- readRDS("./modelling/saved_models/stan_loglik/atp_loglik_fit4.RDS")
stanfit2 <- readRDS("./modelling/saved_models/stan_loglik/atp_loglik_fit2.RDS")

#extract_stan_model <- extract(stanfit4)
extract_stan_model <- extract(stanfit2)
names(extract_stan_model)

player_varying_intercepts <- extract_stan_model$v_id1
player_intercepts <- extract_stan_model$B_0

t_player_varying_intercepts <- player_varying_intercepts[,,1] + player_intercepts[,1]
wide_player_varying_intercepts <- player_varying_intercepts[,,2] +  player_intercepts[,2]


t_medians <- apply(t_player_varying_intercepts, 2, median) 
wide_medians <- apply(wide_player_varying_intercepts, 2, median) 


levels(factor(atp_training_data$server_name))


player_df <- data.frame(
  name = levels(factor(atp_training_data$server_name)),
  median_t = t_medians,
  median_wide = wide_medians)

player_num_matches <- 
atp_data %>%
  group_by(server_name) %>%
  summarise(num_matches = length(unique(match_id)))

player_df <- 
player_df %>%
  left_join(player_num_matches,
            by = c('name' = 'server_name'))


player_df %>%
  arrange(desc(median_wide)) %>%
  View()

player_df %>%
  arrange(desc(median_t)) %>%
  View()

player_df %>%
  filter(num_matches>=2)


ggplot(data = player_df, aes(x = median_t, y = median_wide)) +
  geom_point()



# -- WTA
wta_stanfit4 <- readRDS("./modelling/saved_models/stan_loglik/wta_loglik_fit4.RDS")
wta_extract_stan_model <- extract(wta_stanfit4)

wta_player_varying_intercepts <- wta_extract_stan_model$v_id1
wta_player_intercepts <- wta_extract_stan_model$B_0

t_wta_player_varying_intercepts <- wta_player_varying_intercepts[,,1] + wta_player_intercepts[,1]
wide_wta_player_varying_intercepts <- wta_player_varying_intercepts[,,2] +  wta_player_intercepts[,2]


wta_t_medians <- apply(t_wta_player_varying_intercepts, 2, median) 
wta_wide_medians <- apply(wide_wta_player_varying_intercepts, 2, median) 


levels(factor(wta_training_data$server_name))


wta_player_df <- data.frame(
  name = levels(factor(wta_training_data$server_name)),
  median_t = wta_t_medians,
  median_wide = wta_wide_medians)

wta_player_num_matches <- 
  wta_data %>%
  group_by(server_name) %>%
  summarise(num_matches = length(unique(match_id)))

wta_player_df <- 
  wta_player_df %>%
  left_join(wta_player_num_matches,
            by = c('name' = 'server_name'))


wta_player_df %>%
  arrange(desc(median_wide)) %>%
  View()

wta_player_df %>%
  filter(num_matches>=2)

ggplot(data = wta_player_df, aes(x = median_t, y = median_wide)) +
  geom_point()

