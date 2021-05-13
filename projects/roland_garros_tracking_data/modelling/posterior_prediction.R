
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### == Fit Bayesian Hierarchical MNL on processed pbp data        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

library(dplyr)
library(rstan)
options(tibble.print_max = 30)

# -- Fit STAN Model
library(cmdstanr)
library(data.table)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====              Data Processing Steps                      =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
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

atp_stan_datalist4 <- list(
  N = nrow(atp_training_data),
  N_1 = length(unique(atp_training_data$server_index)),
  y = atp_training_data$y,
  id_1 = atp_training_data$server_index,
  x1 = atp_training_data$is_second_serve,
  x2 = atp_training_data$serve_impact_from_center,
  x3 = atp_training_data$distance_inside_serve,
  x4 = atp_training_data$prev_intended_serve_loc1T,
  x5 = atp_training_data$prev_intended_serve_loc1Wide,
  x6 = atp_training_data$returner_backhand_locT,
  x7 = atp_training_data$point_importance,
  x8 = atp_training_data$server_handL,
  x9 = atp_training_data$court_side_ad,
  x10 = atp_training_data$interaction_s_hand_court_side,
  x11 = atp_training_data$interaction_ss_bh_T,
  K = 3,
  x1_test = 0,
  x2_test = mean(atp_training_data$serve_impact_from_center),
  x3_test = mean(atp_training_data$distance_inside_serve),
  x4_test = 1,
  x5_test = 0,
  x6_test = 0,
  x7_test = 0.3,
  x8_test = 1,
  x9_test = 0,
  x10_test = 0,
  x11_test = 0,
  test_p_id = 62
)


atp_model_pred <- cmdstan_model('./modelling/stan_files/posterior_prediction.stan')


fit_pred_model <- atp_model_pred$variational(
  data = atp_stan_datalist4,
  tol_rel_obj = 0.001
)

stanfit1 <- rstan::read_stan_csv(fit_pred_model$output_files())
names(stanfit1)

rstan::extract(object = stanfit1,
               pars = 'y_test') %>%
  .$y_test %>%
  table()

rstan::extract(object = stanfit1,
               pars = 'y_test_pred')

rstan::extract(object = stanfit1,
               pars = 'p')
