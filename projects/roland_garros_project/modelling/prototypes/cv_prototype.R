# -- Trying out CV

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
         player_hands_match,
         match_id
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


test_ids <- c('year_2019_SM012_tracking_data.json',
              'year_2019_SM002_tracking_data.json',
              'year_2019_SM001_tracking_data.json',
              'year_2020_SM001_tracking_data.json',
              'year_2020_SM002_tracking_data.json')

test_data <- atp_training_data %>%
  filter(match_id %in% test_ids)

training_data <- atp_training_data %>%
  filter(!match_id %in% test_ids)

atp_stan_datalist3 <- list(
  N = nrow(training_data),
  y = training_data$y,
  x1 = training_data$is_second_serve,
  x2 = training_data$serve_impact_from_center,
  x3 = training_data$distance_inside_serve,
  x4 = training_data$prev_intended_serve_loc1T,
  x5 = training_data$prev_intended_serve_loc1Wide,
  x6 = training_data$prev_intended_serve_loc2T,
  x7 = training_data$prev_intended_serve_loc2Wide,
  x8 = training_data$returner_backhand_locT,
  x9 = training_data$point_importance,
  x10 = training_data$server_handL,
  x11 = training_data$court_side_ad,
  x12 = training_data$interaction_s_hand_court_side,
  x13 = training_data$interaction_ss_bh_T,
  K = 3,
  N_test = nrow(test_data),
  x1_test = test_data$is_second_serve,
  x2_test = test_data$serve_impact_from_center,
  x3_test = test_data$distance_inside_serve,
  x4_test = test_data$prev_intended_serve_loc1T,
  x5_test = test_data$prev_intended_serve_loc1Wide,
  x6_test = test_data$prev_intended_serve_loc2T,
  x7_test = test_data$prev_intended_serve_loc2Wide,
  x8_test = test_data$returner_backhand_locT,
  x9_test = test_data$point_importance,
  x10_test = test_data$server_handL,
  x11_test = test_data$court_side_ad,
  x12_test = test_data$interaction_s_hand_court_side,
  x13_test = test_data$interaction_ss_bh_T
)



atp_model3 <- cmdstan_model('./modelling/stan_files/cv_stan/cv_model_3.stan')

fit_atp_model3 <- atp_model3$variational(
  data = atp_stan_datalist3,
  tol_rel_obj = 0.001
)


bayesplot::mcmc_hist(fit_atp_model3$draws(c("p_pred_vals"))[,1:5])
View(fit_atp_model3$draws(c("p_pred_vals"))[,c(1,1677,3353)])
# # -- Look at test error
fit_atp_model3$draws(c("y_new")) %>% dim()

pred_y_test <- fit_atp_model3$draws(c("y_new"))

# - rows are iteration; column is serve point obs


stan_pred <- apply(MARGIN = 2,
                   X = pred_y_test,
                   FUN = get_column_prediction)

mean((ifelse(stan_pred == test_data$y, 0,1)))

