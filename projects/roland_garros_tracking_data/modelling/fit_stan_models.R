### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    SCRIPT FITTING STAN BAYESIAN MODELS W/ RSTAN           =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

library(dplyr)
library(rstan)
options(tibble.print_max = 30)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====              Data Processing Steps                      =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
atp_data <- read.csv('./collect_data/data/atp_processed_roland_garros_tracking_data.csv',
                     stringsAsFactors = FALSE,
                     na.strings=c("","NA"))

# wta_data <- read.csv('./collect_data/data/wta_processed_roland_garros_tracking_data.csv',
#                      stringsAsFactors = FALSE)

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


atp_stan_datalist1 <- list(
  N = nrow(atp_training_data),
  y = atp_training_data$y,
  K = 3
)


atp_stan_datalist2 <- list(
  N = nrow(atp_training_data),
  N_1 = length(unique(atp_training_data$server_index)),
  id_1 = atp_training_data$server_index,
  y = atp_training_data$y,
  K = 3
)


atp_stan_datalist3 <- list(
  N = nrow(atp_training_data),
  y = atp_training_data$y,
  x1 = atp_training_data$is_second_serve,
  x2 = atp_training_data$serve_impact_from_center,
  x3 = atp_training_data$distance_inside_serve,
  x4 = atp_training_data$prev_intended_serve_loc1T,
  x5 = atp_training_data$prev_intended_serve_loc1Wide,
  x6 = atp_training_data$prev_intended_serve_loc2T,
  x7 = atp_training_data$prev_intended_serve_loc2Wide,
  x8 = atp_training_data$returner_backhand_locT,
  x9 = atp_training_data$point_importance,
  x10 = atp_training_data$server_handL,
  x11 = atp_training_data$court_side_ad,
  x12 = atp_training_data$interaction_s_hand_court_side,
  x13 = atp_training_data$interaction_ss_bh_T,
  K = 3
)

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
  x6 = atp_training_data$prev_intended_serve_loc2T,
  x7 = atp_training_data$prev_intended_serve_loc2Wide,
  x8 = atp_training_data$returner_backhand_locT,
  x9 = atp_training_data$point_importance,
  x10 = atp_training_data$server_handL,
  x11 = atp_training_data$court_side_ad,
  x12 = atp_training_data$interaction_s_hand_court_side,
  x13 = atp_training_data$interaction_ss_bh_T,
  K = 3
)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====                  FIT STAN MODEL                        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Model 1: Global intercept
# -- Model 2: Player-varying intercept
# -- Model 3: Global intercept + covariates
# -- Model 4: Player-varying intercept + covariates
# -- Model 5: Player-varying intercept and PI + covariates

options(mc.cores = 4)
atp_model1 <- stan_model('./modelling/stan_files/loglik_stan/loglik_model_1.stan')
atp_model2 <- stan_model('./modelling/stan_files/loglik_stan/loglik_model_2.stan')


fit_atp_model1 <- sampling(atp_model1, 
                           data = atp_stan_datalist1, 
                           iter = 1000)
saveRDS(fit_atp_model1, file = "./modelling/saved_models/loglik_fit1.RDS")

fit_atp_model2 <- sampling(atp_model2, 
                           data = atp_stan_datalist2, 
                           iter = 1000)
saveRDS(fit_atp_model2, file = "./modelling/saved_models/loglik_fit2.RDS")

stanfit1 <- readRDS("./modelling/saved_models/loglik_fit1.RDS")
stanfit2 <- readRDS("./modelling/saved_models/loglik_fit2.RDS")


log_lik_1 <- extract_log_lik(stanfit1)
log_lik_2 <- extract_log_lik(stanfit2)

waic1 <- waic(log_lik_1)
waic2 <- waic(log_lik_2)
print(loo_compare(waic1, waic2))
