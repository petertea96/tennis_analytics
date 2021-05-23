### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    SCRIPT FITTING STAN BAYESIAN MODELS W/ RSTAN           =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

library(dplyr)
library(rstan)
library(loo)
options(tibble.print_max = 30)



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


# atp_stan_datalist3 <- list(
#   N = nrow(atp_training_data),
#   y = atp_training_data$y,
#   x1 = atp_training_data$is_second_serve,
#   x2 = atp_training_data$serve_impact_from_center,
#   x3 = atp_training_data$distance_inside_serve,
#   x4 = atp_training_data$prev_intended_serve_loc1T,
#   x5 = atp_training_data$prev_intended_serve_loc1Wide,
#   x6 = atp_training_data$returner_backhand_locT,
#   x7 = atp_training_data$point_importance,
#   x8 = atp_training_data$server_handL,
#   x9 = atp_training_data$court_side_ad,
#   x10 = atp_training_data$interaction_s_hand_court_side,
#   x11 = atp_training_data$interaction_ss_bh_T,
#   K = 3
# )

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
atp_model4 <- stan_model('./modelling/stan_files/loglik_stan/loglik_model_4.stan')

# ~1 minute to run
fit_atp_model1 <- sampling(atp_model1, 
                           data = atp_stan_datalist1, 
                           iter = 2000)
saveRDS(fit_atp_model1, file = "./modelling/saved_models/stan_loglik/atp_loglik_fit1.RDS")

#~10 minutes to run
fit_atp_model2 <- sampling(atp_model2, 
                           data = atp_stan_datalist2, 
                           iter = 2000)
saveRDS(fit_atp_model2, file = "./modelling/saved_models/stan_loglik/atp_loglik_fit2.RDS")

# ~ 102 minutes
fit_atp_model4 <- sampling(atp_model4, 
                           data = atp_stan_datalist4, 
                           iter = 2000)
saveRDS(fit_atp_model4, file = "./modelling/saved_models/stan_loglik/atp_loglik_fit4.RDS")

summary(fit_atp_model4)

stanfit1 <- readRDS("./modelling/saved_models/stan_loglik/atp_loglik_fit1.RDS")
stanfit2 <- readRDS("./modelling/saved_models/stan_loglik/atp_loglik_fit2.RDS")
stanfit4 <- readRDS("./modelling/saved_models/stan_loglik/atp_loglik_fit4.RDS")

log_lik_1 <- extract_log_lik(stanfit1)
log_lik_2 <- extract_log_lik(stanfit2)
log_lik_4 <- extract_log_lik(stanfit4)

waic1 <- waic(log_lik_1)
waic2 <- waic(log_lik_2)
waic4 <- waic(log_lik_4)
print(loo_compare(waic1, waic2, waic4))

# waic is -2*elpd_diff

# Weights
waics <- c(
  waic1$estimates["elpd_waic", 1],
  waic2$estimates["elpd_waic", 1],
  waic4$estimates["elpd_waic", 1]
)
waic_wts <- exp(waics) / sum(exp(waics))
waic_wts
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==  FITTING STAN BAYESIAN MODELS W/ WTA DATA           =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

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


wta_stan_datalist1 <- list(
  N = nrow(wta_training_data),
  y = wta_training_data$y,
  K = 3
)


wta_stan_datalist2 <- list(
  N = nrow(wta_training_data),
  N_1 = length(unique(wta_training_data$server_index)),
  id_1 = wta_training_data$server_index,
  y = wta_training_data$y,
  K = 3
)

wta_stan_datalist4 <- list(
  N = nrow(wta_training_data),
  N_1 = length(unique(wta_training_data$server_index)),
  y = wta_training_data$y,
  id_1 = wta_training_data$server_index,
  x1 = wta_training_data$is_second_serve,
  x2 = wta_training_data$serve_impact_from_center,
  x3 = wta_training_data$distance_inside_serve,
  x4 = wta_training_data$prev_intended_serve_loc1T,
  x5 = wta_training_data$prev_intended_serve_loc1Wide,
  x6 = wta_training_data$returner_backhand_locT,
  x7 = wta_training_data$point_importance,
  x8 = wta_training_data$server_handL,
  x9 = wta_training_data$court_side_ad,
  x10 = wta_training_data$interaction_s_hand_court_side,
  x11 = wta_training_data$interaction_ss_bh_T,
  K = 3
)



options(mc.cores = 4)
wta_model1 <- stan_model('./modelling/stan_files/loglik_stan/loglik_model_1.stan')
wta_model2 <- stan_model('./modelling/stan_files/loglik_stan/loglik_model_2.stan')
wta_model4 <- stan_model('./modelling/stan_files/loglik_stan/loglik_model_4.stan')

# ~1 minute to run
wta_stanfit1 <- sampling(wta_model1, 
                         data = wta_stan_datalist1, 
                         iter = 2000)
saveRDS(wta_stanfit1, file = "./modelling/saved_models/stan_loglik/wta_loglik_fit1.RDS")

#~10 minutes to run
wta_stanfit2 <- sampling(wta_model2, 
                         data = wta_stan_datalist2, 
                         iter = 2000)

saveRDS(wta_stanfit2, file = "./modelling/saved_models/stan_loglik/wta_loglik_fit2.RDS")

# ~ 102 minutes
wta_stanfit4 <- sampling(wta_model4, 
                         data = wta_stan_datalist4, 
                         iter = 2000)
saveRDS(wta_stanfit4, file = "./modelling/saved_models/stan_loglik/wta_loglik_fit4.RDS")

wta_log_lik_1 <- extract_log_lik(wta_stanfit1)
wta_log_lik_2 <- extract_log_lik(wta_stanfit2)
wta_log_lik_4 <- extract_log_lik(wta_stanfit4)

wta_waic1 <- waic(wta_log_lik_1)
wta_waic2 <- waic(wta_log_lik_2)
wta_waic4 <- waic(wta_log_lik_4)
print(loo_compare(wta_waic1, wta_waic2, wta_waic4))
