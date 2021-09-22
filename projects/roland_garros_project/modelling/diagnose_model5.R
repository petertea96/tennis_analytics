### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    SCRIPT FITTING STAN BAYESIAN MODELS W/ CMDSTAN           =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# A reviewer requested we fit player-varying covariates along with 
# player-varying intercepts...

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_project/")
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

# --- || --- || --- || --- || --- || --- || --- || 
# --- || --- || --- || --- || --- || --- || --- || 
# -- Prepare lists as inputs to STAN ----
# --- || --- || --- || --- || --- || --- || --- || 
# --- || --- || --- || --- || --- || --- || --- || 
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


atp_stan_datalist5 <- list(
  N = nrow(atp_training_data),
  num_players = length(unique(atp_training_data$server_index)),
  y = atp_training_data$y,
  player_id = atp_training_data$server_index,
  x1 = atp_training_data$point_importance,
  x2 = atp_training_data$serve_impact_from_center,
  x3 = atp_training_data$distance_inside_serve,
  x4 = atp_training_data$prev_intended_serve_loc1T,
  x5 = atp_training_data$prev_intended_serve_loc1Wide,
  x6 = atp_training_data$returner_backhand_locT,
  x7 = atp_training_data$court_side_ad,
  x8 = atp_training_data$server_handL,
  x9 = atp_training_data$is_second_serve,
  x10 = atp_training_data$interaction_s_hand_court_side,
  x11 = atp_training_data$interaction_ss_bh_T,
  K = 3
)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====                  FIT STAN MODEL                        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Model 1: Common intercept
# -- Model 2: Player-varying intercept
# -- Model 3: Common intercept + common covariates
# -- Model 4: Player-varying intercept + common covariates
# -- Model 5: Player-varying intercept and varying PI + common covariates
library(cmdstanr)
library(data.table)
options(mc.cores = 4)

{
atp_model1 <- cmdstan_model('./modelling/stan_files/loglik_stan/loglik_model_1.stan')
atp_model2 <- cmdstan_model('./modelling/stan_files/loglik_stan/loglik_model_2.stan')
atp_model4 <- cmdstan_model('./modelling/stan_files/loglik_stan/loglik_model_4.stan')
atp_model5 <- cmdstan_model('./modelling/stan_files/loglik_stan/loglik_model_5.stan')
}
# --- || --- || --- || --- || --- || --- || --- || 
# -- ADVI version (an approximation algorithm) ----
# --- || --- || --- || --- || --- || --- || --- || 
{
fit_atp_model1 <- atp_model1$variational(
  data = atp_stan_datalist1,
  tol_rel_obj = 0.001
)

fit_atp_model2 <- atp_model2$variational(
  data = atp_stan_datalist2,
  tol_rel_obj = 0.001
)

fit_atp_model4 <- atp_model4$variational(
  data = atp_stan_datalist4,
  tol_rel_obj = 0.001
)

fit_atp_model5 <- atp_model5$variational(
  data = atp_stan_datalist5,
  tol_rel_obj = 0.001
)
}

# --- || --- || --- || --- || --- || --- || --- || 
# -- Convert object types to stanfit ----
# --- || --- || --- || --- || --- || --- || --- || 
{
atp.stanfit1 <- rstan::read_stan_csv(fit_atp_model1$output_files())
saveRDS(atp.stanfit1, file = "./modelling/saved_models/advi/advi_atp_model1.RDS")

atp.stanfit2 <- rstan::read_stan_csv(fit_atp_model2$output_files())
saveRDS(atp.stanfit2, file = "./modelling/saved_models/advi/advi_atp_model2.RDS")

atp.stanfit4 <- rstan::read_stan_csv(fit_atp_model4$output_files())
saveRDS(atp.stanfit4, file = "./modelling/saved_models/advi/advi_atp_model4.RDS")

atp.stanfit5 <- rstan::read_stan_csv(fit_atp_model5$output_files())
saveRDS(atp.stanfit5, file = "./modelling/saved_models/advi/advi_atp_model5.RDS")
}


# --- || --- || --- || --- || --- || --- || --- || 
# -- Make model comparisons  ----
# --- || --- || --- || --- || --- || --- || --- || 
stanfit1 <- readRDS("./modelling/saved_models/advi/advi_atp_model1.RDS")
stanfit2 <- readRDS("./modelling/saved_models/advi/advi_atp_model2.RDS")
stanfit4 <- readRDS("./modelling/saved_models/advi/advi_atp_model4.RDS")
stanfit5 <- readRDS("./modelling/saved_models/advi/advi_atp_model5.RDS")

log_lik_1 <- extract_log_lik(stanfit1)
log_lik_2 <- extract_log_lik(stanfit2)
log_lik_4 <- extract_log_lik(stanfit4)
log_lik_5 <- extract_log_lik(stanfit5)

waic1 <- waic(log_lik_1)
waic2 <- waic(log_lik_2)
waic4 <- waic(log_lik_4)
waic5 <- waic(log_lik_5)

print(loo_compare(waic1, waic2, waic4, waic5))




# --- Plot player-varying covariate results
extract_stan_model <- extract(stanfit5)
names(extract_stan_model)


extract_stan_model$B_1 %>% dim()
# 1000 x 2 matrix: 2 columns represent Wide and T

extract_stan_model$v_slope %>% dim()
# 1000 x 74  x 2 matrix

extract_stan_model$v_slope[,,1] %>% dim()

player_data <- data.frame(matrix(ncol = 3, nrow = 74))
# | Player ID | T Mean | Wide Mean |
for(playerid in 1:74){
  player_t_mean = mean(extract_stan_model$B_1[,1] + extract_stan_model$v_slope[,playerid,1]) 
  player_wide_mean = mean(extract_stan_model$B_1[,2] + extract_stan_model$v_slope[,playerid,2])
  
  player_data[playerid, ] <- c(playerid, player_t_mean, player_wide_mean)
}

colnames(player_data) <- c('playerid', 't_mean', 'wide_mean')

player_data$name <- levels(factor(atp_training_data$server_name))

player_data %>% dplyr::arrange(desc(t_mean)) %>% View()

player_data %>% dplyr::arrange(desc(wide_mean)) %>% View()

library(ggplot2)

ggplot(data = player_data, aes (x = t_mean, y = wide_mean)) + 
  geom_point()


extract_stan_model$Rho_slope[,2,1] %>% hist()
  
  
  
