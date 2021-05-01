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
# -- Variables omitted:
# is_prev_doublefault, is_prev_ace, is_break_point, z_ball_at_serve


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====                  FIT STAN MODEL                        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Model 1: Global intercept
# -- Model 2: Player-varying intercept
# -- Model 3: Global intercept + covariates
# -- Model 4: Player-varying intercept + covariates
# -- Model 5: Player-varying intercept and PI + covariates
# -- Model X: Stuff with Generated Quantities (pointwise log-lik & predictions)

atp_model1 <- cmdstan_model('./modelling/stan_files/model_1.stan')
atp_model2 <- cmdstan_model('./modelling/stan_files/model_2.stan')
atp_model3 <- cmdstan_model('./modelling/stan_files/model_3.stan')	
atp_model4 <- cmdstan_model('./modelling/stan_files/model_4.stan')
atp_model5 <- cmdstan_model('./modelling/stan_files/model_5.stan')
# -- ADVI version the fastest MCMC to fit
fit_atp_model1 <- atp_model1$variational(
  data = atp_stan_datalist1,
  tol_rel_obj = 0.001
)

fit_atp_model2 <- atp_model2$variational(
  data = atp_stan_datalist2,
  tol_rel_obj = 0.001
)

fit_atp_model3 <- atp_model3$variational(
  data = atp_stan_datalist3,
  tol_rel_obj = 0.001
)

fit_atp_model4 <- atp_model4$variational(
  data = atp_stan_datalist4,
  tol_rel_obj = 0.001
)

fit_atp_model5 <- atp_model5$variational(
  data = atp_stan_datalist4,
  tol_rel_obj = 0.001
)

# -- Check out summaries (too dense if you're including log likelihoods in
#    generated quantities)
fit_atp_model3$cmdstan_summary()
fit_atp_model3$summary()


fit_atp_model4$summary(variables = c("B_1", "B_2", 'B_3', 
                                     "B_4", "B_5", 'B_6',
                                     'B_7', "B_8", 'B_9',
                                     'B_10', 'B_11', 'B_12',
                                     'B_13'))

# -- Plot posteriors
bayesplot::mcmc_hist(fit_atp_model1$draws(c("B_0")))

bayesplot::mcmc_hist(fit_atp_model4$draws(c("B_1", "B_2", 'B_3', 
                                            "B_4", "B_5", 'B_6')))

bayesplot::mcmc_hist(fit_atp_model4$draws(c("B_7", "B_8", 'B_9',
                                            'B_10', 'B_11', 'B_12',
                                            'B_13')))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====     STAN MODEL DIAGNOSTICS / PERFORMANCE                =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- HOW TO USE LOO TO COMPARE MODEL FITS: https://mc-stan.org/loo/articles/loo2-with-rstan.html
# --I don't think this actually gives you CV- prediction error [0,1]...
# -- Create a stanfit object (required for loo package)

# -- Another tutorial to compare model fit with bayesian stacking.
# https://avehtari.github.io/modelselection/rats_kcv.html

# -- Doing a for loop cv
# https://cran.r-project.org/web/packages/loo/vignettes/loo2-elpd.html

# -- Convert object types to stanfit
stanfit1 <- rstan::read_stan_csv(fit_atp_model1$output_files())
stanfit2 <- rstan::read_stan_csv(fit_atp_model2$output_files())
stanfit3 <- rstan::read_stan_csv(fit_atp_model3$output_files())
stanfit4 <- rstan::read_stan_csv(fit_atp_model4$output_files())
stanfit5 <- rstan::read_stan_csv(fit_atp_model5$output_files())
# -- Save model fits
saveRDS(stanfit1, file = "./modelling/saved_models/fit1.RDS")
saveRDS(stanfit2, file = "./modelling/saved_models/fit2.RDS")
saveRDS(stanfit3, file = "./modelling/saved_models/fit3.RDS")
saveRDS(stanfit4, file = "./modelling/saved_models/fit4.RDS")
saveRDS(stanfit5, file = "./modelling/saved_models/fit5.RDS")
# -- Load previous models, if already saved
stanfit1 <- readRDS("./modelling/saved_models/fit1.RDS")
stanfit2 <- readRDS("./modelling/saved_models/fit2.RDS")
stanfit3 <- readRDS("./modelling/saved_models/fit3.RDS")
stanfit4 <- readRDS("./modelling/saved_models/fit4.RDS")
stanfit5 <- readRDS("./modelling/saved_models/fit5.RDS")
# Compute PSIS-LOO (an approximation to exact LOO-CV?)
log_lik_1 <- loo::extract_log_lik(stanfit1, merge_chains = FALSE)
r_eff_1 <- loo::relative_eff(exp(log_lik_1), cores = 2)
loo_1 <- loo::loo(log_lik_1, r_eff = r_eff_1, cores = 2)
print(loo_1)

log_lik_2 <- loo::extract_log_lik(stanfit2, merge_chains = FALSE)
r_eff_2 <- loo::relative_eff(exp(log_lik_2), cores = 2)
loo_2 <- loo::loo(log_lik_2, r_eff = r_eff_2, cores = 2)
print(loo_2)

log_lik_3 <- loo::extract_log_lik(stanfit3, merge_chains = FALSE)
r_eff_3 <- loo::relative_eff(exp(log_lik_3), cores = 2)
loo_3 <- loo::loo(log_lik_3, r_eff = r_eff_3, cores = 2)
print(loo_3)

log_lik_4 <- loo::extract_log_lik(stanfit4, merge_chains = FALSE)
r_eff_4 <- loo::relative_eff(exp(log_lik_4), cores = 2)
loo_4 <- loo::loo(log_lik_4, r_eff = r_eff_4, cores = 2)
print(loo_4)

log_lik_5 <- loo::extract_log_lik(stanfit5, merge_chains = FALSE)
r_eff_5 <- loo::relative_eff(exp(log_lik_5), cores = 2)
loo_5 <- loo::loo(log_lik_5, r_eff = r_eff_5, cores = 2)
print(loo_5)


# elpd_loo: expected log predictive density (High elpd == more predictive power)
# p_loo: Effective number of parameters
# looic: LOO Information Criterion

# Compare models
comp <- loo::loo_compare(loo_1, loo_2, loo_3, loo_4, loo_5)
print(comp)

# -- model averaging weights based on Bayesian stacking
lpd_point <- cbind(loo_1$pointwise[,"elpd_loo"],
                   loo_2$pointwise[,"elpd_loo"],
                   loo_3$pointwise[,"elpd_loo"],
                   loo_4$pointwise[,"elpd_loo"],
                   loo_5$pointwise[,"elpd_loo"]
                   )
loo::stacking_weights(lpd_point)


# -- estimated difference of expected leave-one-out prediction errors between the two models
# -- 
# -- Here, model 2 is preferred





### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====            Diagnose STAN Model 2                        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Expect Federer to serve towards T
draws_df <- posterior::as_draws_df(fit_atp_model2$draws())
post_means <- colMeans(draws_df[,5:(76*2)])
post_means_T <- post_means[rep(c(TRUE, FALSE), times = 74)]
post_means_Wide <- post_means[rep(c(FALSE, TRUE), times = 74)]

# -- Players most likely to serve T
post_means_T[order(post_means_T)]
atp_training_data %>%
  select(server_name, server_index) %>%
  distinct() %>%
  filter(server_index %in% c(18,62,47,56,6,19,69,70, 44, 51))


# -- Players most likely to serve Wide
post_means_Wide[order(post_means_Wide)]
order(post_means_Wide)
atp_training_data %>%
  select(server_name, server_index) %>%
  distinct() %>%
  filter(server_index %in% c(74,42,47,22,71,45,31,16,34,17))

