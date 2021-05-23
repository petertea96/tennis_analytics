
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### == Fit Bayesian Hierarchical MNL on processed pbp data        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

library(dplyr)
library(rstan)
library(ggplot2)
library(ggridges)
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/src/ggplot_theme.R")

#options(tibble.print_max = 30)

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

# levels(factor(atp_training_data$server_name))
# Player Name | ID
# ................
# Federer     | 62
# ZVEREV      | 7
# THIEM       | 17
# DJOKOVIC    | 51
# NADAL       | 63


fed_avg_lat_dist <- atp_training_data %>%
  filter(server_name == 'R.FEDERER') %>%
  filter(is_second_serve==0) %>%
  select(serve_impact_from_center) %>%
  .$serve_impact_from_center %>%
  mean()

fed_avg_long_dist <- atp_training_data %>%
  filter(server_name == 'R.FEDERER') %>%
  filter(is_second_serve==0) %>%
  select(distance_inside_serve) %>%
  .$distance_inside_serve %>%
  mean()

nad_avg_lat_dist <- atp_training_data %>%
  filter(server_name == 'R.NADAL') %>%
  filter(is_second_serve==0) %>%
  select(serve_impact_from_center) %>%
  .$serve_impact_from_center %>%
  mean()

nad_avg_long_dist <- atp_training_data %>%
  filter(server_name == 'R.NADAL') %>%
  filter(is_second_serve==0) %>%
  select(distance_inside_serve) %>%
  .$distance_inside_serve %>%
  mean()

# Situation:
# Ad. court; Facing break point; Previous Serve Location is T; RH returner; 
# First Serve; 
# Server down a set, trailing 5-6 in games in the 2nd set

atp_stan_datalist4_pred_fed <- list(
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
  x1_test = 0, # First Serve
  x2_test = fed_avg_lat_dist,
  x3_test = fed_avg_long_dist,
  x4_test = 1, # Previous location is T
  x5_test = 0,
  x6_test = 0,
  x7_test = 0.1443323,
  x8_test = 0,
  x9_test = 1,
  x10_test = 0,
  x11_test = 0,
  test_p_id = 62
)


atp_stan_datalist4_pred_nad <- list(
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
  x1_test = 0, # First Serve
  x2_test = nad_avg_lat_dist,
  x3_test = nad_avg_long_dist,
  x4_test = 1, # Previous location is T
  x5_test = 0,
  x6_test = 0,
  x7_test = 0.1443323,
  x8_test = 1,
  x9_test = 1,
  x10_test = 1,
  x11_test = 0,
  test_p_id = 63
)

# -- Prediction with RSTAN -----
options(mc.cores = 4)
atp_model4 <- stan_model('./modelling/stan_files/posterior_prediction.stan')

fit_atp_model4_fed <- sampling(atp_model4, 
                           data = atp_stan_datalist4_pred_fed, 
                           iter = 2000)

fit_atp_model4_nad <- sampling(atp_model4, 
                               data = atp_stan_datalist4_pred_nad, 
                               iter = 2000)


# -- Prediction with ADVI -----
library(cmdstanr)
library(data.table)

atp_model_pred <- cmdstan_model('./modelling/stan_files/posterior_prediction.stan')


fit_pred_model_fed <- atp_model_pred$variational(
  data = atp_stan_datalist4_pred_fed,
  iter = 2000,
  tol_rel_obj = 0.001
)

fit_pred_model_nad <- atp_model_pred$variational(
  data = atp_stan_datalist4_pred_nad,
  iter = 2000,
  tol_rel_obj = 0.001
)


stanfit_test_fed <- rstan::read_stan_csv(fit_pred_model_fed$output_files())
stanfit_test_nad <- rstan::read_stan_csv(fit_pred_model_nad$output_files())

federer_model <- extract(object = stanfit_test_fed) 
nadal_model <- extract(object = stanfit_test_nad) 

federer_predicted_probs <- federer_model$y_test_pred
federer_predicted_categories <- federer_model$y_test
nadal_predicted_probs <- nadal_model$y_test_pred
nadal_predicted_categories <- nadal_model$y_test

plot_federer_predicted_probs <- data.frame(
  prob = c(federer_predicted_probs[,1],
           federer_predicted_probs[,2],
           federer_predicted_probs[,3]),
  serve_dir = rep(c('T', 'Wide', 'Body'), each = 1000))
plot_federer_predicted_probs$serve_dir <- factor(plot_federer_predicted_probs$serve_dir,
                                                 levels = c('Body', 'T', 'Wide'))

plot_nadal_predicted_probs <- data.frame(
  prob = c(nadal_predicted_probs[,1],
           nadal_predicted_probs[,2],
           nadal_predicted_probs[,3]),
  serve_dir = rep(c('T', 'Wide', 'Body'), each = 1000))
plot_nadal_predicted_probs$serve_dir <- factor(plot_nadal_predicted_probs$serve_dir,
                                                 levels = c('Body', 'T', 'Wide'))

# -- Plot predictive posteriors
ggplot(data = plot_federer_predicted_probs,
       aes(x = prob, y = serve_dir)) + 
  geom_density_ridges(aes(fill = as.factor(serve_dir)),
                      alpha = 0.5, show.legend = FALSE) +
  labs(x = "Probability", 
       y = "Serve Direction",
       #title = "Federer's Predicted Posterior Probabilities",
       fill = ''#,
       #caption = 'Roland Garros\n2019-20'
       ) +
  xlim(0,0.7) +
  peter_theme(family_font = 'Tahoma')

ggsave('/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/modelling/federer_predictive_post.jpg',
       width=6, height=4,
       dpi = 300)

federer_predicted_categories %>% table()

# -- NADAL's Predicted Posteriors
ggplot(data = plot_nadal_predicted_probs,
       aes(x = prob, y = serve_dir)) + 
  geom_density_ridges(aes(fill = as.factor(serve_dir)),
                      alpha = 0.5, show.legend = FALSE) +
  labs(x = "Probability", 
       y = "Serve Direction",
       title = "Nadal's Predicted Posterior Probabilities",
       fill = ''#,
       #caption = 'Roland Garros\n2019-20'
  ) +
  xlim(0,0.7) +
  peter_theme(family_font = 'Tahoma')
ggsave('/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/modelling/nadal_predictive_post.jpg',
       width=6, height=4,
       dpi = 300)

nadal_predicted_categories %>% table()




