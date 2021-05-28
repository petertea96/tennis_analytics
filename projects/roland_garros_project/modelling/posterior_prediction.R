
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### == Fit Bayesian Hierarchical MNL on processed pbp data        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_project//")

library(dplyr)
library(rstan)
library(ggplot2)
library(ggridges)
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/ggplot_theme.R")

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

# nad_avg_lat_dist <- atp_training_data %>%
#   filter(server_name == 'R.NADAL') %>%
#   filter(is_second_serve==0) %>%
#   select(serve_impact_from_center) %>%
#   .$serve_impact_from_center %>%
#   mean()
# 
# nad_avg_long_dist <- atp_training_data %>%
#   filter(server_name == 'R.NADAL') %>%
#   filter(is_second_serve==0) %>%
#   select(distance_inside_serve) %>%
#   .$distance_inside_serve %>%
#   mean()

# Situation:
# Ad. court; Facing break point; Previous Serve Location is T; RH returner; 
# First Serve; 
# Server down a set, trailing 5-6 in games in the 2nd set

# -- We can do Prediction manually, using the ALREADY compiled posterior samples...----- 
# I.e. we don't need to re-fit using the generated quantities block in STAN
set.seed(824)
stanfit4 <- readRDS("./modelling/saved_models/stan_loglik/atp_loglik_fit4.RDS")
extract_model <- extract(stanfit4)
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/helper_functions.R")


fed_id <- 62

# -- Predict Federer on 1st serve -----
# -- T Logit
prob_T_logit_1st <-
(extract_model$v_id1[,fed_id,1] + extract_model$B_0[,1]) + # Intercept
extract_model$B_1[,1]*0 + # 1st Serve
extract_model$B_2[,1]*fed_avg_lat_dist + # Average Lateral Distance on 1st serve
extract_model$B_3[,1]*fed_avg_long_dist + 
extract_model$B_4[,1]*1 + # Previous location is T
extract_model$B_5[,1]*0 + 
extract_model$B_6[,1]*0 + 
extract_model$B_7[,1]*0.1443323 + 
extract_model$B_8[,1]*0 + 
extract_model$B_9[,1]*1 + 
extract_model$B_10[,1]*0 + 
extract_model$B_11[,1]*0 

# -- Wide Logit
prob_Wide_logit_1st <-
(extract_model$v_id1[,fed_id,2] + extract_model$B_0[,2]) +
extract_model$B_1[,2]*0 + # 1st Serve
extract_model$B_2[,2]*fed_avg_lat_dist + # Average Lateral Distance on 1st serve
extract_model$B_3[,2]*fed_avg_long_dist + 
extract_model$B_4[,2]*1 + # Previous location is T
extract_model$B_5[,2]*0 + 
extract_model$B_6[,2]*0 + 
extract_model$B_7[,2]*0.1443323 + 
extract_model$B_8[,2]*0 + 
extract_model$B_9[,2]*1 + 
extract_model$B_10[,2]*0 + 
extract_model$B_11[,2]*0 

prob_Body_logit <- rep(0, times = 4000)

prob_logit_1st <- data.frame(
  T_logit = prob_T_logit_1st,
  W_logit = prob_Wide_logit_1st,
  B_logit = prob_Body_logit 
)
normalized_probs_1st <- apply(prob_logit, MARGIN = 1, FUN = softmax) %>% t() 
federer_first_serve_preds <- apply(normalized_probs_1st, MARGIN = 1, FUN = predict_dir)
table(federer_first_serve_preds) / 4000

plot_federer_predicted_probs <- data.frame(
  prob = c(normalized_probs_1st[,1],
           normalized_probs_1st[,2],
           normalized_probs_1st[,3]),
  serve_dir = rep(c('T', 'Wide', 'Body'), each = 4000))
plot_federer_predicted_probs$serve_dir <- factor(plot_federer_predicted_probs$serve_dir,
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

# -- Predict Federer on 2nd serve -----
# -- T Logit
prob_T_logit_2nd <-
  (extract_model$v_id1[,fed_id,1] + extract_model$B_0[,1]) + # Intercept
  extract_model$B_1[,1]*1 + # 1st Serve
  extract_model$B_2[,1]*fed_avg_lat_dist + # Average Lateral Distance on 1st serve
  extract_model$B_3[,1]*fed_avg_long_dist + 
  extract_model$B_4[,1]*1 + # Previous location is T
  extract_model$B_5[,1]*0 + 
  extract_model$B_6[,1]*0 + 
  extract_model$B_7[,1]*0.1443323 + 
  extract_model$B_8[,1]*0 + 
  extract_model$B_9[,1]*1 + 
  extract_model$B_10[,1]*0 + 
  extract_model$B_11[,1]*0 

# -- Wide Logit
prob_Wide_logit_2nd <-
  (extract_model$v_id1[,fed_id,2] + extract_model$B_0[,2]) +
  extract_model$B_1[,2]*1 + # 1st Serve
  extract_model$B_2[,2]*fed_avg_lat_dist + # Average Lateral Distance on 1st serve
  extract_model$B_3[,2]*fed_avg_long_dist + 
  extract_model$B_4[,2]*1 + # Previous location is T
  extract_model$B_5[,2]*0 + 
  extract_model$B_6[,2]*0 + 
  extract_model$B_7[,2]*0.1443323 + 
  extract_model$B_8[,2]*0 + 
  extract_model$B_9[,2]*1 + 
  extract_model$B_10[,2]*0 + 
  extract_model$B_11[,2]*0 

prob_logit_2nd <- data.frame(
  T_logit = prob_T_logit_2nd,
  W_logit = prob_Wide_logit_2nd,
  B_logit = prob_Body_logit 
)

normalized_probs_2nd <- apply(prob_logit_2nd, MARGIN = 1, FUN = softmax) %>% t() 
federer_second_serve_preds <- apply(normalized_probs_2nd, MARGIN = 1, FUN = predict_dir)


plot_federer_predicted_probs_2 <- data.frame(
  prob = c(normalized_probs_2nd[,1],
           normalized_probs_2nd[,2],
           normalized_probs_2nd[,3]),
  serve_dir = rep(c('T', 'Wide', 'Body'), each = 4000))
plot_federer_predicted_probs_2$serve_dir <- factor(plot_federer_predicted_probs_2$serve_dir,
                                                 levels = c('Body', 'T', 'Wide'))

# -- Plot predictive posteriors
ggplot(data = plot_federer_predicted_probs_2,
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



# -- Bar plots of Federer's predicted serve directions

(table(federer_first_serve_preds) / 4000)
table(federer_second_serve_preds) / 4000 


plot_fed_preds_df <- data.frame(
  preds = c(table(federer_first_serve_preds)/4000, table(federer_second_serve_preds) / 4000),
  serve_dir = rep(c('T', 'Wide', 'Body'), times = 2),
  serve_num = rep(c(1,2), each= 3))

servenum.labs <- c("First Serve", "Second Serve")
names(servenum.labs) <- c("1", "2")

ggplot(data = plot_fed_preds_df) + 
  coord_flip() +
  geom_bar( aes(x = serve_dir, fill = serve_dir, y = preds), 
            stat = "identity", position = 'dodge', width = 0.8, color = 'black',
            show.legend = FALSE) +
  facet_wrap(~ serve_num,
             labeller = labeller(serve_num = servenum.labs)) +
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text = element_text(colour = 'black',face = 'bold')
  )+
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6)) +
  labs(#title = 'Men Estimated Serve Direction Probabilities',
    x='',
    y = 'Probability'#,
    #caption="Roland Garros 2019-20"
  ) +
  peter_theme(family_font = 'Tahoma') 


ggsave('./modelling/plots/federer_pred_barplot.jpg',
       width=5.5, height=4,
       dpi = 280)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Refitting the STAN Model... ----
# atp_stan_datalist4_pred_fed <- list(
#   N = nrow(atp_training_data),
#   N_1 = length(unique(atp_training_data$server_index)),
#   y = atp_training_data$y,
#   id_1 = atp_training_data$server_index,
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
#   K = 3,
#   x1_test = 0, # First Serve
#   x2_test = fed_avg_lat_dist,
#   x3_test = fed_avg_long_dist,
#   x4_test = 1, # Previous location is T
#   x5_test = 0,
#   x6_test = 0,
#   x7_test = 0.1443323,
#   x8_test = 0,
#   x9_test = 1,
#   x10_test = 0,
#   x11_test = 0,
#   test_p_id = 62
# )
# 
# options(mc.cores = 4)
# atp_model4 <- stan_model('./modelling/stan_files/posterior_prediction.stan')
# 
# fit_atp_model4_fed <- sampling(atp_model4, 
#                            data = atp_stan_datalist4_pred_fed, 
#                            iter = 2000)


