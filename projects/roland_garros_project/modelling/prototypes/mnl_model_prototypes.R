# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
###             Modelling EDA                                --------
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data")
atp_pbp_df <- read.csv('./collect_data/data/atp_processed_roland_garros_tracking_data.csv',
                       na.strings=c("","NA"))

#atp_pbp_df %>% select(fault_distance_missed_m, intended_serve_dir) %>% View()
# -- Does toss height vary?
atp_pbp_df %>%
  filter(z_ball_at_serve > 1) %>%
  select(z_ball_at_serve) %>%
  .$z_ball_at_serve %>%
  hist()

atp_pbp_df %>%
  group_by(server_name) %>%
  summarise(avg_toss_height = mean(z_ball_at_serve, na.rm = TRUE),
            sd_toss_height = sd(z_ball_at_serve, na.rm = TRUE)) %>%
  arrange(desc(sd_toss_height)) %>%
  View()

atp_pbp_df %>%
  group_by(server_name) %>%
  summarise(avg_toss_height = mean(abs(x_ball_at_serve), na.rm = TRUE),
            sd_toss_height = sd(abs(x_ball_at_serve), na.rm = TRUE)) %>%
  arrange(desc(avg_toss_height)) %>%
  View()

# -- As prototype, fit model using Thiem's serves
player_name <- 'D.THIEM'
player_name <- 'R.NADAL'
player_name <- 'N.DJOKOVIC'
player_name <- 'S.TSITSIPAS'
# -- Which variables seem to have an impact on intended serve direction?
# -- Run a simple MNL model and peek at the significance of the candidate covariates

# -- Code Body serves to be the baseline
training_data_multinom <- atp_pbp_df %>%
  select(point_ID, server_name, returner_name,
         server_hand, returner_hand,
         serve_num, court_side, serve_impact_from_center,
         z_ball_at_serve, x_ball_at_serve,
         is_break_point, is_prev_doublefault, is_prev_ace,
         prev_intended_serve_loc1, prev_intended_serve_loc2,
         point_importance, z_scaled_point_importance,
         minmax_scaled_point_importance, intended_serve_dir,
         player_hands_match, returner_backhand_loc) %>%
  #filter(server_name == player_name) %>%
  mutate(intended_serve_dir = ifelse(intended_serve_dir == 'Body', 1, 
                                     ifelse(intended_serve_dir == 'T', 2, 
                                            ifelse(intended_serve_dir == 'Wide', 3, NA))),
         
         is_court_ad = ifelse(court_side =='DeuceCourt', 0, 1),
         is_court_deuce = ifelse(court_side =='DeuceCourt', 1, 0),
         is_first_serve = ifelse(serve_num == 2, 0 , serve_num),
         is_server_left_handed = ifelse(server_hand == 'left-handed', 1, 0),
         distance_inside_serve = 11.89 - abs(x_ball_at_serve)
         
         # -- Variables that don't seem to be significant
         
         #second_serve_df = ifelse( (serve_num == 2) & (is_prev_doublefault==1), 1,0)
  )   %>%
  filter(complete.cases(.)) 


# -- Data check
# training_data_multinom %>%
#   select(point_ID, intended_serve_dir, prev_intended_serve_loc1, prev_intended_serve_loc2) %>%
#   View()

# -- Proportion of each serve direction
training_data_multinom %>%
  group_by(intended_serve_dir) %>%
  summarise(prop = n() / nrow(training_data_multinom))


mosaicplot(~ intended_serve_dir + is_court_ad, data=training_data_multinom)
mosaicplot(~ intended_serve_dir + is_first_serve, data=training_data_multinom)
#mosaicplot(~ intended_serve_dir + cruciality, data=training_data)
mosaicplot(~ intended_serve_dir + is_prev_doublefault, data=training_data_multinom)
#mosaicplot(~ intended_serve_dir + prev_intended_serve_loc1, data=training_data_multinom)
#mosaicplot(~ intended_serve_dir + z_scaled_point_importance, data=training_data)

ggplot(training_data_multinom, 
       aes(x= point_importance, 
           fill=as.factor(intended_serve_dir) )) +
  geom_density( aes(y = ..density.., fill=as.factor(intended_serve_dir)), 
                alpha = 0.3) 


training_data %>%
  group_by(is_prev_doublefault, intended_serve_dir) %>%
  summarise(count = n())


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====     Fit a simple MNL                                    =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# By default, the 1st level in nnet::multinom() is used as the reference category!

mnl_nnet_fit <- nnet::multinom(intended_serve_dir ~ is_first_serve +
                                 is_first_serve * returner_backhand_loc +
                                 #is_first_serve*is_court_ad +
                                 #is_court_ad + 
                                 is_court_ad*is_server_left_handed+
                                 #is_court_deuce*point_importance +
                                 #z_scaled_point_importance + 
                                 #minmax_scaled_point_importance + 
                                 point_importance +
                                 is_server_left_handed + 
                                 #is_break_point + 
                                 serve_impact_from_center + 
                                 distance_inside_serve +
                                 z_ball_at_serve + 
                                 #x_ball_at_serve + 
                                 prev_intended_serve_loc1 + prev_intended_serve_loc2 + 
                                 is_prev_ace + 
                                 is_prev_doublefault  + 
                                 player_hands_match + returner_backhand_loc,
                                 data = training_data_multinom)
summary(mnl_nnet_fit)
car::Anova(mnl_nnet_fit)

# Misclassification Errors
pred.class <- predict(mnl_nnet_fit, newdata=training_data)

(mul.misclass <- mean(ifelse(pred.class == training_data$intended_serve_dir, yes=0, no=1)))


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====     Fit a Bayesian MNL                                  =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
training_data <- atp_pbp_df %>%
  select(point_ID, server_name, returner_name,
         server_hand, returner_hand,
         serve_num, court_side, server_dist_from_center,
         is_break_point, is_prev_doublefault, is_prev_ace,
         prev_intended_serve_loc1, prev_intended_serve_loc2,
         point_importance, z_scaled_point_importance,
         minmax_scaled_point_importance, intended_serve_dir,
         player_hands_match, returner_backhand_loc) %>%
  filter(server_name == player_name) %>%
  mutate(intended_serve_dir = ifelse(intended_serve_dir == 'Body', 3, 
                                     ifelse(intended_serve_dir == 'T', 1, 
                                            ifelse(intended_serve_dir == 'Wide', 2, NA))),
         
         is_court_ad = ifelse(court_side =='DeuceCourt', 0, 1),
         is_first_serve = ifelse(serve_num == 2, 0 , serve_num),
  )   %>%
  filter(complete.cases(.)) 

library(rstan)
# -- Fit STAN Model
library(cmdstanr)
library(data.table)

entire_fit_data <- atp_pbp_df %>%
  select(point_ID, server_name, returner_name,
         server_hand, returner_hand,
         serve_num, court_side, server_dist_from_center,
         is_break_point, is_prev_doublefault, is_prev_ace,
         prev_intended_serve_loc1, prev_intended_serve_loc2,
         point_importance, z_scaled_point_importance,
         minmax_scaled_point_importance, intended_serve_dir,
         player_hands_match, returner_backhand_loc) %>%
  mutate(intended_serve_dir = ifelse(intended_serve_dir == 'Body', 3, 
                                     ifelse(intended_serve_dir == 'T', 1, 
                                            ifelse(intended_serve_dir == 'Wide', 2, NA))),
         
         is_court_ad = ifelse(court_side =='DeuceCourt', 0, 1),
         is_first_serve = ifelse(serve_num == 2, 0 , serve_num),
         is_bh_T = ifelse(returner_backhand_loc == 'T', 1,0),
         is_bh_Wide = ifelse(returner_backhand_loc == 'Wide', 1,0)
  )   %>%
  filter(complete.cases(.)) 

stan_dat <- list(
  N = nrow(entire_fit_data),
  y = entire_fit_data$intended_serve_dir,
  x1 = entire_fit_data$server_dist_from_center,
  x2 = entire_fit_data$is_prev_doublefault,
  x3 = entire_fit_data$is_prev_ace,
  x4 = entire_fit_data$is_first_serve,
  x5 = entire_fit_data$is_court_ad,
  x6 = entire_fit_data$is_bh_Wide,
  K = 3
)


model <- cmdstan_model('./modelling/fixed_effects_model.stan')	

fitted_model <- model$variational(
  data = stan_dat,
  tol_rel_obj = 0.001
)

plot(fitted_model, plotfun="trace", pars = c('B_0', 'B_1', 'B_2', 'B_3'))

serve_num_effects <- fitted_model$draws("B_4")
fitted_model$cmdstan_summary()




