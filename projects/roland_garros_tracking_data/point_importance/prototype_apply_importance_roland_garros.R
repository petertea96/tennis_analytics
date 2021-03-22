# -- Prototype Point Importance Analysis for Roland Garros

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

atp_pbp_df <- read.csv('./eda/data/atp_roland_garros_19_20.csv')

playerid_df <- read.csv('collect_data/data/roland_garros_player_id.csv',
                        stringsAsFactors = FALSE)

#length(unique(playerid_df$id)) 
#nrow(playerid_df)

playerid_df$id <- as.integer(playerid_df$id)


# atp_pbp_df %>% 
#   filter(rally_length <=1) %>% 
#   select(point_ID, server_id, returner_id, point_winner_id,
#          serve_speed_kph, rally_length, is_fault,
#          is_doublefault,
#          point_end_type
#          ) %>%
#   View()

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Select variables we care about, and mutate variable types -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
training_data <- atp_pbp_df %>%
  select(point_ID, set_num, game_num, point_num, serve_num,
         server_id, returner_id, point_winner_id, 
         server_score, returner_score, player1, player2,
         p1_cum_games, p2_cum_games, p1_cum_sets, p2_cum_sets,
         court_side, is_break_point,
         is_break_point_converted, is_track_avail, serve_dir, 
         is_fault, is_doublefault, is_tiebreak,
         match_id, year
         #z_net_serve, serve_speed_kph, serve_type, y_ball_at_serve,
         ) %>%
  mutate(
    #dist_from_center = abs(y_ball_at_serve),
    is_break_point = ifelse(is_break_point == 'True',
                            1, 0),
    is_break_point_converted = ifelse(is_break_point_converted == 'True',
                                      1, 0),
    is_track_avail = ifelse(is_track_avail == 'True',
                            TRUE, FALSE),
    is_tiebreak = ifelse(is_tiebreak == 1,
                         TRUE, FALSE)#,
    
    #serve_speed_kph = as.numeric(gsub("([0-9]+).*$", "\\1", serve_speed_kph))
    
  ) %>%
  # -- Add player names
  left_join(playerid_df, by = c('server_id' = 'id')) %>%
  rename(server_name = name) %>%
  left_join(playerid_df, by = c('returner_id' = 'id')) %>%
  rename(returner_name = name) %>%
  
  # -- Add player names again denoting player1 or player2 (confusing, I know)
  left_join(playerid_df, by = c('player1' = 'id')) %>%
  rename(p1_name = name) %>%
  left_join(playerid_df, by = c('player2' = 'id')) %>%
  rename(p2_name = name) %>%
  
  # -- Configure p1 & p2 cumulative games/sets into server & returner
  mutate(
    s_cum_games = ifelse(server_name == p1_name,
                         p1_cum_games, p2_cum_games),
    r_cum_games = ifelse(returner_name == p1_name,
                         p1_cum_games, p2_cum_games),
    s_cum_sets = ifelse(server_name == p1_name,
                         p1_cum_sets, p2_cum_sets),
    r_cum_sets = ifelse(returner_name == p1_name,
                        p1_cum_sets, p2_cum_sets)
  )


training_data %>%
  filter(serve_num == 1) %>%
  filter(serve_dir %in% c('Body', 'Wide', 'T')) %>%
  group_by(server_name, serve_dir) %>%
  summarise(tot_serves = n(),
            df = sum(is_fault),
            df_rate = df/tot_serves) %>%
  View()

training_data %>%
  filter(serve_num == 2) %>%
  filter(serve_dir %in% c('Body', 'Wide', 'T')) %>%
  group_by(server_name, serve_dir) %>%
  summarise(tot_serves = n(),
            df = sum(is_doublefault),
            df_rate = df/tot_serves) %>%
  View()
  

# ***************************
# -- Code accepts deuce scores as 3 - 3 
# So for example, we can't have an input score of 4 - 4

s_score <- vector()
r_score <- vector()

for(i in 1:nrow(training_data)){
  is_tiebreak = training_data[i, 'is_tiebreak']
  server_score = training_data[i, 'server_score']
  returner_score = training_data[i, 'returner_score']
  
  if(is_tiebreak) {
    if((server_score >= 7) & (returner_score >=7)){
      if( (server_score - returner_score) == -1 ){
        s_score[i] <- 6
        r_score[i] <- 7}
      else if( server_score == returner_score ){
        s_score[i] <- 6
        r_score[i] <- 6}
      else if((server_score - returner_score) == 1){
        s_score[i] <- 7
        r_score[i] <- 6}
      else{
        s_score[i] <- NA
        r_score[i] <- NA}
    }
  }
  else{
    if(((server_score >= 4) & (returner_score >=4))){
      if( (server_score - returner_score) == -1 ){
        s_score[i] <- 3
        r_score[i] <- 4}
      else if( server_score == returner_score ){
        s_score[i] <- 3
        r_score[i] <- 3}
      else if((server_score - returner_score) == 1){
        s_score[i] <- 4
        r_score[i] <- 3}
      else{
        s_score[i] <- NA
        r_score[i] <- NA}}
    else{
      s_score[i] <- server_score
      r_score[i] <- returner_score}
    }
  }

training_data$s_score <- s_score
training_data$r_score <- r_score

table(training_data$s_score)
table(training_data$r_score)

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Remove matches that don't include atleast 70% of the tracking information -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
cutoff_tracking_available <- 0.7

ineligible_match_ids <- training_data %>%
  group_by(match_id) %>% 
  summarise(tot_serves = n(),
            pts_avail = sum(is_track_avail),
            percent_pts_available = pts_avail / tot_serves) %>%
  filter(percent_pts_available < cutoff_tracking_available) %>%
  .$match_id


training_data <- training_data %>%
  filter( !(match_id %in% ineligible_match_ids) )


# How many points do we have from each player?
training_data %>%
  group_by(server_name) %>%
  summarise(pts_avail = sum(is_track_avail)) %>%
  arrange(desc(pts_avail))

# ### --- ### --- ### --- ### --- ### --- ### --- ### #
# -- Add Point Importance -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### #
load(file = "/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/point_importance/importance.RData")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/point_importance/importance.R")

training_data_with_importance <-
  training_data %>%
  filter(complete.cases(.)) %>%
  #View() 
  rowwise() %>%
  mutate(
    point_importance = importance(point_x = s_score, 
                                  point_y = r_score,
                                  game_x = s_cum_games, 
                                  game_y = r_cum_games, 
                                  set_x = s_cum_sets, 
                                  set_y = r_cum_sets,
                                  tiebreak = is_tiebreak, 
                                  bestof3 = FALSE)
  )

summary(training_data_with_importance$point_importance)

# -- Not apppropriate to include 5th set tiebreaks
table(training_data_with_importance$game_num)

training_data_with_importance %>%
  filter( ((set_num == 5) & (game_num == 13)) )


full_data <- training_data_with_importance %>%
  filter(serve_num == 1) %>%
  mutate(
    is_wide = ifelse(serve_dir == 'Wide',
                     1,0),
    is_t = ifelse(serve_dir == 'T',
                  1,0),
    is_deuce_court = ifelse(court_side == 'DeuceCourt',
                            1,0)
    
    # STAN wants player id coded in mumbers starting from 1,..., L
  ) %>%
  select(point_importance,
         is_wide,
         is_t,
         server_id,
         server_name,
         is_deuce_court,
         is_break_point,
         is_fault,
         is_doublefault,
         serve_num
  ) 
full_data$z_point_importance <- scale(full_data$point_importance)
summary(full_data$z_point_importance)

full_data$new_server_id = as.numeric(as.factor(full_data$server_name))
table(full_data$new_server_id)


full_data %>%
  group_by(server_name, is_wide, is_t) %>%
  summarise(tot_serves = n(),
            df = sum(is_fault),
            df_rate = df/tot_serves) %>%
  View()
  

# ### --- ### --- ### --- ### --- ### --- ###
### -- Predict Prob(Fault)  -----
# ### --- ### --- ### --- ### --- ### --- ###
test_model <- glm(data = full_data,
                  formula = is_fault ~ z_point_importance + 
                    is_wide + is_t + is_deuce_court +
                    is_break_point)
summary(test_model)

library(lme4)

test_mixed_model <- glmer(data = full_data,
                        formula = is_fault ~ point_importance + 
                          is_wide + is_t + is_deuce_court +
                          is_break_point + (1 | server_name),
                        family = 'binomial'
                  )
summary(test_mixed_model)
# -- Fit Hierarchical Model -----

model_matrix <- 
cbind(rep(1,nrow(full_data)),
as.matrix(full_data[, c('point_importance', 'is_wide', 'is_t', 'is_break_point')]))

stan_dat <- list(
  D = 4 + 1,
  N = nrow(full_data),
  L = length(unique(full_data$server_id)),
  y = full_data$is_fault,
  player_id = full_data$new_server_id,
  x = model_matrix
)

library(rstan) 
stan_hierarchical_model <- stan_model(file = './eda/logistic_hierarchical.stan')

fit_hier_model <- sampling(stan_hierarchical_model, 
                           data = stan_dat, 
                           iter = 1000)



# -- Try a more efficient sampler?
#remotes::install_github("stan-dev/cmdstanr")
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()

library(cmdstanr)
library(data.table)

model <- cmdstan_model('./eda/logistic_hierarchical.stan')	
fitted_model <- model$variational(
  data = stan_dat,
  tol_rel_obj = 0.001
)


b0_mean <- fitted_model$draws("beta")[,1:74] %>% colMeans()
b0_sd <-  fitted_model$draws("beta")[,1:74] %>% apply( 2, sd)

b1_mean <- fitted_model$draws("beta")[,75:(2*74)] %>% colMeans()
b1_sd <-  fitted_model$draws("beta")[,75:(2*74)] %>% apply( 2, sd)

b2_mean <- fitted_model$draws("beta")[,149:(3*74)] %>% colMeans()
b2_sd <-  fitted_model$draws("beta")[,149:(3*74)] %>% apply( 2, sd)

b3_mean <- fitted_model$draws("beta")[,223:(4*74)] %>% colMeans()
b3_sd <-  fitted_model$draws("beta")[,223:(4*74)] %>% apply( 2, sd)

b4_mean <- fitted_model$draws("beta")[,297:(5*74)] %>% colMeans()
b4_sd <-  fitted_model$draws("beta")[,297:(5*74)] %>% apply( 2, sd)

#b5_mean <- fitted_model$draws("beta")[,371:(6*74)] %>% colMeans()
#b5_sd <-  fitted_model$draws("beta")[,371:(6*74)] %>% apply( 2, sd)


effects_df <- data.frame(
  player_name = levels(as.factor(full_data$server_name)),
  intercept_mean = b0_mean,
  intercept_sd = b0_sd,
  point_importance_mean = b1_mean,
  point_importance_sd = b1_sd,
  is_wide_mean = b2_mean,
  is_wide_sd = b2_sd,
  is_t_mean = b3_mean,
  is_t_sd = b3_sd,
  is_break_point_mean = b4_mean,
  is_break_point_sd = b4_sd,
  stringsAsFactors = FALSE
  )

effects_df %>%
  arrange(is_break_point_mean) %>%
  View()



#### Conclusion:

## It seems like fault rates are highest when the ball is aimed "body".
# I suspect that net faults are likely to land in the body zone, and I question 
# whether the body was truly the intended targetted area...


# ### --- ### --- ### --- ### --- ### --- ###
# Plot serve directions on most important points -----
library(ggplot2)

training_data_with_importance$z_point_importance <- scale(training_data_with_importance$point_importance)

training_data_with_importance %>%
  filter(server_name == 'D.THIEM') %>%
  filter(serve_dir != '') %>%
  filter(z_point_importance > 1) %>%
  ggplot( aes(y = z_point_importance, fill = serve_dir)) +
  geom_boxplot(alpha= 0.4) +
  facet_grid(~serve_num + court_side)

player_name = 'N.DJOKOVIC'
player_name = 'R.NADAL'
player_name = 'R.FEDERER'
player_name = 'A.ZVEREV'
player_name = 'S.WAWRINKA'
training_data_with_importance %>%
  filter(server_name == player_name) %>%
  filter(serve_dir != '') %>%
  filter(z_point_importance > 1) %>%
  ggplot( aes(y = z_point_importance, fill = serve_dir)) +
  geom_boxplot(alpha= 0.4) +
  facet_grid(~serve_num + court_side)


# ### --- ### --- ### --- ### --- ### --- ###
# Plot serve directions on break points -----
player_name = 'N.DJOKOVIC'
player_name = 'R.NADAL'
player_name = 'R.FEDERER'
player_name = 'A.ZVEREV'
player_name = 'S.WAWRINKA'
training_data_with_importance %>%
  filter(server_name == player_name) %>%
  filter(serve_dir != '') %>%
  filter(is_break_point == 1) %>%
  group_by(serve_dir, serve_num) %>%
  summarise(count = n()) %>%
  ggplot( aes(y = count, x = serve_dir)) +
  geom_bar(stat="identity") +
  facet_grid(~serve_num ) +
  labs(title = 'Serve Directions on Break point')

