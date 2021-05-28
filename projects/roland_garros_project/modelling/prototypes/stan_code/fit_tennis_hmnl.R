### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====     Fit Hierarchical MNL on tennis pbp in STAN        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
library(dplyr)
setwd("~/tennis_analytics/projects/roland_garros_tracking_data/stan_code/")

source('./src/helper_functions.R')
atp_pbp_df <- read.csv('atp_pbp_df.csv')

colnames(atp_pbp_df)

# -- Add ace or double fault identifier column
table(atp_pbp_df$point_end_type)

atp_pbp_df <- atp_pbp_df %>%
  mutate(is_ace = ifelse(point_end_type == 'Ace', 1, 0),
         is_doublefault = ifelse(point_end_type == 'DoubleFault', 1, 0)
         )

# -- This only works on the 1st serve... maybe should add on 2nd serve too?
# -- Should apply this code per match too (what if a match ends with an ace or double fault?)
is_prev_ace <- c(0,atp_pbp_df$is_ace[1:(nrow(atp_pbp_df)-1)])
is_prev_doublefault <- c(0,atp_pbp_df$is_doublefault[1:(nrow(atp_pbp_df)-1)])

atp_pbp_df$is_prev_ace <- is_prev_ace
atp_pbp_df$is_prev_doublefault <- is_prev_doublefault

is_prev_ace_id <- which(atp_pbp_df$is_prev_ace == 1)

is_second_serve_id <- which(atp_pbp_df$serve_num == 2)
  
sum(is_second_serve_id == is_prev_ace_id)
  
  
atp_pbp_df %>%
  mutate(is_prev_ace = ifelse(serve_num==2 ))


sort(table(atp_pbp_df$server_name))


# # # # # # # # # # # # # # 
# -- Adding serve type?
# # # # # # # # # # # # # # 

# -- Some serve types are missing / mostly Unclassified
sort(table(atp_pbp_df$serve_type))
sum(is.na(atp_pbp_df$serve_type))

atp_pbp_df %>%
  group_by(serve_type, serve_num) %>%
  summarise(count = n())

atp_pbp_df %>%
  group_by(serve_dir, serve_num) %>%
  summarise(count = n())

player_name <- 'D.THIEM'

# -- Careful about missing data
atp_pbp_df %>%
  filter(server_name == player_name) %>%
  filter(!serve_dir %in% c('Body', 'Wide', 'T')) %>% View()

player_serves <- atp_pbp_df %>%
  select(server_name, returner_name, serve_side, serve_num, serve_speed_kph, is_break_point, serve_dir, year) %>%
  filter(server_name == player_name) %>%
  mutate(serve_dir = ifelse(serve_dir == 'Body', 3, 
                            ifelse(serve_dir == 'T', 1, 
                                   ifelse(serve_dir == 'Wide', 2, NA))),
         
         is_court_ad = ifelse(serve_side =='Deuce', 0, 1),
         is_bp = ifelse(is_break_point, 1,0),
         is_first_serve = ifelse(serve_num == 2, 0 , serve_num),
         serve_speed = as.numeric(gsub("([0-9]+).*$", "\\1", serve_speed_kph)),
         match_identifier = paste(returner_name, year, sep = '_'),
         match_id_num = as.numeric(as.factor(match_identifier))
         )   %>%
  select(server_name, returner_name, is_court_ad, is_bp,
         is_first_serve, serve_speed, serve_dir,match_id_num, match_identifier) %>%
  filter(complete.cases(.))

# -- Data Dictionary:
# serve_dir: 1 = T; 2 = Wide; 3 = Body


# -- Data check
table(player_serves$serve_dir)

table(player_serves$match_identifier)
table(player_serves$match_id_num)

mosaicplot(~ serve_dir + is_court_ad, data=player_serves)
mosaicplot(~ serve_dir + is_first_serve, data=player_serves)
mosaicplot(~ serve_dir + is_bp, data=player_serves)
# -- Fit MNL Hierarchical Model
hier_mnl <- stan_model(file = 'tennis_hmnl.stan')

stan_dat <- list(
  N = nrow(player_serves),
  tot_match_num = length(unique(player_serves$match_id_num)),
  y = player_serves$serve_dir,
  match_id = player_serves$match_id_num,
  x1 = player_serves$is_court_ad,
  x2 = player_serves$is_bp,
  x3 = player_serves$is_first_serve,
  K = 3
)

fit_hier_mnl <- sampling(hier_mnl, data = stan_dat, iter = 1000)

# -- Summary diagnostics of MCMC
#summary(fit_hier_mnl)$summary

# -- Look at Fixed effects
plot(fit_hier_mnl, plotfun="trace", pars = c('B_0', 'B_1', 'B_2', 'B_3'))
plot(fit_hier_mnl, plotfun="hist", pars = c('B_0', 'B_1', 'B_2', 'B_3'))

# -- Look at Random Effects
plot(fit_hier_mnl, plotfun="dens", pars = c('v_id1'))

# Get posterior means & medians
names(extract(fit_hier_mnl))
extract_elements <- extract(fit_hier_mnl)


# -- Look at training error
pred_y_train <- extract(fit_hier_mnl)$pred_y_train

stan_pred <- apply(MARGIN = 2,
                   X = pred_y_train,
                   FUN = get_column_prediction)


cbind(stan_pred, player_serves$serve_dir)

mean((ifelse(stan_pred == player_serves$serve_dir, 0,1)))

table(stan_pred, player_serves$serve_dir)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====     Fit a simple MNL for performance benchmarks        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
mnl_nnet_fit <- nnet::multinom(serve_dir ~ is_court_ad + is_bp + is_first_serve, data = player_serves)
summary(mnl_nnet_fit)


# Misclassification Errors
pred.class <- predict(mnl_nnet_fit, newdata=player_serves)

(mul.misclass <- mean(ifelse(pred.class == player_serves$serve_dir, yes=0, no=1)))
