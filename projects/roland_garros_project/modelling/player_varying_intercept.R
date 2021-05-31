### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    PLOT MODEL 2 RESULTS (JUST PLAYER-VARYING INTERCEPTS)   =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Plot Player Intercepts (Wide vs T) for Model 2 (Not Model 4)
# -- Plot tranformed intercept from logit scale --> probability scale
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_project/")

library(dplyr)
library(rstan)
options(tibble.print_max = 30)
#library(reshape2)

# -- Fit STAN Model
#library(cmdstanr)
#library(data.table)


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
         player_hands_match, match_id
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


atp_stan_datalist2 <- list(
  N = nrow(atp_training_data),
  N_1 = length(unique(atp_training_data$server_index)),
  id_1 = atp_training_data$server_index,
  y = atp_training_data$y,
  K = 3
)

# -- Prediction with RSTAN -----
options(mc.cores = 4)
player_varying_intercept_model <- stan_model('./modelling/stan_files/predict_player_intercept.stan')

fit_atp_model <- sampling(player_varying_intercept_model, 
                          data = atp_stan_datalist2, 
                          iter = 2000)

extract_stan_model <- extract(fit_atp_model)
names(extract_stan_model)

dim(extract_stan_model$player_probs)
dim(extract_stan_model$player_preds)

dim(extract_stan_model$player_probs[,,1])

# -- Prob(T)
probT_df <- extract_stan_model$player_probs[,,1]
t_means <- colMeans(probT_df)
t_medians <- apply(probT_df, 2, median) 
t_sd <- apply(probT_df, 2, sd) 
t_2.5_quantile <- apply(probT_df, 2, quantile, probs=0.025)
t_97.5_quantile <- apply(probT_df, 2, quantile, probs=0.975)

# -- Prob(Wide)
probWide_df <- extract_stan_model$player_probs[,,2]
wide_mean <- colMeans(probWide_df)
wide_sd <- apply(probWide_df , 2, sd) 
wide_medians <- apply(probWide_df , 2, median) 
wide_2.5_quantile <- apply(probWide_df , 2, quantile, probs=0.025)
wide_97.5_quantile <- apply(probWide_df , 2, quantile, probs=0.975)

# -- Prob(Body)
probBody_df <- extract_stan_model$player_probs[,,3]
body_medians <- apply(probBody_df, 2, median) 
body_mean <- colMeans(probBody_df)
body_sd <- apply(probBody_df , 2, sd) 
body_2.5_quantile <- apply(probBody_df, 2, quantile, probs=0.025)
body_97.5_quantile <- apply(probBody_df, 2, quantile, probs=0.975)


atp_player_df <- data.frame(
  name = levels(factor(atp_training_data$server_name)),
  median_t = t_medians,
  mean_t = t_means,
  sd_t = t_sd,
  t_quant_2.5 = t_2.5_quantile,
  t_quant_97.5 = t_97.5_quantile,
  median_wide = wide_medians,
  mean_wide = wide_mean,
  sd_wide = wide_sd,
  wide_quant_2.5 = wide_2.5_quantile,
  wide_quant_97.5 = wide_97.5_quantile,
  median_body = body_medians,
  mean_body = body_mean,
  sd_body = body_sd,
  body_quant_2.5 = body_2.5_quantile,
  body_quant_97.5 = body_97.5_quantile,
  gender = 'Men'
)

player_num_matches <- 
  atp_training_data %>%
  group_by(server_name) %>%
  summarise(num_matches = length(unique(match_id)),
            num_serves = n())

atp_player_df <- atp_player_df %>%
  left_join(player_num_matches,
            by = c('name' = 'server_name'))



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
         player_hands_match, match_id
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

# wta_training_data %>%
#   filter(server_name == 'S.WILLIAMS') %>%
#   .$y %>%
#   table()
# wta_training_data %>%
#   filter(server_name == 'O.JABEUR') %>%
#   .$y %>%
#   table()

wta_stan_datalist2 <- list(
  N = nrow(wta_training_data),
  N_1 = length(unique(wta_training_data$server_index)),
  id_1 = wta_training_data$server_index,
  y = wta_training_data$y,
  K = 3
)

fit_wta_model <- sampling(player_varying_intercept_model, 
                          data = wta_stan_datalist2, 
                          iter = 2000)

extract_wta_stan_model <- extract(fit_wta_model)
# -- Prob(T)
wta_probT_df <- extract_wta_stan_model$player_probs[,,1]
wta_t_medians <- apply(wta_probT_df, 2, median) 
wta_t_mean <- colMeans(wta_probT_df)
wta_t_sd <- apply(wta_probT_df , 2, sd) 
wta_t_2.5_quantile <- apply(wta_probT_df, 2, quantile, probs=0.025)
wta_t_97.5_quantile <- apply(wta_probT_df, 2, quantile, probs=0.975)

# -- Prob(Wide)
wta_probWide_df <- extract_wta_stan_model$player_probs[,,2]
wta_wide_medians <- apply(wta_probWide_df , 2, median) 
wta_wide_mean <- colMeans(wta_probWide_df)
wta_wide_sd <- apply(wta_probWide_df , 2, sd)
wta_wide_2.5_quantile <- apply(wta_probWide_df , 2, quantile, probs=0.025)
wta_wide_97.5_quantile <- apply(wta_probWide_df , 2, quantile, probs=0.975)

# -- Prob(Body)
wta_probBody_df <- extract_wta_stan_model$player_probs[,,3]
wta_body_medians <- apply(wta_probBody_df, 2, median) 
wta_body_mean <- colMeans(wta_probBody_df)
wta_body_sd <- apply(wta_probBody_df , 2, sd)
wta_body_2.5_quantile <- apply(wta_probBody_df, 2, quantile, probs=0.025)
wta_body_97.5_quantile <- apply(wta_probBody_df, 2, quantile, probs=0.975)

wta_player_df <- data.frame(
  name = levels(factor(wta_training_data$server_name)),
  median_t = wta_t_medians,
  mean_t = wta_t_mean,
  sd_t = wta_t_sd,
  t_quant_2.5 = wta_t_2.5_quantile,
  t_quant_97.5 = wta_t_97.5_quantile,
  median_wide = wta_wide_medians,
  mean_wide = wta_wide_mean,
  sd_wide = wta_wide_sd,
  wide_quant_2.5 = wta_wide_2.5_quantile,
  wide_quant_97.5 = wta_wide_97.5_quantile,
  median_body = wta_body_medians,
  mean_body = wta_body_mean,
  sd_body = wta_body_sd,
  body_quant_2.5 = wta_body_2.5_quantile,
  body_quant_97.5 = wta_body_97.5_quantile,
  gender = 'Women'
)

wta_player_num_matches <- 
  wta_training_data %>%
  group_by(server_name) %>%
  summarise(num_matches = length(unique(match_id)),
            num_serves = n())

wta_player_df <- wta_player_df %>%
  left_join(wta_player_num_matches,
            by = c('name' = 'server_name'))

wta_player_df %>%
  arrange(desc(mean_wide)) %>% View()

wta_player_df %>%
  arrange(desc(mean_body)) %>% View()



# -- Save Data
plot_data <- rbind(atp_player_df, wta_player_df)
write.csv(plot_data, './modelling/compare_intercepts_probs/plot_player_probs_dataframe.csv', row.names = FALSE)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    PLOT BOTH MEN AND WOMEN PROBABILITIES          =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
library(ggplot2)
library(ggrepel)
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/ggplot_theme.R")
plot_data <- read.csv('./modelling/compare_intercepts_probs/plot_player_probs_dataframe.csv')

wta_plot_data <- plot_data %>%
  filter(gender == 'Women') #%>%
  #filter(num_serves>100)

wta_plot_data %>%
  arrange(desc(mean_t)) %>%
  View()

wta_labels <- c('O.JABEUR', 'K.SINIAKOVA', 'N.OSAKA',
                'M.VONDROUSOVA', 'S.SORRIBESTORMO', 'J.KONTA',
                'P.KVITOVA', 'S.KENIN', 'S.WILLIAMS',
                'I.SWIATEK')

ggplot(wta_plot_data,
       aes(x = mean_t, y = mean_wide)) +
  geom_abline(intercept = 0, slope = 1, size = 0.25,linetype='dashed') +
  geom_point(shape = 21,
             size = 2,
             alpha = 0.75,
             fill = '#EC7063') +
  geom_point(data = wta_plot_data  %>%
               filter(name %in% wta_labels), #%>%
             #filter(num_serves>100)
             aes(x = mean_t, y = mean_wide),
             fill = '#ffeda5',
             shape = 21,
             size = 2.5) +
  # WILLIAMS
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='S.WILLIAMS'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0,
                   nudge_x = 0.02,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='O.JABEUR'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0.01,
                   nudge_x = 0.0,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='K.SINIAKOVA'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0.01,
                   nudge_x = 0.0,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) + 
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='N.OSAKA'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = -0.01,
                   nudge_x = 0.04,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='M.VONDROUSOVA'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0.0,
                   nudge_x = -0.02,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='S.SORRIBESTORMO'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0.0,
                   nudge_x = 0.02,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) + 
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='J.KONTA'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0.0,
                   nudge_x = 0.02,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) + 
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='S.KENIN'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0.0,
                   nudge_x = 0.03,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='P.KVITOVA'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0.01,
                   nudge_x = 0.0,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = wta_plot_data %>%
                     filter(name =='I.SWIATEK'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   alpha = 0.8,
                   fill = '#EC7063',
                   nudge_y = -0.03,
                   nudge_x = 0.01,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  labs(#title = 'Women Estimated Serve Direction Probabilities',
       x='Mean Prob(T)',
       y = 'Mean Prob(Wide)'#,
       #caption="Roland Garros 2019-20"
  ) +
  ylim(0.2, 0.55) + 
  xlim(0.2, 0.56) +
  #scale_y_continuous(breaks = c(0.2, 0.3, 0.4, 0.5))+
  #scale_x_continuous(breaks = c(0.2, 0.3, 0.4, 0.5))+
  peter_theme(family_font = 'Tahoma') 

ggsave('./modelling/plots/wta_comparing_player_probs.jpg',
       width=6, height=4,
       dpi = 280)

# -- ATP
atp_plot_data <- plot_data %>%
  filter(gender == 'Men') #%>%
  #filter(num_serves>150)


atp_plot_data %>%
  arrange(desc(mean_t)) %>%
  View()

atp_labels <- c('D.SHAPOVALOV', 'D.THIEM', #'S.WAWRINKA',
                'R.FEDERER', 'N.DJOKOVIC', 'R.NADAL',
                'J.DELPOTRO', 'A.RUBLEV', 'A.ZVEREV',
                'A.MURRAY'#, 'J.SOCK' #'S.TSITSIPAS'
                )

ggplot(data = atp_plot_data,
       aes(x = mean_t, y = mean_wide)) +
  geom_abline(intercept = 0, slope = 1, size = 0.25,linetype='dashed') +
  geom_point(shape = 21,
             size = 2,
             alpha = 0.75,
             fill = '#5DADE2') +
  geom_point(data = atp_plot_data %>%
               filter(name %in% atp_labels),
             aes(x = mean_t, y = mean_wide),
             fill = '#ffeda5',
             shape = 21,
             size = 2.5) +
  geom_label_repel(data = atp_plot_data %>%
                     filter(name =='R.FEDERER'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = 0.0,
                   nudge_x = 0.04,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = atp_plot_data %>%
                     filter(name =='R.NADAL'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = -0.035,
                   nudge_x = -0.015,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = atp_plot_data %>%
                     filter(name =='N.DJOKOVIC'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = 0.03,
                   nudge_x = 0.035,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = atp_plot_data %>%
                     filter(name =='D.SHAPOVALOV'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = 0.023,
                   nudge_x = -0.04,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = atp_plot_data %>%
                     filter(name =='D.THIEM'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = 0.022,
                   nudge_x = 0.03,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = atp_plot_data %>%
                     filter(name =='A.ZVEREV'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = -0.032,
                   nudge_x = 0.0,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = atp_plot_data %>%
                     filter(name =='J.DELPOTRO'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = -0.01,
                   nudge_x = 0.04,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  # geom_label_repel(data = atp_plot_data %>%
  #                    filter(name =='S.WAWRINKA'),
  #                  aes(y = mean_wide,
  #                      x = mean_t,
  #                      label = name),
  #                  fill = '#5DADE2',
  #                  #alpha = 0.8,
  #                  nudge_y = 0.02,
  #                  nudge_x = 0.02,
  #                  fontface = 'bold',
  #                  size = 2.3,
  #                  show.legend = FALSE,
  #                  arrow = arrow(ends ='last',
  #                                type = 'closed',
  #                                length = unit(0.015, "npc")),
  #                  point.padding = unit(0.5, "lines"),
  #                  label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = atp_plot_data %>%
                     filter(name =='A.RUBLEV'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = 0,
                   nudge_x = 0.04,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  geom_label_repel(data = atp_plot_data %>%
                     filter(name =='A.MURRAY'),
                   aes(y = mean_wide,
                       x = mean_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = 0.00,
                   nudge_x = 0.04,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")) +
  #geom_label_repel(data = atp_plot_data %>%
  #                    filter(name =='S.TSITSIPAS'),
  #                  aes(y = mean_wide,
  #                      x = mean_t,
  #                      label = name),
  #                  alpha=0.8,
  #                  fill = '#5DADE2',
  #                  nudge_y = 0.008,
  #                  nudge_x = -0.02,
  #                  fontface = 'bold',
  #                  size = 2.3,
  #                  show.legend = FALSE,
  #                  arrow = arrow(ends ='last',
  #                                type = 'closed',
  #                                length = unit(0.015, "npc")),
  #                  point.padding = unit(0.5, "lines"),
  #                  label.padding = unit(0.25, "lines")) +
  labs(#title = 'Men Estimated Serve Direction Probabilities',
       x='Mean Prob(T)',
       y = 'Mean Prob(Wide)'#,
       #caption="Roland Garros 2019-20"
  ) +
  ylim(0.2, 0.55) + 
  xlim(0.2, 0.56) +
  peter_theme(family_font = 'Tahoma') 
  #scale_y_continuous(breaks=seq(0.2,0.5,0.1))+
  #scale_x_continuous(breaks = c(0.2, 0.3, 0.4, 0.5))


ggsave('./modelling/plots/atp_comparing_player_probs.jpg',
       width=6, height=4,
       dpi = 280)


# -- Everything on one plot

player_labels <- c(
  'O.JABEUR', 'K.SINIAKOVA', 'N.OSAKA',
  'M.VONDROUSOVA', 'S.SORRIBESTORMO', 'J.KONTA',
  'P.KVITOVA', 'S.KENIN', 'S.WILLIAMS',
  'I.SWIATEK', 'D.SHAPOVALOV', 'D.THIEM', 'S.WAWRINKA',
  'R.FEDERER', 'N.DJOKOVIC', 'R.NADAL',
  'J.DELPOTRO', 'A.RUBLEV', 'A.ZVEREV',
  'A.MURRAY'
  
)
ggplot(data = plot_data,
       aes(x = mean_t, y = mean_wide)) +
  geom_abline(intercept = 0, slope = 1, size = 0.25,linetype='dashed') +
  geom_point(shape = 21,
             size = 2,
             alpha = 0.7,
             aes(fill = gender)) +
  geom_point(data = plot_data %>%
               filter(name %in% player_labels),
             aes(x = mean_t, y = mean_wide),
             fill = '#ffeda5',
             shape = 21,
             size = 2.5) +
  peter_theme() +
  ylim(0.2, 0.55) + 
  xlim(0.2, 0.56) 

