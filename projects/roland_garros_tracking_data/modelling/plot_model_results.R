### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    PLOT MODEL RESULTS          =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")
library(dplyr)
library(rstan)

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



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    (ATP) MODEL RESULTS   =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
stanfit4 <- readRDS("./modelling/saved_models/stan_loglik/atp_loglik_fit4.RDS")
#stanfit2 <- readRDS("./modelling/saved_models/stan_loglik/atp_loglik_fit2.RDS")

extract_stan_model <- extract(stanfit4)
#extract_stan_model <- extract(stanfit2)
names(extract_stan_model)

player_varying_intercepts <- extract_stan_model$v_id1
player_intercepts <- extract_stan_model$B_0

t_player_varying_intercepts <- player_varying_intercepts[,,1] + player_intercepts[,1]
wide_player_varying_intercepts <- player_varying_intercepts[,,2] +  player_intercepts[,2]

t_medians <- apply(t_player_varying_intercepts, 2, median) 
wide_medians <- apply(wide_player_varying_intercepts, 2, median) 


# quantiles
t_2.5_quantile <- apply(t_player_varying_intercepts, 2, quantile, probs=0.025)
t_97.5_quantile <- apply(t_player_varying_intercepts, 2, quantile, probs=0.975)
wide_2.5_quantile <- apply(wide_player_varying_intercepts, 2, quantile, probs=0.025)
wide_97.5_quantile <- apply(wide_player_varying_intercepts, 2, quantile, probs=0.975)

#levels(factor(atp_training_data$server_name))

atp_player_df <- data.frame(
  name = levels(factor(atp_training_data$server_name)),
  median_t = t_medians,
  t_quant_2.5 = t_2.5_quantile,
  t_quant_97.5 = t_97.5_quantile,
  median_wide = wide_medians,
  wide_quant_2.5 = wide_2.5_quantile,
  wide_quant_97.5 = wide_97.5_quantile,
  gender = 'Men'
  )

player_num_matches <- 
atp_data %>%
  group_by(server_name) %>%
  summarise(num_matches = length(unique(match_id)))

atp_player_df <- 
atp_player_df %>%
  left_join(player_num_matches,
            by = c('name' = 'server_name'))


atp_player_df %>%
  arrange(desc(median_wide)) %>%
  filter(num_matches>=5) %>%
  View()


# -- Covariance parameters
atp_sd_params <- extract_stan_model$sigma_id1
atp_sd_median <- apply(atp_sd_params, 2, median) 
atp_sd_2.5_quantile <- apply(atp_sd_params, 2, quantile, probs=0.025)
atp_sd_97.5_quantile <- apply(atp_sd_params, 2, quantile, probs=0.975)

atp_sd_median 
atp_sd_2.5_quantile
atp_sd_97.5_quantile


atp_rho_param <- extract_stan_model$Rho_id1
atp_rho_params <- atp_rho_param[,2,1]
atp_rho_median <- median(atp_rho_params)
atp_rho_2.5_quantile <- quantile(atp_rho_params, probs = 0.025)
atp_rho_97.5_quantile <- quantile(atp_rho_params, probs = 0.975)

atp_rho_2.5_quantile
atp_rho_median
atp_rho_97.5_quantile

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    WTA MODEL RESULTS          =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- WTA
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



wta_stanfit4 <- readRDS("./modelling/saved_models/stan_loglik/wta_loglik_fit4.RDS")
wta_extract_stan_model <- extract(wta_stanfit4)

wta_player_varying_intercepts <- wta_extract_stan_model$v_id1
wta_player_intercepts <- wta_extract_stan_model$B_0

t_wta_player_varying_intercepts <- wta_player_varying_intercepts[,,1] + wta_player_intercepts[,1]
wide_wta_player_varying_intercepts <- wta_player_varying_intercepts[,,2] +  wta_player_intercepts[,2]


wta_t_medians <- apply(t_wta_player_varying_intercepts, 2, median) 
wta_wide_medians <- apply(wide_wta_player_varying_intercepts, 2, median) 

# quantiles
wta_t_2.5_quantile <- apply(t_wta_player_varying_intercepts, 2, quantile, probs=0.025)
wta_t_97.5_quantile <- apply(t_wta_player_varying_intercepts, 2, quantile, probs=0.975)
wta_wide_2.5_quantile <- apply(wide_wta_player_varying_intercepts, 2, quantile, probs=0.025)
wta_wide_97.5_quantile <- apply(wide_wta_player_varying_intercepts, 2, quantile, probs=0.975)

#levels(factor(wta_training_data$server_name))


wta_player_df <- data.frame(
  name = levels(factor(wta_training_data$server_name)),
  median_t = wta_t_medians,
  t_quant_2.5 = wta_t_2.5_quantile,
  t_quant_97.5 = wta_t_97.5_quantile,
  median_wide = wta_wide_medians,
  wide_quant_2.5 = wta_wide_2.5_quantile,
  wide_quant_97.5 = wta_wide_97.5_quantile,
  gender = 'Women'
  )

wta_player_num_matches <- 
  wta_data %>%
  group_by(server_name) %>%
  summarise(num_matches = length(unique(match_id)))

wta_player_df <- 
  wta_player_df %>%
  left_join(wta_player_num_matches,
            by = c('name' = 'server_name'))


wta_player_df %>%
  arrange(desc(median_wide)) %>%
  filter(num_matches>=4) %>%
  View()

# -- Covariance parameters
wta_sd_params <- wta_extract_stan_model$sigma_id1
wta_sd_median <- apply(wta_sd_params, 2, median) 
wta_sd_2.5_quantile <- apply(wta_sd_params, 2, quantile, probs=0.025)
wta_sd_97.5_quantile <- apply(wta_sd_params, 2, quantile, probs=0.975)

wta_sd_2.5_quantile
wta_sd_median 
wta_sd_97.5_quantile


wta_rho_param <- wta_extract_stan_model$Rho_id1
wta_rho_params <- wta_rho_param[,2,1]
wta_rho_median <- median(wta_rho_params)
wta_rho_2.5_quantile <- quantile(wta_rho_params, probs = 0.025)
wta_rho_97.5_quantile <- quantile(wta_rho_params, probs = 0.975)

wta_rho_2.5_quantile
wta_rho_median
wta_rho_97.5_quantile





### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    PLOT BOTH MEN AND WOMEN INTERCEPTS          =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#plot_data <- rbind(atp_player_df, wta_player_df)
#write.csv(plot_data, 'plot_median_intercepts_dataframe.csv', row.names = FALSE)
library(ggrepel)
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/src/ggplot_theme.R")
plot_data <- read.csv('./modelling/plot_median_intercepts_dataframe.csv')

player_labels <- c('R.FEDERER', 'N.DJOKOVIC', 'R.NADAL',
                   'S.KENIN', 'S.WILLIAMS', 'I.SWIATEK')
#S.HALEP
ggplot(data = plot_data #%>% filter(num_matches>1),
       ,aes(y = median_t, x = median_wide)) +
  geom_point(aes(fill = as.factor(gender)),
             shape = 21,
             size = 2.5,
             alpha = 0.75) +
  geom_point(data = plot_data %>% filter(name %in% player_labels),
             fill = '#ff8c00',
             shape = 21,
             size = 2.8) +
  # -- Change legend color scale
  scale_fill_manual(values = c('#5DADE2', '#EC7063'), 
                    c('Men', 'Women'), name = '') +
  # Federer
  geom_label_repel(data = plot_data %>%
                     filter(name =='R.FEDERER'),
                   aes(x = median_wide,
                       y = median_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = 0,
                   nudge_x = -0.75,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  # 
  geom_label_repel(data = plot_data %>%
                     filter(name =='R.NADAL'),
                   aes(x = median_wide,
                       y = median_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = -0.5,
                   nudge_x = 0.1,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  geom_label_repel(data = plot_data %>%
                     filter(name =='N.DJOKOVIC'),
                   aes(x = median_wide,
                       y = median_t,
                       label = name),
                   fill = '#5DADE2',
                   nudge_y = -0.5,
                   nudge_x = 0.1,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  geom_label_repel(data = plot_data %>%
                     filter(name =='S.WILLIAMS'),
                   aes(x = median_wide,
                       y = median_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0,
                   nudge_x = -0.6,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  geom_label_repel(data = plot_data %>%
                     filter(name =='S.KENIN'),
                   aes(x = median_wide,
                       y = median_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0.1,
                   nudge_x = -0.5,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  geom_label_repel(data = plot_data %>%
                     filter(name =='I.SWIATEK'),
                   aes(x = median_wide,
                       y = median_t,
                       label = name),
                   fill = '#EC7063',
                   nudge_y = 0.6,
                   nudge_x = 0,
                   fontface = 'bold',
                   size = 2.3,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  geom_vline(xintercept = 0, size = 0.5,linetype='dashed') +
  geom_hline(yintercept = 0, size = 0.5, linetype='dashed') +
  labs(#title = 'Comparing Player-Varying Intercepts',
       x='Median Preference Serving\nWide over Body',
       y = 'Median Preference Serving\nT over Body',
       fill = ""#,
       #caption="."
  ) +
  peter_theme(family_font = 'Tahoma') 


ggsave('./modelling/comparing_player_intercepts2.jpg',
       width=5.5, height=4,
       dpi = 280)

plot_data %>%
  group_by(name) %>%
  summarise(distance = sum(median_t^2 + median_wide^2)) %>%
  arrange(desc(distance)) %>%
  View()
  




