#### -- How predictible is point importance?

library(dplyr)
library(ggplot2)
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/src/ggplot_theme.R")
source('src/gg_tennis_court.R')


atp_pbp_df <- read.csv('./collect_data/data/atp_processed_roland_garros_tracking_data.csv',
                       na.strings=c("","NA"))


# -- As prototype, fit model using Thiem's serves
player_name <- 'D.THIEM'
player_name <- 'N.DJOKOVIC'
player_name <- 'A.ZVEREV'
player_name <- 'R.NADAL'
player_name <- 'R.FEDERER'

# -- Code Body serves to be the baseline
thiem_df <- atp_pbp_df %>%
  filter(server_name == player_name) %>%
  select(point_ID, server_name, returner_name,
         server_hand, returner_hand,
         serve_num, court_side, serve_impact_from_center,
         z_ball_at_serve, x_ball_at_serve,
         is_break_point, is_prev_doublefault, is_prev_ace,
         prev_intended_serve_loc1, prev_intended_serve_loc2,
         point_importance, z_scaled_point_importance,
         minmax_scaled_point_importance, intended_serve_dir,
         player_hands_match, returner_backhand_loc
         
         ) %>%
  #filter(server_name == player_name) %>%
  mutate(
    # intended_serve_dir = ifelse(intended_serve_dir == 'Body', 1, 
    #                                  ifelse(intended_serve_dir == 'T', 2, 
    #                                         ifelse(intended_serve_dir == 'Wide', 3, NA))),
         
         is_court_ad = ifelse(court_side =='DeuceCourt', 0, 1),
         is_first_serve = ifelse(serve_num == 2, 0 , serve_num),
         is_server_left_handed = ifelse(server_hand == 'left-handed', 1, 0)
         
         # -- Variables that don't seem to be significant
         
         #second_serve_df = ifelse( (serve_num == 2) & (is_prev_doublefault==1), 1,0)
  )   %>%
  filter(complete.cases(.)) 


# -- Cut point importance into a categorical variable
summary(thiem_df$point_importance)

thiem_df$cat_point_importance <- ifelse(thiem_df$point_importance >= quantile(thiem_df$point_importance, 0.8),
       'high','low')



# -- Plot of Serve Direction Proportions stratified by point importance & Fill by Court Side 
# ***** (1st Serve) *****
thiem_count_df <- thiem_df %>%
  #filter(serve_dir != '') %>%
  filter(serve_num==2) %>%
  group_by(cat_point_importance, court_side, intended_serve_dir) %>%
  summarise(count = n()) %>%
  left_join(thiem_df %>%
              #filter(serve_dir != '') %>%
              filter(serve_num==2) %>%
              group_by(cat_point_importance, court_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves)


ggplot(data=thiem_count_df , 
       aes(x=intended_serve_dir, y=prop, fill = cat_point_importance)) +
  facet_wrap(~court_side) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  peter_theme(family_font = 'Tahoma') + 
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  labs(title = paste(player_name, 'Serve Directions\non 2nd Serve'),
       x='',
       y = 'Proportion of Serves',
       fill = "Point Importance",
       caption=""
  ) 

ggsave('thiem_point_imp.jpg',
       width=7, height=5,
       dpi = 400)


# ggsave('FED_point_imp.jpg',
#        width=7, height=5,
#        dpi = 400)


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----         Heatmaps on Important Points                    -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

player_names <- c('R.FEDERER', 'N.DJOKOVIC', 'R.NADAL')

df_list <- list()
for (i in 1:length(player_names)){
  player_name <- player_names[i]
  df <- atp_pbp_df %>%
    filter(server_name == player_name) %>%
    # select(point_ID, server_name, returner_name,
    #        server_hand, returner_hand,
    #        serve_num, court_side,
    #        point_importance, z_scaled_point_importance,
    #        minmax_scaled_point_importance, intended_serve_dir,
    #        which_side, intended_serve_bounce_x, intended_serve_bounce_y
    #        
    # ) %>%
    mutate( x_coord = ifelse( (which_side == 'right'), 
                              -1 *intended_serve_bounce_x,
                              intended_serve_bounce_x),
            
            y_coord = ifelse( (which_side == 'right'), 
                              -1 *intended_serve_bounce_y,
                              intended_serve_bounce_y)) %>%
    filter(complete.cases(.))
  
  # -- Cut point importance into a categorical variable
  df$cat_point_importance <- ifelse(df$point_importance >= quantile(df$point_importance, 0.8),
                                          'High','Low')
  
  df_list[[i]] <- df
  
  }

 
importance_df <- do.call(rbind, df_list)

importance_df$court_side <- factor(importance_df$court_side,
                                      levels = c('DeuceCourt', 'AdCourt'))

courtside.labs <- c("Ad. Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")

cat_point_importance.labs <- c("High Importance", " Low Importance")
names(cat_point_importance.labs) <- c("High", "Low")

importance_df %>%
  filter(server_name == 'R.FEDERER') %>%
  group_by(cat_point_importance) %>%
  summarise(n())
  

importance_df %>%
  filter(server_name == 'R.FEDERER') %>%
  distinct() %>%
  #filter(court_side == 'AdCourt') %>%
  filter(x_coord > -2) %>%
  filter(x_coord < 11) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  facet_grid(court_side ~ cat_point_importance,
             labeller = labeller(court_side = courtside.labs,
                                 cat_point_importance = cat_point_importance.labs)) + 

  labs(x = "", 
       y = "",
       title = "Federer Adjusting Serve Direction on Important Points?",
       caption ='Data: Roland Garros 2019-20'
       )

ggsave('./eda/plots/fed_adjusting_serve_imp.jpg',
       width=6, height=4,
       dpi = 250)

importance_df %>%
  filter(server_name == 'R.FEDERER') %>%
  #distinct() %>%
  nrow()

importance_df %>%
  filter(server_name == 'N.DJOKOVIC') %>%
  distinct() %>%
  nrow()


importance_df %>%
  filter(server_name == 'N.DJOKOVIC') %>%
  #filter(court_side == 'AdCourt') %>%
  filter(x_coord > -2) %>%
  filter(x_coord < 11) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  facet_grid(court_side ~ cat_point_importance,
             labeller = labeller(court_side = courtside.labs,
                                 cat_point_importance = cat_point_importance.labs)) + 
  labs(x = "", 
       y = "",
       title = "Djokovic's Consistent Serve Direction on Important Points",
       caption ='Data: Roland Garros 2019-20'
       )

ggsave('./eda/plots/djokovic_adjusting_serve_imp.jpg',
       width=6, height=4,
       dpi = 250)



# -- Boxplot of point importance
plot_fed_djoko <- 
importance_df %>%
  filter(server_name %in% c('R.FEDERER', 'N.DJOKOVIC')) 

plot_fed_djoko$server_name <- factor(plot_fed_djoko$server_name)


plot_fed_djoko %>%
  filter(server_name == 'R.FEDERER') %>%
  select(point_importance) %>%
  .$point_importance %>%
  quantile(0.8)

plot_fed_djoko %>%
  filter(server_name == 'N.DJOKOVIC') %>%
  select(point_importance) %>%
  .$point_importance %>%
  quantile(0.8)


plot_fed_djoko %>%
  ggplot(aes(x = server_name, y = point_importance, fill = server_name)) +
  # geom_jitter(color = 'blue',
  #             fill = 'black',
  #             alpha = 0.15) +
  geom_boxplot(position = position_dodge(0.9),
               alpha = 0.75,
               color = '#6c7a86',
               #fill = 'blue'
               ) +
  geom_hline(yintercept = 0.05610644, linetype = 'dashed', color = 'coral') +
  geom_hline(yintercept = 0.05341246, linetype = 'dashed', color = '#00BFC4') +
  peter_theme(family_font = 'Tahoma') + 
  annotate("text", x = 2.35, y = 0.07, label = "80th Percentile",
           colour = 'black', fontface =1, size = 3.5) +
  annotate("rect", xmin = 2.1, 
           xmax = 2.6, 
           ymin = 0.055,
           ymax = 0.085,
           alpha = .1) + 
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text.x = element_text(size = 10),
        legend.position = "none") +
  labs(y = 'Point Importance',
       x = ''
       #caption = 'Data: Roland Garros 2019-20'
       )


ggsave('./eda/plots/point_imp_boxplot.jpg',
       width=5.5, height=3.5,
       dpi = 250)

