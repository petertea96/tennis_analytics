### For a given player, plot their serve direction patterns (with barcharts)

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

atp_pbp_df <- read.csv('./atp_processed_roland_garros_tracking_data.csv')


#player_name <- 'R.NADAL'
#player_name <- 'N.DJOKOVIC'
#player_name <- 'R.FEDERER'
#player_name <- 'D.THIEM'
#player_name <- 'S.TSITSIPAS'
player_name <- 'A.ZVEREV'

atp_to_plot <- atp_pbp_df %>%
  select(serveBounceCordinate_y, serve_dir, court_side, serve_num,
         fault_distance_missed_m, point_num, game_num, set_num, year, 
         server_name, returner_name, point_end_type) %>%
  filter(server_name == player_name)



atp_to_plot$serve_dir <- factor(atp_to_plot$serve_dir,
                                        levels = c('T', 'Body', 'Wide',''))


# -- Plot of Serve Direction Proportions stratified by match & Fill by Court Side 
# ***** (1st Serve) *****
atp_plot_count_df <- atp_to_plot %>%
  filter(serve_dir != '') %>%
  filter(serve_num==1) %>%
  group_by(year, returner_name, serve_dir, court_side) %>%
  summarise(count = n()) %>%
  left_join(atp_to_plot %>%
              filter(serve_dir != '') %>%
              filter(serve_num==1) %>%
              group_by(year, returner_name, court_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))


ggplot(data=atp_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = court_side)) +
  facet_wrap(~match_lab) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = paste(player_name, '1st Serve RG Serve Directions'),
       x='',
       y = 'Proportion of Serves',
       fill = "Court Side",
       caption=""
  ) 


# -- Plot of Serve Direction Proportions stratified by match & Fill by Court Side 
# ***** (2nd Serve) *****

atp_plot_count_df <- atp_to_plot %>%
  filter(serve_dir != '') %>%
  filter(serve_num==2) %>%
  group_by(year, returner_name, serve_dir, court_side) %>%
  summarise(count = n()) %>%
  left_join(atp_to_plot %>%
              filter(serve_dir != '') %>%
              filter(serve_num==2) %>%
              group_by(year, returner_name, court_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))


ggplot(data=atp_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = court_side)) +
  facet_wrap(~match_lab) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = paste(player_name,'2nd Serve RG Serve Directions'),
       x='',
       y = 'Proportion of Serves',
       fill = "Court Side",
       caption=""
  ) 




##### Do the Same for WTA Players -----
wta_pbp_df <- read.csv('./wta_processed_roland_garros_tracking_data.csv')


player_name <- 'S.KENIN'
# player_name <- 'S.HALEP'
# player_name <- 'C.GARCIA'
# player_name <- 'E.SVITOLINA'
player_name <- 'P.KVITOVA'

wta_to_plot <- wta_pbp_df %>%
  select(serveBounceCordinate_y, serve_dir, court_side, serve_num,
         fault_distance_missed_m, point_num, game_num, set_num, year, 
         server_name, returner_name ) %>%
  filter(server_name == player_name)

wta_to_plot$serve_dir <- factor(wta_to_plot$serve_dir,
                                levels = c('T', 'Body', 'Wide',''))


# -- Plot of Serve Direction Proportions stratified by match & Fill by Court Side 
# ***** (1st Serve) *****
wta_plot_count_df <- wta_to_plot %>%
  filter(serve_dir != '') %>%
  filter(serve_num==1) %>%
  group_by(year, returner_name, serve_dir, court_side) %>%
  summarise(count = n()) %>%
  left_join(wta_to_plot %>%
              filter(serve_dir != '') %>%
              filter(serve_num==1) %>%
              group_by(year, returner_name, court_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))


ggplot(data=wta_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = court_side)) +
  facet_wrap(~match_lab) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = paste(player_name, '1st Serve RG Serve Directions'),
       x='',
       y = 'Proportion of Serves',
       fill = "Serve Direction",
       caption=""
  ) 


# -- Plot of Serve Direction Proportions stratified by match & Fill by Court Side 
# ***** (2nd Serve) *****

wta_plot_count_df <- wta_to_plot %>%
  filter(serve_dir != '') %>%
  filter(serve_num==2) %>%
  group_by(year, returner_name, serve_dir, court_side) %>%
  summarise(count = n()) %>%
  left_join(wta_to_plot %>%
              filter(serve_dir != '') %>%
              filter(serve_num==2) %>%
              group_by(year, returner_name, court_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))


ggplot(data=wta_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = court_side)) +
  facet_wrap(~match_lab) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = paste(player_name,'2nd Serve RG Serve Directions'),
       x='',
       y = 'Proportion of Serves',
       fill = "Serve Direction",
       caption=""
  ) 

