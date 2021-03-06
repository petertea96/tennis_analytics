### Plot Djokovic's serve patterns 

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

playerid_df <- read.csv('collect_data/player_id.csv',stringsAsFactors = FALSE)
playerid_df$id <- as.integer(playerid_df$id)

djoko_df <- read.csv('./djokovic/djokovic_roland_garros_19_20.csv')

djoko_df <- djoko_df %>%
  mutate(serve_side = ifelse( point_num %% 2 == 0, 'Ad', 'Deuce')) %>%
  left_join(playerid_df, by = c('server_id' = 'id')) %>%
  rename(server_name = name) %>%
  left_join(playerid_df, by = c('returner_id' = 'id')) %>%
  rename(returner_name = name)
  

djoko_plot_df <- djoko_df %>%
  select(serveBounceCordinate_y, serve_dir, serve_side, serve_num,
         fault_distance_missed_m, point_num, game_num, set_num, year, 
         server_name, returner_name ) %>%
  filter(server_name == 'N.DJOKOVIC')


djoko_plot_count_df <- djoko_plot_df %>%
  filter(serve_dir != '') %>%
  group_by(year, returner_name, serve_dir) %>%
  summarise(count = n()) %>%
  left_join(djoko_plot_df %>%
              filter(serve_dir != '') %>%
              group_by(year, returner_name) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))


# -- Plot of Serve Direction Proportions stratified by match
ggplot(data=djoko_plot_count_df, 
       aes(x=serve_dir, y=prop)) +
  facet_wrap(~match_lab) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = 'Djokovic RG Serve Directions',
       x='',
       y = 'Proportion of Serves',
       fill = "Serve Direction",
       caption=""
  ) 


# -- Plot of Serve Direction Proportions stratified by match & Fill by Court Side 
# ***** (1st Serve) *****

djoko_plot_count_df <- djoko_plot_df %>%
  filter(serve_dir != '') %>%
  filter(serve_num==1) %>%
  group_by(year, returner_name, serve_dir, serve_side) %>%
  summarise(count = n()) %>%
  left_join(djoko_plot_df %>%
              filter(serve_dir != '') %>%
              filter(serve_num==1) %>%
              group_by(year, returner_name, serve_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))

djoko_plot_count_df$serve_dir <- factor(djoko_plot_count_df$serve_dir,
                                        levels = c('T', 'Body', 'Wide',''))

ggplot(data=djoko_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = serve_side)) +
  facet_wrap(~match_lab) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = 'Djokovic 1st Serve RG Serve Directions',
       x='',
       y = 'Proportion of Serves',
       fill = "Serve Direction",
       caption=""
  ) 


# -- Plot of Serve Direction Proportions stratified by match & Fill by Court Side 
# ***** (2nd Serve) *****

djoko_plot_count_df <- djoko_plot_df %>%
  filter(serve_dir != '') %>%
  filter(serve_num==2) %>%
  group_by(year, returner_name, serve_dir, serve_side) %>%
  summarise(count = n()) %>%
  left_join(djoko_plot_df %>%
              filter(serve_dir != '') %>%
              filter(serve_num==2) %>%
              group_by(year, returner_name, serve_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))

djoko_plot_count_df$serve_dir <- factor(djoko_plot_count_df$serve_dir,
                                        levels = c('T', 'Body', 'Wide',''))

ggplot(data=djoko_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = serve_side)) +
  facet_wrap(~match_lab) +
  geom_bar(stat="identity",
           position=position_dodge(),
           width=0.75, colour = 'black') +
  labs(title = 'Djokovic 2nd Serve RG Serve Directions',
       x='',
       y = 'Proportion of Serves',
       fill = "Serve Direction",
       caption=""
  ) 
