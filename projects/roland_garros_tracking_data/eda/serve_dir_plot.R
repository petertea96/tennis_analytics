### For a given player, plot their serve direction patterns

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

playerid_df <- read.csv('collect_data/player_id.csv',stringsAsFactors = FALSE)
playerid_df$id <- as.integer(playerid_df$id)

atp_pbp_df <- read.csv('./eda/atp_roland_garros_19_20.csv')

atp_pbp_df <- atp_pbp_df %>%
  filter(year != 2018) 

atp_pbp_df$server_id <- as.integer(as.character(atp_pbp_df$server_id))
atp_pbp_df$returner_id <- as.integer(as.character(atp_pbp_df$returner_id))

atp_pbp_df <- atp_pbp_df %>%
  mutate(serve_side = ifelse( point_num %% 2 == 0, 'Ad', 'Deuce')) %>%
  left_join(playerid_df, by = c('server_id' = 'id')) %>%
  rename(server_name = name) %>%
  left_join(playerid_df, by = c('returner_id' = 'id')) %>%
  rename(returner_name = name)

write.csv(atp_pbp_df,
          'atp_pbp_df.csv', 
          row.names = FALSE)


#player_name <- 'R.NADAL'
#player_name <- 'N.DJOKOVIC'
#player_name <- 'R.FEDERER'
#player_name <- 'D.THIEM'
#player_name <- 'S.TSITSIPAS'
player_name <- 'A.ZVEREV'

atp_to_plot <- atp_pbp_df %>%
  select(serveBounceCordinate_y, serve_dir, serve_side, serve_num,
         fault_distance_missed_m, point_num, game_num, set_num, year, 
         server_name, returner_name, point_end_type) %>%
  filter(server_name == player_name)

# -- Save Shapo data
# player_name <- 'D.SHAPOVALOV'
# shapo_data <- atp_pbp_df %>%
#   select(year,point_ID, set_num, game_num, point_num,  serve_num,
#          serve_speed_kph, is_break_point, serve_side,
#          serveBounceCordinate_y, serve_dir, 
#          fault_distance_missed_m,   
#          server_name, returner_name, point_end_type) %>%
#   filter(server_name == player_name | returner_name == player_name)

write.csv(shapo_data,'shapovalov.csv', row.names = FALSE)


atp_to_plot$serve_dir <- factor(atp_to_plot$serve_dir,
                                        levels = c('T', 'Body', 'Wide',''))


# -- Plot of Serve Direction Proportions stratified by match & Fill by Court Side 
# ***** (1st Serve) *****
atp_plot_count_df <- atp_to_plot %>%
  filter(serve_dir != '') %>%
  filter(serve_num==1) %>%
  group_by(year, returner_name, serve_dir, serve_side) %>%
  summarise(count = n()) %>%
  left_join(atp_to_plot %>%
              filter(serve_dir != '') %>%
              filter(serve_num==1) %>%
              group_by(year, returner_name, serve_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))


ggplot(data=atp_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = serve_side)) +
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
  group_by(year, returner_name, serve_dir, serve_side) %>%
  summarise(count = n()) %>%
  left_join(atp_to_plot %>%
              filter(serve_dir != '') %>%
              filter(serve_num==2) %>%
              group_by(year, returner_name, serve_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))


ggplot(data=atp_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = serve_side)) +
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
library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

playerid_df <- read.csv('collect_data/player_id.csv',stringsAsFactors = FALSE)
playerid_df$id <- as.integer(playerid_df$id)

wta_pbp_df <- read.csv('./eda/wta_roland_garros_19_20.csv')


wta_pbp_df$server_id <- as.integer(as.character(wta_pbp_df$server_id))
wta_pbp_df$returner_id <- as.integer(as.character(wta_pbp_df$returner_id))

wta_pbp_df <- wta_pbp_df %>%
  mutate(serve_side = ifelse( point_num %% 2 == 0, 'Ad', 'Deuce')) %>%
  left_join(playerid_df, by = c('server_id' = 'id')) %>%
  rename(server_name = name) %>%
  left_join(playerid_df, by = c('returner_id' = 'id')) %>%
  rename(returner_name = name)


player_name <- 'S.KENIN'
# player_name <- 'S.HALEP'
# player_name <- 'C.GARCIA'
# player_name <- 'E.SVITOLINA'
player_name <- 'P.KVITOVA'

wta_to_plot <- wta_pbp_df %>%
  select(serveBounceCordinate_y, serve_dir, serve_side, serve_num,
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
  group_by(year, returner_name, serve_dir, serve_side) %>%
  summarise(count = n()) %>%
  left_join(wta_to_plot %>%
              filter(serve_dir != '') %>%
              filter(serve_num==1) %>%
              group_by(year, returner_name, serve_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))


ggplot(data=wta_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = serve_side)) +
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
  group_by(year, returner_name, serve_dir, serve_side) %>%
  summarise(count = n()) %>%
  left_join(wta_to_plot %>%
              filter(serve_dir != '') %>%
              filter(serve_num==2) %>%
              group_by(year, returner_name, serve_side) %>%
              summarise(tot_serves = n())) %>%
  mutate(prop = count / tot_serves,
         match_lab = paste(returner_name, year))


ggplot(data=wta_plot_count_df, 
       aes(x=serve_dir, y=prop, fill = serve_side)) +
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

