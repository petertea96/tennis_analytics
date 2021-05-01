# -- Plot 3 ATP and 3 WTA players in serve heatmaps 
# -- It's easier to have a single plot, with a small subset of players (for the paper)

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")
source('src/gg_tennis_court.R')


atp_training_data <- read.csv('./collect_data/data/atp_processed_roland_garros_tracking_data.csv',
                              stringsAsFactors = FALSE)

wta_training_data <- read.csv('./collect_data/data/wta_processed_roland_garros_tracking_data.csv',
                              stringsAsFactors = FALSE)

atp_players <- c('R.NADAL', 'R.FEDERER', 'N.DJOKOVIC')
wta_players <- c('I.SWIATEK', 'S.HALEP', 'A.BARTY')

training_data <- rbind(atp_training_data  %>% filter(server_name %in% atp_players),
                       wta_training_data  %>% filter(server_name %in% wta_players)
                       )

plot_one_court_data <- training_data %>%
  filter(is_track_avail) %>%
  distinct() %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *intended_serve_bounce_x,
                            intended_serve_bounce_x),
          
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *intended_serve_bounce_y,
                            intended_serve_bounce_y))

plot_one_court_data$server_name <- factor(plot_one_court_data$server_name,
                                          levels = c('R.FEDERER', 'R.NADAL', 'N.DJOKOVIC', 
                                                     'I.SWIATEK', 'S.HALEP', 'A.BARTY'))

plot_one_court_data %>%
  filter(court_side =='DeuceCourt') %>%
  #filter(serve_num == 2) %>%
  #filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  facet_wrap(~server_name) + 
  labs(x = "", 
       y = "",
       title = "All 1st & 2nd Serve Locations on Deuce Court",
       caption = "Data: Roland Garros 2019-20") 

ggsave('./eda/plots/atp_wta_serve_loc_on_deuce.jpg',
       width=6, height=3.5,
       dpi = 300)



