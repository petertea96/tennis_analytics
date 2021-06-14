library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)

source('/Users/petertea/tennis_analytics/projects/roland_garros_project/src/gg_tennis_court.R')
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/ggplot_theme.R")
source('/Users/petertea/tennis_analytics/projects/roland_garros_project/src/impute_serve_location.R')
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_project/")
training_data <- read.csv('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/processed_roland_garros_2021.csv',
                          na.strings=c("","NA"),
                          stringsAsFactors = FALSE)

courtside.labs <- c("Ad. Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")

servenum.labs <- c("First Serve", "Second Serve")
names(servenum.labs) <- c("1", "2")

iswta.labs <- c("Women", "Men")
names(iswta.labs) <- c("True", "False")

player_name <- 'V.AZARENKA'
player_name <- 'I.SWIATEK'
player_name <- 'S.TSITSIPAS'
player_name <- 'R.NADAL'

plot_data <- training_data %>%
  filter(is_track_avail) %>%
  #filter(server_name == player_name) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    x_new = impute_next_location(x1 = x_ball_at_serve,
                                 y1 = y_ball_at_serve, 
                                 x2 = serveBounceCordinate_x, 
                                 y2 = serveBounceCordinate_y,
                                 fwd = 1)[1],
    
    y_new = impute_next_location(x1 = x_ball_at_serve,
                                 y1 = y_ball_at_serve, 
                                 x2 = serveBounceCordinate_x, 
                                 y2 = serveBounceCordinate_y,
                                 fwd = 1)[2]
  ) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_x,
                            serveBounceCordinate_x),
          
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_y,
                            serveBounceCordinate_y),
          x_coord_new = ifelse( (which_side == 'right'), 
                                -1 *x_new,
                                x_new),
          
          y_coord_new = ifelse( (which_side == 'right'), 
                                -1 * y_new,
                                y_new)
  )

plot_data %>%
  filter(x_coord > -2) %>%
  #filter(serve_num == 2) %>%
  filter(is_wta =='True') %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  #draw_full_tennis_court() +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  facet_grid(~court_side,
             labeller = labeller(court_side = courtside.labs)
  ) +
  labs(x = "", 
       y = "",
       title = "Serve Bounce Locations",
       caption = 'Data: 2021 Roland Garros'
  )




my_plotx1 <- 
  plot_data %>%
  filter(x_coord > -2) %>%
  filter(is_wta =='False') %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  facet_grid(~court_side,
             labeller = labeller(court_side = courtside.labs)
  ) +
  labs(x = "", 
       y = "",
       title = "Men's Serve Bounce Locations \n Serve Number: {closest_state}",
       caption = 'Data: 2021 Roland Garros'
  ) + 
  transition_states(serve_num,
                    state_length = 4, 
                    transition_length = 4) +
  enter_fade() +
  exit_fade() 

animate(my_plotx1, duration = 5, fps = 10, units = 'in', res = 180,
        width = 6, height = 4,
        renderer = gifski_renderer('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/plots/men_servenum.gif'))


my_plotx2 <- 
  plot_data %>%
  filter(x_coord > -2) %>%
  filter(is_wta =='True') %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  facet_grid(~court_side,
             labeller = labeller(court_side = courtside.labs)
  ) +
  labs(x = "", 
       y = "",
       title = "Women's Serve Bounce Locations \n Serve Number: {closest_state}",
       caption = 'Data: 2021 Roland Garros'
  ) + 
  transition_states(serve_num,
                    state_length = 4, 
                    transition_length = 4) +
  enter_fade() +
  exit_fade() 

animate(my_plotx2, duration = 5, fps = 10, units = 'in', res = 180,
        width = 6, height = 4,
        renderer = gifski_renderer('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/plots/women_servenum.gif'))
