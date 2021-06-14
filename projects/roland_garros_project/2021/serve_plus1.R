# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Plot serve +1 bounce location densities        -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
source('/Users/petertea/tennis_analytics/projects/roland_garros_project/src/gg_tennis_court.R')
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/ggplot_theme.R")
source('/Users/petertea/tennis_analytics/projects/roland_garros_project/src/impute_serve_location.R')
training_data <- read.csv('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/processed_roland_garros_2021.csv',
                          na.strings=c("","NA"),
                          stringsAsFactors = FALSE)

courtside.labs <- c("Advantage Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")
servenum.labs <- c("1st Serve Returns", "2nd Serve Returns")
names(servenum.labs) <- c("1", "2")



# -- Nadal vs. Diego -----
my_match_id <- 'year_2021_SM005_tracking_data.json'
plot_data <- training_data %>%
  filter(match_id == my_match_id) %>%
  filter(is_track_avail) %>%
  #filter(server_name == player_name) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    x_new = impute_next_location(x1 = serve_return_bounce_x,
                                 y1 = serve_return_bounce_y, 
                                 x2 = serve_plus1_bounce_x, 
                                 y2 = serve_plus1_bounce_y,
                                 fwd = 1)[1],
    
    y_new = impute_next_location(x1 = serve_return_bounce_x,
                                 y1 = serve_return_bounce_y, 
                                 x2 = serve_plus1_bounce_x, 
                                 y2 = serve_plus1_bounce_y,
                                 fwd = 1)[2]
  ) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *serve_plus1_bounce_x,
                            serve_plus1_bounce_x),
          
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *serve_plus1_bounce_y,
                            serve_plus1_bounce_y),
          x_coord_new = ifelse( (which_side == 'right'), 
                                -1 *x_new,
                                x_new),
          
          y_coord_new = ifelse( (which_side == 'right'), 
                                -1 * y_new,
                                y_new)
  )

ggplot() +
draw_half_tennis_court() +
annotate("text", x = 10.5, y = -4.8, label = "Schwartzman's\n BH Side", colour = 'black', size= 2.7,
         fontface =2) +
  annotate("text", x = 10.5, y = 4.8, label = "Schwartzman's\n FH Side", colour = 'black', size= 2.7,
           fontface =2) +
  annotate("rect", xmin = 10, 
           xmax = 11, 
           ymin = 3.5,
           ymax = 5.5,
           alpha = .1)

my_plot <- 
plot_data %>%
  filter(x_coord > -2) %>%
  #filter(set_num ==3) %>%
  filter(server_name == 'R.NADAL') %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  #draw_full_tennis_court() +
  draw_half_tennis_court() +
  #lazy_add_heatmap_ver2() +
  geom_segment(aes(x=x_coord,
                   y=y_coord,
                   xend = x_coord_new,
                   yend = y_coord_new),
               arrow=arrow(length = unit(0.1,"cm"),
                           type = "closed",
                           ends = 'last'),
               #size=0.2,
               color="black",
               alpha = 0.5) +
  geom_point(alpha = 0.8, fill = '#ccff00', shape = 21,
             size = 2.5) +
  #  facet_wrap(~server_name#,
  # #            #labeller = labeller(server_name = server.labs)
  #  ) 
  annotate("text", x = 10, y = -4.8, label = "Schwartzman's\n BH Side", colour = 'green', size= 3.5,
           fontface =2) +
  annotate("text", x = 10, y = 4.8, label = "Schwartzman's\n FH Side", colour = 'green', size= 3.5,
           fontface =2) +
  transition_states(set_num,
                    state_length = 4, 
                    transition_length = 2) +
  enter_fade() +
  exit_fade() +
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Nadal's Serve +1 Locations against \nSchwartzman Set Number: {closest_state}",
       caption = 'Data: 2021 Roland Garros'
  )

animate(my_plot, duration = 14, fps = 10, units = 'in', res = 200,
        width = 6, height = 4,
        renderer = gifski_renderer('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/plots/nadal_plus1.gif'))

