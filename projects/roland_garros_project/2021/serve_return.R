# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Plot serve return location densities        -----    
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

plot_data <- training_data %>%
  filter(match_id == my_match_id) %>%
  filter(is_track_avail) %>%
  #filter(server_name == player_name) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    x_new = impute_next_location(x1 = serve_return_impact_x,
                                 y1 = serve_return_impact_y, 
                                 x2 = serve_return_bounce_x, 
                                 y2 = serve_return_bounce_y,
                                 fwd = 0.75)[1],
    
    y_new = impute_next_location(x1 = serve_return_impact_x,
                                 y1 = serve_return_impact_y, 
                                 x2 = serve_return_bounce_x, 
                                 y2 = serve_return_bounce_y,
                                 fwd = 0.75)[2]
  ) %>%
  mutate( x_coord = ifelse( (which_side == 'left'), 
                            -1 *serve_return_bounce_x,
                            serve_return_bounce_x),
          
          y_coord = ifelse( (which_side == 'left'), 
                            -1 * serve_return_bounce_y,
                            serve_return_bounce_y),
          x_coord_new = ifelse( (which_side == 'left'), 
                                -1 *x_new,
                                x_new),
          
          y_coord_new = ifelse( (which_side == 'left'), 
                                -1 * y_new,
                                y_new)
  )

# -- Williams vs Collins
my_match_id <- 'wta_year_2021_SD024_tracking_data.json'
plot_data %>%
  filter(x_coord > -2) %>%
  #filter(set_num == 2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  geom_segment(aes(x=x_coord,
                   y=y_coord,
                   xend = x_coord_new,
                   yend = y_coord_new),
               arrow=arrow(length = unit(0.1,"cm"), 
                           type = "closed",
                           ends = 'last'),
               #size=0.2,
               color="black",
               alpha = 0.2) +
  geom_point(alpha = 0.5, fill = 'red', shape = 21,
             size = 2.5) +
  #lazy_add_heatmap() +
   # facet_wrap(~intended_serve_dir)
  facet_wrap(~server_name)

my_plot <- 
plot_data %>%
  filter(x_coord > -2) %>%
  #filter(set_num == 2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  geom_segment(aes(x=x_coord,
                   y=y_coord,
                   xend = x_coord_new,
                   yend = y_coord_new),
               arrow=arrow(length = unit(0.1,"cm"), 
                           type = "closed",
                           ends = 'last'),
               #size=0.2,
               color="yellow",
               alpha = 0.7) +
  geom_point(alpha = 0.5, fill = 'green', shape = 21,
             size = 2.5)  +
  #transition_time(set_num) + frame_time
  transition_states(set_num,
                    state_length = 4, 
                    transition_length = 4) +
  enter_fade() +
  exit_fade() +
  # enter_grow() + 
  # exit_shrink()+
  labs(x = "", 
       y = "",
       title = "Williams Serve Returns against Collins \n Set Number: {closest_state}",
       caption = 'Data: 2021 Roland Garros'
  )

animate(my_plot, duration = 14, fps = 10, units = 'in', res = 200,
        width = 6, height = 4,
        renderer = gifski_renderer('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/plots/test.gif'))




# -- Plot with heatmaps
server.labs <- c("Nadal's Serve Returns", "Norrie's Serve Returns")
names(server.labs) <- c("R.NADAL", "C.NORRIE")


my_plot2 <- 
plot_data %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  geom_segment(aes(x=x_coord,
                   y=y_coord,
                   xend = x_coord_new,
                   yend = y_coord_new),
               arrow=arrow(length = unit(0.1,"cm"), 
                           type = "closed",
                           ends = 'last'),
               #size=0.2,
               color="black",
               alpha = 0.2) +
  geom_point(alpha = 0.5, fill = 'red', shape = 21,
             size = 2.5) +
  # facet_wrap(~serve_num,
  #            labeller = labeller(serve_num = servenum.labs)) +
  facet_wrap(~server_name,
             labeller = labeller(server_name = server.labs)) +
  transition_states(set_num,
                    state_length = 4, 
                    transition_length = 4) +
  enter_fade() +
  exit_fade() +
  labs(x = "", 
       y = "",
       title = "Serve Returns for Nadal vs. Norrie \n Set Number: {closest_state}",
       caption = 'Data: 2021 Roland Garros'
  )

animate(my_plot2, duration = 14, fps = 10, units = 'in', res = 200,
        width = 6, height = 4,
        renderer = gifski_renderer('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/plots/nadal_vs_norrie.gif'))


# -- Federer vs. Koepfer
my_match_id <- 'year_2021_SM019_tracking_data.json'
plot_data <- training_data %>%
  filter(match_id == my_match_id) %>%
  filter(is_track_avail) %>%
  #filter(server_name == player_name) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    x_new = impute_next_location(x1 = serve_return_impact_x,
                                 y1 = serve_return_impact_y, 
                                 x2 = serve_return_bounce_x, 
                                 y2 = serve_return_bounce_y,
                                 fwd = 0.75)[1],
    
    y_new = impute_next_location(x1 = serve_return_impact_x,
                                 y1 = serve_return_impact_y, 
                                 x2 = serve_return_bounce_x, 
                                 y2 = serve_return_bounce_y,
                                 fwd = 0.75)[2]
  ) %>%
  mutate( x_coord = ifelse( (which_side == 'left'), 
                            -1 *serve_return_bounce_x,
                            serve_return_bounce_x),
          
          y_coord = ifelse( (which_side == 'left'), 
                            -1 * serve_return_bounce_y,
                            serve_return_bounce_y),
          x_coord_new = ifelse( (which_side == 'left'), 
                                -1 *x_new,
                                x_new),
          
          y_coord_new = ifelse( (which_side == 'left'), 
                                -1 * y_new,
                                y_new)
  )

server.labs <- c("Federer's Serve Returns", "Koepfer's Serve Returns")
names(server.labs) <- c("R.FEDERER", "D.KOEPFER")

my_plot3 <-
plot_data %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  geom_segment(aes(x=x_coord,
                   y=y_coord,
                   xend = x_coord_new,
                   yend = y_coord_new),
               arrow=arrow(length = unit(0.1,"cm"), 
                           type = "closed",
                           ends = 'last'),
               #size=0.2,
               color="black",
               alpha = 0.2) +
  geom_point(alpha = 0.5, fill = 'red', shape = 21,
             size = 2.5) +
  facet_wrap(~server_name,
             labeller = labeller(server_name = server.labs)) +
  transition_states(set_num,
                    state_length = 4, 
                    transition_length = 4) +
  enter_fade() +
  exit_fade() +
  labs(x = "", 
       y = "",
       title = "Serve Returns for Federer vs. Koepfer \n Set Number: {closest_state}",
       caption = 'Data: 2021 Roland Garros'
  )
animate(my_plot3, duration = 14, fps = 10, units = 'in', res = 200,
        width = 6, height = 4,
        renderer = gifski_renderer('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/plots/federer_vs_koepfer.gif'))






# -- Blank templates -----
my_match_id <- 'year_2021_SM013_tracking_data.json'
plot_data <- training_data %>%
  filter(match_id == my_match_id) %>%
  filter(is_track_avail) %>%
  #filter(server_name == player_name) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    x_new = impute_next_location(x1 = serve_return_impact_x,
                                 y1 = serve_return_impact_y, 
                                 x2 = serve_return_bounce_x, 
                                 y2 = serve_return_bounce_y,
                                 fwd = 0.75)[1],
    
    y_new = impute_next_location(x1 = serve_return_impact_x,
                                 y1 = serve_return_impact_y, 
                                 x2 = serve_return_bounce_x, 
                                 y2 = serve_return_bounce_y,
                                 fwd = 0.75)[2]
  ) %>%
  mutate( x_coord = ifelse( (which_side == 'left'), 
                            -1 *serve_return_bounce_x,
                            serve_return_bounce_x),
          
          y_coord = ifelse( (which_side == 'left'), 
                            -1 * serve_return_bounce_y,
                            serve_return_bounce_y),
          x_coord_new = ifelse( (which_side == 'left'), 
                                -1 *x_new,
                                x_new),
          
          y_coord_new = ifelse( (which_side == 'left'), 
                                -1 * y_new,
                                y_new)
  )

plot_data %>%
  select(serve_return_bounce_x, serve_return_bounce_y, serve_return_bounce_z,
         serve_return_impact_x, serve_return_impact_y, serve_return_impact_z) %>%
  View()

plot_data %>%
  #filter(x_coord > -2) %>%
  #filter(set_num ==1) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_full_tennis_court() +
  #draw_half_tennis_court() +
  #lazy_add_heatmap() +
  geom_segment(aes(x=x_coord,
                   y=y_coord,
                   xend = x_coord_new,
                   yend = y_coord_new),
               arrow=arrow(length = unit(0.1,"cm"), 
                           type = "closed",
                           ends = 'last'),
               #size=0.2,
               color="black",
               alpha = 0.2) +
  geom_point(alpha = 0.5, fill = 'red', shape = 21,
             size = 2.5) +
  facet_wrap(~server_name#,
             #labeller = labeller(server_name = server.labs)
  )


server.labs <- c("Medvedev's Serve Returns", "Nishikori's Serve Returns")
names(server.labs) <- c("D.MEDVEDEV", "C.GARIN")

my_plotx <- 
  plot_data %>%
  #filter(x_coord > -2) %>%
  #filter(set_num ==1) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_full_tennis_court() +
  #draw_half_tennis_court() +
  #lazy_add_heatmap() +
  geom_segment(aes(x=x_coord,
                   y=y_coord,
                   xend = x_coord_new,
                   yend = y_coord_new),
               arrow=arrow(length = unit(0.1,"cm"), 
                           type = "closed",
                           ends = 'last'),
               #size=0.2,
               color="black",
               alpha = 0.2) +
  geom_point(alpha = 0.5, fill = 'red', shape = 21,
             size = 2.5) +
  facet_wrap(~server_name#,
             #labeller = labeller(server_name = server.labs)
             ) +
  transition_states(set_num,
                    state_length = 4, 
                    transition_length = 4) +
  enter_fade() +
  exit_fade() +
  labs(x = "", 
       y = "",
       title = "Serve Returns ZVEREV vs. NISHIKORI \n Set Number: {closest_state}",
       caption = 'Data: 2021 Roland Garros'
  )

animate(my_plotX, duration = 14, fps = 10, units = 'in', res = 200,
        width = 6, height = 4,
        renderer = gifski_renderer('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/plots/X.gif'))

