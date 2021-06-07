# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Plot serve bounce location densities        -----    
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



# -- Williams vs. Buzarnescu -----
my_match_id <- 'wta_year_2021_SD048_tracking_data.json'
plot_data <- training_data %>%
  filter(match_id == my_match_id) %>%
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
  filter(set_num ==2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  #draw_full_tennis_court() +
  draw_half_tennis_court() +
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


# -- Zverev vs. Nishikori -----
server_name.labs <- c("Zverev Serve Locations", "Nishikori Serve Locations")
names(server_name.labs) <- c("A.ZVEREV", "K.NISHIKORI")

my_match_id <- 'year_2021_SM012_tracking_data.json'

plot_data <- training_data %>%
  filter(match_id == my_match_id) %>%
  filter(is_track_avail) %>%
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
             size = 1.5) +
  
  facet_grid(court_side ~ server_name,
             labeller =  labeller(court_side = courtside.labs,
                                  server_name = server_name.labs))

my_plot <- 
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
             size = 1.5) +
  
  facet_grid(court_side ~ server_name,
             labeller =  labeller(court_side = courtside.labs,
                                  server_name = server_name.labs))+ 
  guides( fill = guide_colourbar( barheight = unit( 2 , "in" ) ))+
  transition_states(set_num,
                    state_length = 4, 
                    transition_length = 4) +
  enter_fade() +
  exit_fade() +
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Serve Bounces ZVEREV vs. NISHIKORI \n Set Number: {closest_state}",
       caption = 'Data: 2021 Roland Garros'
  )
animate(my_plot, duration = 14, fps = 10, units = 'in', res = 200,
        width = 6, height = 4,
        renderer = gifski_renderer('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/plots/zverev_nishikori.gif'))
