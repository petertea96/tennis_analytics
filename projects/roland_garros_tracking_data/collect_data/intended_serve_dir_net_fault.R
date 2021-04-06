# -- prototype script to impute intended serve location for net faults

library(dplyr)
library(ggplot2)
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/collect_data/data/")
source('./../../src/gg_tennis_court.R')

atp_pbp_df <- read.csv('atp_processed_roland_garros_tracking_data.csv')

#colnames(atp_pbp_df)
#table(atp_pbp_df$point_end_type)
#table(atp_pbp_df$error_type)
#table(atp_pbp_df$trapped_by_net)
# Filter so that 
# --> point_end_type == 'Faulty Serve'
# --> error_type == 'Net Error'

# trapped_by_net == 'True' tells you...nothing???

# there is no reliable way to pinpoint which serves are net serves.
# Instead, I will say all serve bounces with x-coordinates < 6.4/3 meters are net faults

# A "better" solution:
# Create a line from the high post (1.07 m high) to the middle net (0.91 m high).
# Use a function that takes as input the y-coordinate to determine whether the ball clipped 
# the net.


get_net_height <- function(y_coordinate){
  # -- Get net height given the y (vertical) coordinate.
  
  y_coordinate <- abs(y_coordinate)
  
  m = (1.07 - 0.91) / (4.115 - 0)
  b = 1.07 - m*4.115
  
  net_height <- m*y_coordinate + b 
  
  return(net_height)
}

get_net_height(4.115)

# -- Use physics
get_intended_serve_bounce_loc <- function(x_ball_at_serve, y_ball_at_serve, z_ball_at_serve,
                                          z_net_serve, y_net_serve){
  
  direction_vec <- c(0 - x_ball_at_serve, 
                     y_net_serve - y_ball_at_serve, 
                     z_net_serve - z_ball_at_serve)
  
  z_term <- (0 - z_net_serve) / direction_vec[3] 
  intended_x <- z_term*direction_vec[1] + 0
  intended_y <- z_term*direction_vec[2] + y_net_serve
  
  return(c(intended_x, intended_y))
  
}

processed_atp_pbp_df <- 
  atp_pbp_df %>%
  filter(is_track_avail) %>%
  filter(!is.na(y_net_serve)) %>%
  rowwise() %>%
  mutate(
    error_type = ifelse( ((is_fault == 1) & (z_net_serve <= get_net_height(y_coordinate= y_net_serve))),
                         'Net Error',
                         error_type),
    
    intended_serve_bounce_x = ifelse(error_type == 'Net Error',
                                     get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
                                                                   y_ball_at_serve = y_ball_at_serve,
                                                                   z_ball_at_serve = z_ball_at_serve,
                                                                   y_net_serve = y_net_serve,
                                                                   z_net_serve = z_net_serve)[1],
                                     serveBounceCordinate_x),
    intended_serve_bounce_y = ifelse(error_type == 'Net Error',
                                     get_intended_serve_bounce_loc(x_ball_at_serve = x_ball_at_serve,
                                                                   y_ball_at_serve = y_ball_at_serve,
                                                                   z_ball_at_serve = z_ball_at_serve,
                                                                   y_net_serve = y_net_serve,
                                                                   z_net_serve = z_net_serve)[2],
                                     serveBounceCordinate_y),
    
    
    # -- Transform all coordinates to lie on the right part of the court.
    # Is it appropriate to take the absolute value?
    # On the right side of court, x_coord is always positive. However, y coord can be
    # (+) on Deuce or (-) on AdCourt
    x_coord = ifelse( (which_side == 'right'), 
                      abs(intended_serve_bounce_x),
                      intended_serve_bounce_x),
    # Something funky with some left side serves (all faults) being recorded as highly (-).
    x_coord = ifelse( ((which_side == 'left') & (x_coord < 0)),
                      abs(x_coord),
                      x_coord),
    y_coord = ifelse( (which_side == 'right'),
                      -1*(intended_serve_bounce_y),
                      intended_serve_bounce_y)
    
  )

sum(processed_atp_pbp_df$error_type == 'Net Error', na.rm = TRUE)


processed_atp_pbp_df %>%
  select(which_side, is_fault, error_type,
         x_ball_at_serve, intended_serve_bounce_x, x_coord, serveBounceCordinate_x, serveBounceCordinate_y,
         y_ball_at_serve, intended_serve_bounce_y,  y_coord) %>%
  View()


# processed_atp_pbp_df %>%
#   filter(x_coord < 0) %>%
#   select(which_side, is_fault, error_type,
#          x_ball_at_serve, intended_serve_bounce_x, x_coord, serveBounceCordinate_x, serveBounceCordinate_y,
#          y_ball_at_serve, intended_serve_bounce_y,  y_coord) %>%
#   View()

# --- ##### --- ##### --- ##### --- #####  --- ##### --- ##### 
# -- Data checks: ------
# --- ##### --- ##### --- ##### --- #####  --- ##### --- ##### 

# -- How many Net errors are there?
sum(processed_atp_pbp_df$error_type == 'Net Error', na.rm = TRUE)

# -- Expect most (-) y-coordinates to be on Ad court serves
processed_atp_pbp_df %>%
  group_by(court_side) %>%
  summarise(sum(y_coord<0, na.rm = TRUE))

# -- Expect most (+) y-coordinates to be on Deuce court
processed_atp_pbp_df %>%
  group_by(court_side) %>%
  summarise(sum(y_coord>0, na.rm = TRUE))

processed_atp_pbp_df %>%
  filter(error_type == 'Net Error') %>%
  select(z_net_serve, error_type, point_end_type, trapped_by_net) %>%
  View()
  

# -- Plot all Non-net error fault locations
processed_atp_pbp_df %>%
  filter(is_fault == 1) %>%
  filter(error_type != 'Net Error') %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') 

# -- Plot all net error fault projected locations
processed_atp_pbp_df %>%
  filter(is_fault == 1) %>%
  filter(error_type == 'Net Error') %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') 


# Fit regression model:
# Meh solution
# Idea: Fit linear model on intended x and y coordinate serve locations using the 
# following as predictors: Serve toss height and position, speed, position at net. 
fit_data <- 
plot_fault_data %>%
  filter(serve_speed_kph > 0) %>%
  filter(is_track_avail) %>%
  filter( !((is_fault == 1) & (error_type !='Net Error')) )


model_serve_bounce_x <- lm(x_coord ~ x_ball_at_serve + y_ball_at_serve + z_ball_at_serve + z_net_serve,
                           data = fit_data)



