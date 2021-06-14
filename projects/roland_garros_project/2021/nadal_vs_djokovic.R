# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Nadal Vs. Djokovic        -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
source('/Users/petertea/tennis_analytics/projects/roland_garros_project/src/gg_tennis_court.R')
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/ggplot_theme.R")
source('/Users/petertea/tennis_analytics/projects/roland_garros_project/src/impute_serve_location.R')

courtside.labs <- c("Ad. Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")

match_labs <- c('2020 Final vs. Nadal', '2021 Semi-Final vs. Nadal')
names(match_labs) <- c('2020', '2021')

training_data <- read.csv('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/processed_roland_garros_2021.csv',
                          na.strings=c("","NA"),
                          stringsAsFactors = FALSE)
rg_2020 <- read.csv("/Users/petertea/tennis_analytics/projects/roland_garros_project/collect_data/data/atp_processed_roland_garros_project.csv")


nadal_djokovic_2021 <- training_data %>%
  filter(match_id == 'year_2021_SM002_tracking_data.json') %>%
  filter(server_name == 'N.DJOKOVIC') %>%
  select(point_ID, set_num, serve_num, court_side, is_track_avail,
         is_break_point, is_fault, year, 
         x_ball_at_serve, y_ball_at_serve,
         serveBounceCordinate_x, serveBounceCordinate_y,
         serve_return_impact_x, serve_return_impact_y,
         serve_return_impact_z, 
         serve_return_bounce_x, serve_return_bounce_y,
         serve_return_bounce_z,
         serve_plus1_bounce_x, serve_plus1_bounce_y,
         serve_plus1_bounce_z,
         which_side, server_name, returner_name,
         point_importance)


nadal_djokovic_2021$cat_point_importance <- ifelse(nadal_djokovic_2021$point_importance >= quantile(nadal_djokovic_2021$point_importance, 0.6),
                                         1,0)

nadal_djokovic_2020 <- rg_2020 %>%
  filter(match_id == 'year_2020_SM001_tracking_data.json') %>%
  filter(server_name == 'N.DJOKOVIC') %>%
  select(point_ID, set_num, serve_num, court_side,
         is_track_avail,
         is_break_point, is_fault, year, 
         x_ball_at_serve, y_ball_at_serve,
         serveBounceCordinate_x, serveBounceCordinate_y,
         serve_return_impact_x, serve_return_impact_y,
         serve_return_impact_z, 
         serve_return_bounce_x, serve_return_bounce_y,
         serve_return_bounce_z,
         serve_plus1_bounce_x, serve_plus1_bounce_y,
         serve_plus1_bounce_z,
         which_side, server_name, returner_name,
         point_importance)
nadal_djokovic_2020$cat_point_importance <- ifelse(nadal_djokovic_2020$point_importance >= quantile(nadal_djokovic_2020$point_importance, 0.6),
                                                   1,0)


plot_data <- rbind(nadal_djokovic_2020,nadal_djokovic_2021)
plot_data$court_side <- factor(plot_data$court_side,
                               levels = c('DeuceCourt', 'AdCourt'))

plot_data <- plot_data %>%
  mutate(lab_cat_point_importance = ifelse(cat_point_importance == 0,
                                           paste(cat_point_importance, 'Low', sep="-"),
                                           paste(cat_point_importance, 'High', sep="-")
                                           ))

plot_data$lab_cat_point_importance <- factor(plot_data$lab_cat_point_importance)

# -- Serve Bounce by Point Importance -----
plot_serve_bounce_data <- plot_data %>%
  filter(is_track_avail) %>%
  filter(server_name == 'N.DJOKOVIC') %>%
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

serve_bounce_plot <- 
plot_serve_bounce_data %>%
  filter(x_coord > -2) %>%
  #filter(cat_point_importance == 0) %>%
  #filter(serve_num==1) %>%
  #filter(set_num ==2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  # geom_segment(aes(x=x_coord,
  #                  y=y_coord,
  #                  xend = x_coord_new,
  #                  yend = y_coord_new),
  #              arrow=arrow(length = unit(0.1,"cm"),
  #                          type = "closed",
  #                          ends = 'last'),
  #              #size=0.2,
  #              color="black",
  #              alpha = 0.2) +
  # geom_point(alpha = 0.5, fill = 'red', shape = 21,
  #            size = 2.5) +
  facet_grid(court_side~year,
             labeller = labeller(court_side = courtside.labs,
                                 year = match_labs)
  ) +
  transition_states(lab_cat_point_importance,
                    state_length = 4,
                    transition_length = 4) +
  enter_fade() +
  exit_fade() +
  labs(fill = 'Density',
       #"Djokovic's Serve Location \n Point Importance Level:",
       title =paste("Djokovic's Serve Location Heat Maps \n Match Pressure Level:",
                    '{gsub(pattern = "[0-9]+-", replacement = "", closest_state)}'))

animate(serve_bounce_plot, duration = 7, fps = 10, units = 'in', res = 200,
        width = 6, height = 4,
        renderer = gifski_renderer('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/plots/djokovic_nadal_serve.gif'))



# Serve Returns -----
plot_serve_return_bounce_data <- plot_data %>%
  filter(is_track_avail) %>%
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


plot_serve_return_bounce_data %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  #lazy_add_heatmap() +
  # geom_segment(aes(x=x_coord,
  #                  y=y_coord,
  #                  xend = x_coord_new,
  #                  yend = y_coord_new),
  #              arrow=arrow(length = unit(0.1,"cm"),
  #                          type = "closed",
  #                          ends = 'last'),
  #              #size=0.2,
  #              color="black",
  #              alpha = 0.2) +
  geom_point(alpha = 0.5, fill = 'red', shape = 21,
             size = 2.5) +
  facet_grid(serve_num~year) 


# Serve +1 -----

plot_serve_plus1_bounce_data <- plot_data %>%
  filter(is_track_avail) %>%
  filter(server_name == 'N.DJOKOVIC') %>%
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


plot_serve_plus1_bounce_data %>%
  filter(x_coord > -2) %>%
  #filter(set_num ==3) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  #draw_full_tennis_court() +
  draw_half_tennis_court() +
  lazy_add_heatmap_ver2() +
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
  facet_grid(~year)

