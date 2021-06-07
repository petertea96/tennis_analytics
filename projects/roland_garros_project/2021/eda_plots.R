# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Plot player serve location densities        -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

library(dplyr)
library(ggplot2)
source('/Users/petertea/tennis_analytics/projects/roland_garros_project/src/gg_tennis_court.R')
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/ggplot_theme.R")
source('/Users/petertea/tennis_analytics/projects/roland_garros_project/src/impute_serve_location.R')
training_data <- read.csv('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/processed_roland_garros_2021.csv',
                          na.strings=c("","NA"),
                          stringsAsFactors = FALSE)


my_match_id <- 'wta_year_2021_SD048_tracking_data.json'
my_match_id <- 'wta_year_2021_SD024_tracking_data.json'
player_name <- 'S.WILLIAMS'

plot_data <- training_data %>%
  filter(match_id == my_match_id) %>%
  filter(is_track_avail) %>%
  filter(server_name == player_name) %>%
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


# -- Serve Bounces
plot_data %>%
  #filter(court_side =='AdCourt') %>%
  #filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  geom_point(alpha = 0.5, fill = 'orange', shape = 21,
             size = 3) +
  #lazy_add_heatmap() +
  facet_grid(court_side ~ serve_num) + 
  guides( fill = guide_colourbar( barheight = unit( 2 , "in" ) ))+
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Serve Bounces",
       caption = 'Data: 2021 Roland Garros'
  )


# -- Serve Bounces with Direction vectors
plot_data %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = serveBounceCordinate_x, 
             y = serveBounceCordinate_y)) +
  draw_full_tennis_court() +
  geom_segment(aes(x=x_ball_at_serve, 
                   y=y_ball_at_serve, 
                   xend=serveBounceCordinate_x, 
                   yend=serveBounceCordinate_y),
               arrow=arrow(length = unit(0.2,"cm"), type = "closed"), 
               #size=0.2, 
               color="red") + 
  geom_point(alpha = 0.5, fill = 'orange', shape = 21,
             size = 3) +
  #lazy_add_heatmap() +
  facet_grid(court_side ~ serve_num) + 
  guides( fill = guide_colourbar( barheight = unit( 2 , "in" ) ))+
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Serve Bounces",
       caption = 'Data: 2021 Roland Garros'
  )


# ****
plot_data %>%
  filter(x_coord > -2) %>%
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
             size = 2.5) +
  #lazy_add_heatmap() +
  facet_grid(court_side ~ serve_num) + 
  guides( fill = guide_colourbar( barheight = unit( 2 , "in" ) ))+
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Serve Bounces",
       caption = 'Data: 2021 Roland Garros'
  )

plot_data %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = serveBounceCordinate_x, 
             y = serveBounceCordinate_y)) +
  draw_full_tennis_court() +
  geom_segment(aes(x=serveBounceCordinate_x,
                   y=serveBounceCordinate_y,
                   xend = x_new,
                   yend = y_new),
               arrow=arrow(length = unit(0.1,"cm"), type = "closed"),
               #size=0.2,
               color="indianred") +
  geom_point(alpha = 0.5, fill = 'orange', shape = 21,
             size = 2) +
  #lazy_add_heatmap() +
  facet_grid(court_side ~ serve_num) + 
  guides( fill = guide_colourbar( barheight = unit( 2 , "in" ) ))+
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Serve Bounces",
       caption = 'Data: 2021 Roland Garros'
  )

# -- Cut point importance into a categorical variable
hist(plot_data$point_importance)
summary(plot_data$point_importance)
plot_data$cat_point_importance <- ifelse(plot_data$point_importance >= quantile(plot_data$point_importance, 0.8),
                                         'High','Low')

plot_data %>%
  distinct() %>%
  #filter(court_side == 'AdCourt') %>%
  filter(x_coord > -2) %>%
  filter(x_coord < 11) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  #lazy_add_heatmap() +
  geom_point(alpha=0.5)+
  facet_grid(court_side ~ cat_point_importance,
             #labeller = labeller(court_side = courtside.labs,
                                 #cat_point_importance = cat_point_importance.labs)
) + 
  labs(x = "", 
       y = "",
       title = "",
       caption ='Data: Roland Garros 2021'
  )



# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Plot Return Locations        -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

#my_match_id <- 'year_2021_SM050_tracking_data.json'
plot_data <- training_data %>%
  filter(match_id == my_match_id) %>%
  filter(is_track_avail) %>%
  filter(server_name != player_name) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    x_new = impute_next_location(x1 = serve_return_impact_x,
                                 y1 = serve_return_impact_y, 
                                 x2 = serve_return_bounce_x, 
                                 y2 = serve_return_bounce_y,
                                 fwd = 1)[1],
    
    y_new = impute_next_location(x1 = serve_return_impact_x,
                                 y1 = serve_return_impact_y, 
                                 x2 = serve_return_bounce_x, 
                                 y2 = serve_return_bounce_y,
                                 fwd = 1)[2]
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
  #filter(x_coord > -2) %>%
  ggplot(aes(x = serve_return_bounce_x, 
             y = serve_return_bounce_y)) +
  draw_full_tennis_court() +
  geom_segment(aes(x=serve_return_bounce_x,
                   y=serve_return_bounce_y,
                   xend = x_new,
                   yend = y_new),
               arrow=arrow(length = unit(0.1,"cm"),
                           type = "closed",
                           ends = 'last'),
               #size=0.2,
               color="yellow",
               alpha = 0.7) +
  geom_point(alpha = 0.5, fill = 'green', shape = 21,
             size = 2.5) +
  #lazy_add_heatmap() +
  facet_grid(court_side ~ serve_num) + 
  guides( fill = guide_colourbar( barheight = unit( 2 , "in" ) ))+
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Serve Bounces",
       caption = 'Data: 2021 Roland Garros'
  )

courtside.labs <- c("Advantage Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")
servenum.labs <- c("1st Serve Returns", "2nd Serve Returns")
names(servenum.labs) <- c("1", "2")

plot_data %>%
  filter(x_coord > -0) %>%
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
             size = 2.5) +
  #lazy_add_heatmap() +
  facet_grid(court_side ~ serve_num,
             labeller = labeller(court_side = courtside.labs,
                                 serve_num = servenum.labs)) + 
  guides( fill = guide_colourbar( barheight = unit( 2 , "in" ) ))+
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Serena Williams Serve Returns against Collins",
       caption = 'Data: 2021 Roland Garros'
  ) 

ggsave('./plots/williams_collins_serve_returns.jpg',
       width=6, height=4,
       dpi = 400)




# -- Boxplot of net height
plot_data %>%
  mutate(serve_num = as.factor(serve_num)) %>%
  ggplot( aes(y = serve_return_net_z, 
              x = serve_num,
              fill=serve_num)) +
  geom_boxplot(alpha = 0.5) +
  facet_grid(~ court_side,)
             #labeller = labeller(serve_num = serve_num.labs)
  #            ) + 
  # peter_theme(family_font = 'Tahoma') + 
  # theme(strip.background =element_rect(fill="#f7e3c3"),
  #       strip.text.x = element_text(size = 10, face = 'bold'),
  #       legend.position = "none") +
  # labs(x = 'Serve Direction', y = 'Serve Return Net Height',
  #      fill = '',
  #      title = ""
  #      #caption = 'Data: Roland Garros\n2019-20'
  # )



# June 6th 2021 -----
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


courtside.labs <- c("Advantage Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")
server_name.labs <- c("Zverev Serve Locations", "Nishikori Serve Locations")
names(server_name.labs) <- c("A.ZVEREV", "K.NISHIKORI")

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

