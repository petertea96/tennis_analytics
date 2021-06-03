# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Plot player serve location densities        -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

library(dplyr)
library(ggplot2)
source('/Users/petertea/tennis_analytics/projects/roland_garros_project/src/gg_tennis_court.R')

training_data <- read.csv('/Users/petertea/tennis_analytics/projects/roland_garros_project/2021/processed_roland_garros_2021.csv',
                          stringsAsFactors = FALSE)


my_match_id <- 'wta_year_2021_SD048_tracking_data.json'
player_name <- 'S.WILLIAMS'

plot_data <- training_data %>%
  filter(match_id == my_match_id) %>%
  filter(is_track_avail) %>%
  filter(server_name == player_name) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_x,
                            serveBounceCordinate_x),
          
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_y,
                            serveBounceCordinate_y))


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
       title = "",
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
  filter(returner_name == player_name)  %>%
  mutate( x_coord = ifelse( (which_side == 'left'), 
                            -1 *serve_return_bounce_x,
                            serve_return_bounce_x),
          
          y_coord = ifelse( (which_side == 'left'), 
                            -1 *serve_return_bounce_y,
                            serve_return_bounce_y))


plot_data %>%
  distinct() %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  geom_point(alpha=0.5)+
  facet_grid(court_side ~ serve_num,
             #labeller = labeller(court_side = courtside.labs,
             #cat_point_importance = cat_point_importance.labs)
  )




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
