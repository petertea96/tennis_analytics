# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Plot player serve location densities        -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_project/")
source('src/gg_tennis_court.R')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

# *** NOTE ***
# --> ServeBounce are actual bounce locations
# --> intended_serve_bounce are imputed serve directions on net faults

training_data <- read.csv('./collect_data/data/atp_processed_roland_garros_tracking_data.csv',
                          stringsAsFactors = FALSE)
# training_data %>% filter(is_track_avail) %>% distinct() %>% nrow()
# training_data %>%
#   filter(is_track_avail) %>%
#   filter(returner_hand=='left-handed') %>%
#   group_by(returner_name) %>%
#   summarise(count = n())

players_of_interest <- c('R.NADAL', 'R.FEDERER', 'N.DJOKOVIC',
                         'D.THIEM', 'S.TSITSIPAS', 'A.ZVEREV',
                         'B.PAIRE', 'J.TSONGA', 'D.SHAPOVALOV')

players_of_interest <- c('R.NADAL',  'N.DJOKOVIC','R.FEDERER',
                         'D.THIEM', 'S.TSITSIPAS', 'A.ZVEREV')


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----                  Plots on Half Court                    -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
plot_one_court_data <- training_data %>%
  filter(is_track_avail) %>%
  filter(server_name %in% players_of_interest) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_x,
                            serveBounceCordinate_x),
          
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_y,
                            serveBounceCordinate_y))

# Note: Something weird happening. Seems like some serves are hit backwards...
summary(plot_one_court_data$x_coord)
# plot_one_court_data %>%
#   filter(x_coord < -8) %>%
#   select(serveBounceCordinate_x, serveBounceCordinate_y, court_side, which_side, x_coord, y_coord) %>%
#   View()

# -- Quick check how many serve obersations there are for each player
plot_one_court_data %>%
  group_by(server_name) %>%
  summarise(n())


# -- Change order of players
plot_one_court_data$server_name <- factor(plot_one_court_data$server_name,
                                          levels = players_of_interest)

plot_one_court_data %>%
  filter(court_side =='AdCourt') %>%
  #filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = TRUE, 
                  #bins = 15, 
                  alpha = .8
                  )+
  scale_fill_gradientn(colours = c('white','pink1', 'red4')) +
  #, trans = 'log10'
  facet_wrap(~server_name, nrow = 2) + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'right',
        legend.direction = 'vertical',
        legend.margin=margin(1,1,1,1),
        plot.caption = element_text(vjust = 6, face = "italic")
        )+
  guides( fill = guide_colourbar( barheight = unit( 2 , "in" ) ))+
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Men's 1st and 2nd Serve Locations on Advantage Court",
       caption = 'Data: 2019 & 2020 Roland Garros'
       )

ggsave('./eda/plots/atp_serve_loc_on_ad.jpg',
       width=6, height=3.5,
       dpi = 240)

plot_one_court_data %>%
  filter(court_side =='DeuceCourt') %>%
  #filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = TRUE, 
                  #bins = 15, 
                  alpha = .8)+
  scale_fill_gradientn(colours = c('white','pink1', 'red4')) +
  #, trans = 'log10'
  facet_wrap(~server_name) + 
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'right',
        legend.direction = 'vertical',
        legend.margin=margin(1,1,1,1),
        plot.caption = element_text(vjust = 6, face = "italic")
        )+
  guides( fill = guide_colourbar( barheight = unit( 2 , "in" ) ))+
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Men's 1st & 2nd Serve Locations on Deuce Court",
       caption = 'Data: 2019 & 2020 Roland Garros') 
  
ggsave('./eda/plots/atp_serve_loc_on_deuce.jpg',
       width=6, height=3.5,
       dpi = 240)

# -- Why does it seem like Sasha doesn;t fault on Deuce?
training_data %>%
  filter(server_name == 'A.ZVEREV') %>%
  group_by(court_side) %>%
  summarise(sum(is_fault))

plot_one_court_data %>%
  filter(court_side =='DeuceCourt') %>%
  filter(server_name == 'A.ZVEREV') %>%
  #filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  geom_point(alpha = 0.1, colour = 'red') 


# Interpreting legend -----
# https://stackoverflow.com/questions/53172200/stat-density2d-what-does-the-legend-mean?noredirect=1&lq=1
# Basically, the algorithm forms a bunch of binning grids and colours each grid
# corresponding to the number of points falling within the grid.
# nlevel: https://ggplot2.tidyverse.org/reference/geom_contour.htm

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
#       Serve locations of right vs. left handed returners      -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

courtside.labs <- c("Advantage Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")

handedness.labs <- c("vs. Left-Handed Returners", "vs. Right-Handed Returners")
names(handedness.labs) <- c("left-handed", "right-handed")


# training_data$court_side <- factor(training_data$court_side,
#                                    levels = c('DeuceCourt', 'AdCourt'))

training_data$court_side <- factor(training_data$court_side,
                                   levels = c('DeuceCourt', 'AdCourt'))

courtside.labs <- c("Ad. Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")

training_data %>%
  filter(is_track_avail) %>%
  #filter(server_name %in% players_of_interest) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *intended_serve_bounce_x,
                            intended_serve_bounce_x),
          
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *intended_serve_bounce_y,
                            intended_serve_bounce_y)) %>%
  #filter(court_side =='DeuceCourt') %>%
  filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  filter(x_coord < 11) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  facet_grid(court_side ~ returner_hand,
             labeller = labeller(court_side = courtside.labs,
                                 returner_hand = handedness.labs)) + 
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "Men's 2nd Serve Locations",
       caption = 'Data: Roland Garros 2019-20'
       ) 

ggsave('./eda/plots/atp_serve_loc_against_returner_handedness.jpg',
       width=5, height=4,
       dpi = 280)


# -- Serve location against Nadal
nadal.labs <- c("vs. Nadal", "vs. Every Other Lefty")
names(nadal.labs) <- c("FALSE", "TRUE")

nadal_plot_comp_data <- training_data %>%
  filter(is_track_avail) %>%
  #filter(server_name %in% players_of_interest) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *intended_serve_bounce_x,
                            intended_serve_bounce_x),
          
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *intended_serve_bounce_y,
                            intended_serve_bounce_y)) %>%
  #filter(court_side =='DeuceCourt') %>%
  filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  filter(x_coord < 11) %>%
  filter(returner_hand == 'left-handed') %>%
  mutate(not_nadal = ifelse(!(returner_name == 'R.NADAL'), TRUE, FALSE)) 



nadal_plot_comp_data %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  lazy_add_heatmap() +
  facet_grid(court_side ~ not_nadal,
             labeller = labeller(court_side = courtside.labs,
                                 not_nadal = nadal.labs)) + 
  labs(x = "", 
       y = "",
       fill = 'nLevel',
       title = "ATP 2nd Serve Locations against Lefties",
       caption = 'Data: Roland Garros 2019-20') 

ggsave('./eda/plots/atp_serve_loc_against_returner_handedness_not_nadal.jpg',
       width=7, height=5,
       dpi = 240)

training_data %>%
  filter(is_track_avail) %>%
  filter(serve_num == 2) %>%
  filter(returner_hand == 'left-handed') %>%
  mutate(is_nadal = ifelse(returner_name == 'R.NADAL', TRUE, FALSE)) %>%
  group_by(is_nadal) %>%
  summarise(count = n())

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Heat maps of net clearance                  -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
plot_one_court_data %>%
  filter(court_side =='DeuceCourt') %>%
  filter(z_net_serve > 1.21) %>%
  #filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  facet_wrap(~server_name) + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  labs(x = "", 
       y = "",
       title = 'Serve Locations of High Net Clearance \n on Deuce Court') 


plot_one_court_data %>%
  filter(court_side =='DeuceCourt') %>%
  filter(z_net_serve <= 1.21) %>%
  #filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  facet_wrap(~server_name) + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", 
       y = "",
       title = 'Serve Locations of Low Net Clearance\n on Deuce Court') 




# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
#                  Serve locations on BreakPoint                -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Should we only look at Adcourt?
training_data %>%
  filter(is_track_avail) %>%
  filter(is_break_point ==1) %>%
  filter(server_name %in% players_of_interest) %>%
  group_by(court_side, serve_num) %>%
  summarise(count = n())


training_data %>%
  filter(is_track_avail) %>%
  #filter(server_name %in% players_of_interest) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_x,
                            serveBounceCordinate_x),
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_y,
                            serveBounceCordinate_y)) %>%
  #filter(court_side =='DeuceCourt') %>%
  filter(x_coord > -2) %>%
  filter(is_break_point==1) %>%
  filter(court_side == 'AdCourt') %>%
  filter(server_name %in% players_of_interest) %>%
  #filter(returner_hand == 'right-handed') %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  facet_wrap(~ server_name) + 
  #facet_grid(serve_num~server_name) + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  labs(x = "", 
       y = "",
       title = "ATP Serve Locations on Break Point\n (Ad Court only)") 

ggsave('atp_serve_loc_on_break_point.jpg',
       width=6, height=5,
       dpi = 400)


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Plots on 1st & 2nd Serve                    -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

training_data %>%
  filter(is_track_avail) %>%
  #filter(server_name %in% players_of_interest) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_x,
                            serveBounceCordinate_x),
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_y,
                            serveBounceCordinate_y)) %>%
  filter(x_coord > -2) %>%
  filter(server_name %in% players_of_interest) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  facet_grid(serve_num~server_name) + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  labs(x = "", 
       y = "",
       title = "ATP Serve Locations") 

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----                  Plots on Full Court                    -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
plot_serve_loc_data_deuce <- training_data %>%
  filter(is_track_avail) %>%
  filter(court_side == 'DeuceCourt') %>%
  filter(server_name %in% players_of_interest)

plot_serve_loc_data_ad <- training_data %>%
  filter(is_track_avail) %>%
  filter(court_side == 'AdCourt') %>%
  filter(server_name %in% players_of_interest)

# -- Plot serve location density
ggplot(data = plot_serve_loc_data_deuce,
       aes(x = serveBounceCordinate_x, 
           y = serveBounceCordinate_y)) +
  draw_full_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  facet_wrap(~server_name) + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", 
       y = "",
       title = 'Serve Locations on Deuce Court')

ggsave('fullcourt_serve_loc_on_deuce.jpg',
       width=7.25, height=5,
       dpi = 400)


ggplot(data = plot_serve_loc_data_ad,
       aes(x = serveBounceCordinate_x, 
           y = serveBounceCordinate_y)) +
  draw_full_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  facet_wrap(~server_name) + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", 
       y = "",
       title = 'Serve Locations on Advantage Court')
ggsave('fullcourt_serve_loc_on_ad.jpg',
       width=7.25, height=5,
       dpi = 400)




# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Imputed serve directions                    -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
plot_imputed_data <- training_data %>%
  filter(is_track_avail) %>%
  filter(server_name %in% players_of_interest) %>%
  mutate( 
          # -- Transform all coordinates to lie on the right part of the court.
          # Is it appropriate to take the absolute value?
          # On the right side of court, x_coord is always positive. However, y coord can be
          # (+) on Deuce or (-) on AdCourt
          x_coord = ifelse( (which_side == 'right'),
                            abs(intended_serve_bounce_x),
                            intended_serve_bounce_x),
          # # Something funky with some left side serves (all faults) being recorded as highly (-).
          x_coord = ifelse( ((which_side == 'left') & (x_coord < 0)),
                            abs(x_coord),
                            x_coord),
          y_coord = ifelse( (which_side == 'right'),
                            -1*(intended_serve_bounce_y),
                            intended_serve_bounce_y)
          
          )

plot_imputed_data %>%
  filter(error_type == 'Net Error') %>%
  select(intended_serve_bounce_x, intended_serve_bounce_y) %>%
  View()

plot_imputed_data %>%
  filter(error_type == 'Net Error') %>%
  filter(x_coord < 12)%>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  labs(x = "", 
       y = "",
       title = 'All Imputed Net Fault Locations')

ggsave('imputed_locations.jpg',
       width=7.25, height=5,
       dpi = 400)

summary(atp_rolandgarros_training_data_with_importance$x_coord)
hist((atp_rolandgarros_training_data_with_importance$x_coord))



# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----   Re-do all plots with intended serve direction         -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
plot_one_court_data <- training_data %>%
  filter(is_track_avail) %>%
  filter(server_name %in% players_of_interest) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *intended_serve_bounce_x,
                            intended_serve_bounce_x),
          
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *intended_serve_bounce_y,
                            intended_serve_bounce_y))


plot_one_court_data %>%
  filter(court_side =='DeuceCourt') %>%
  #filter(serve_num == 2) %>%
  #filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  facet_wrap(~server_name) + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  labs(x = "", 
       y = "",
       title = "Men's Serve Locations on Deuce Court",
       caption = "Data: Roland Garros 2019-20") 

ggsave('atp_serve_loc_on_deuce.jpg',
       width=7.25, height=4,
       dpi = 400)
