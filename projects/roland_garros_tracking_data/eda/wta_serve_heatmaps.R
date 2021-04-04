# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----               Repeat for WTA players                    -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- WTA serve Heatmaps 
# -- Exploratory Data Analysis


library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")
source('src/gg_tennis_court.R')


courtside.labs <- c("Advantage Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")

handedness.labs <- c("vs. Left-Handed Returners", "vs. Right-Handed Returners")
names(handedness.labs) <- c("left-handed", "right-handed")

players_of_interest <- c('S.KENIN', 'S.HALEP', 'C.GARCIA',
                         'E.SVITOLINA', 'A.BARTY', 'P.KVITOVA')
training_data <- read.csv('./processed_wta_roland_garros_tracking_data.csv',
                          stringsAsFactors = FALSE)

training_data <-
training_data %>%
  filter(is_track_avail) %>%
  #filter(server_name %in% players_of_interest) %>%
  mutate( x_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_x,
                            serveBounceCordinate_x),
          y_coord = ifelse( (which_side == 'right'), 
                            -1 *serveBounceCordinate_y,
                            serveBounceCordinate_y)) 

training_data %>%
  filter(x_coord < 0) %>%
  View()


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- WTA players serve location against Returner Handedness -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
training_data %>%
  filter(is_track_avail) %>%
  filter(serve_num == 2) %>%
  group_by(returner_hand) %>%
  summarise(count = n())

# training_data %>%
#   filter(returner_hand == 'left-handed') %>%
#   View()


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
  filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  #facet_wrap(~ court_side + returner_hand ) + 
  facet_grid(court_side ~ returner_hand,
             labeller = labeller(court_side = courtside.labs,
                                 returner_hand = handedness.labs)) + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  labs(x = "", 
       y = "",
       title = "WTA 2nd Serve Locations",
       caption = 'Data: Roland Garros 2019-20') 


ggsave('wta_serve_loc_against_returner_handedness.jpg',
       width=7, height=5,
       dpi = 400)



# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----               General Serve Locations                   -----    
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

plot_one_court_data %>%
  filter(court_side =='AdCourt') %>%
  #filter(serve_num == 2) %>%
  #filter(returner_hand == 'left-handed') %>%
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
       title = 'Roland Garros 2019-20 Serve Locations on Advantage Court')

ggsave('wta_serve_loc_on_ad.jpg',
       width=7.25, height=4,
       dpi = 400)

plot_one_court_data %>%
  filter(court_side =='DeuceCourt') %>%
  #filter(serve_num == 2) %>%
  filter(x_coord > -2) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .4)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  facet_wrap(~server_name) + 
  theme(strip.background =element_rect(fill="#f7e3c3")) +
  labs(x = "", 
       y = "",
       title = 'Roland Garros 2019-20 Serve Locations on Deuce Court') 

ggsave('wta_serve_loc_on_deuce.jpg',
       width=7.25, height=4,
       dpi = 400)




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
       title = "WTA Serve Locations") 

