# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----             Plot player serve location densities        -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")


# -- Draw a tennis court
draw_full_tennis_court <- function(){
  list(
    # -- Two outer doubles lines
    geom_rect(mapping=aes(xmin=-11.89, xmax=11.89, ymin=-5.485, ymax=-4.115), 
              color="black", alpha=0.5, fill = 'grey'), 
    geom_rect(mapping=aes(xmin=-11.89, xmax=11.89, ymin= 4.115, ymax= 5.485), 
              color="black", alpha=0.5, fill = 'grey'),
    # -- Service boxes
    geom_rect(mapping=aes(xmin=-6.4, xmax=0, ymin=-4.115, ymax=0), 
              color="black", alpha=0.5, fill = 'lightgreen'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=-4.115), 
              color="black", alpha=0.5, fill = 'lightgreen'),
    geom_rect(mapping=aes(xmin=-6.4, xmax=0, ymin=0, ymax= 4.115), 
              color="black", alpha=0.5, fill = 'lightgreen'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=4.115), 
              color="black", alpha=0.5, fill = 'lightgreen'),
    # -- Baseline
    geom_rect(mapping=aes(xmin=-11.89, xmax=-6.4, ymin=-4.115, ymax=4.115), 
              color="black", alpha=0.5, fill = 'white'),
    geom_rect(mapping=aes(xmin=6.4, xmax=11.89, ymin=-4.115, ymax=4.115), 
              color="black", alpha=0.5, fill = 'white'),
    
    # -- Emphasize the net
    geom_segment(aes(x = 0, xend = 0, y = -5.485, yend = 5.485),
                 size = 1.5), 
    labs(x = '', y = ''))
}

ggplot() + 
  draw_full_tennis_court()



draw_half_tennis_court <- function(){
  list(
    # -- Two outer doubles lines
    geom_rect(mapping=aes(xmin=-1, xmax=11.89, ymin=-5.485, ymax=-4.115), 
              color="black", alpha=0.5, fill = 'grey'), 
    geom_rect(mapping=aes(xmin=-1, xmax=11.89, ymin= 4.115, ymax= 5.485), 
              color="black", alpha=0.5, fill = 'grey'),
    # -- Service boxes
    geom_rect(mapping=aes(xmin=-1, xmax=0, ymin=-4.115, ymax=0), 
              color="black", size = 0.75, alpha=0.5, fill = 'lightgreen'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=-4.115), 
              color="black", size = 0.75, alpha=0.5, fill = 'lightgreen'),
    geom_rect(mapping=aes(xmin=-1, xmax=0, ymin=0, ymax= 4.115), 
              color="black", size = 0.75, alpha=0.5, fill = 'lightgreen'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=4.115), 
              color="black", size = 0.75, alpha=0.5, fill = 'lightgreen'),
    # -- Baseline
    #geom_rect(mapping=aes(xmin=-11.89, xmax=-6.4, ymin=-4.115, ymax=4.115), 
    #          color="black", alpha=0.5, fill = 'white'),
    geom_rect(mapping=aes(xmin=6.4, xmax=11.89, ymin=-4.115, ymax=4.115), 
              color="black", alpha=0.05, fill = 'lightgreen'),
    
    # -- Emphasize the net
    geom_segment(aes(x = 0, xend = 0, y = -5.485, yend = 5.485),
                 size = 1), 
    # -- Add dashed lines separating 3 serve locations
    geom_segment(aes(x = -1, xend = 6.4, y = 1.37, yend = 1.37),
                 size = 0.25, linetype='dashed', colour = '#505050'),
    geom_segment(aes(x = -1, xend = 6.4, y = 2*1.37, yend = 2*1.37),
                   size = 0.25, linetype='dashed', colour = '#505050'),
    geom_segment(aes(x = -1, xend = 6.4, y = -1.37, yend = -1.37),
                 size = 0.25, linetype='dashed', colour = '#505050'),
    geom_segment(aes(x = -1, xend = 6.4, y = -2*1.37, yend = -2*1.37),
                 size = 0.25, linetype='dashed', colour = '#505050'),
    labs(x = '', y = ''))
  
}

ggplot() + 
  draw_half_tennis_court()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

training_data <- read.csv('./processed_roland_garros_tracking_data.csv',
                          stringsAsFactors = FALSE)

players_of_interest <- c('R.NADAL', 'R.FEDERER', 'N.DJOKOVIC',
                         'D.THIEM', 'S.TSITSIPAS', 'A.ZVEREV',
                         'B.PAIRE', 'J.TSONGA', 'D.SHAPOVALOV')

players_of_interest <- c('R.NADAL', 'R.FEDERER', 'N.DJOKOVIC',
                         'D.THIEM', 'S.TSITSIPAS', 'A.ZVEREV')



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

ggsave('serve_loc_on_deuce.jpg',
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
ggsave('serve_loc_on_ad.jpg',
       width=7.25, height=5,
       dpi = 400)


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
plot_one_court_data %>%
  filter(x_coord < -8) %>%
  select(serveBounceCordinate_x, serveBounceCordinate_y, court_side, which_side, x_coord, y_coord) %>%
  View()


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
  theme_bw() + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", 
       y = "",
       title = "Men's Roland Garros 2019-20 Serve Locations\n on Advantage Court")

ggsave('atp_serve_loc_on_ad.jpg',
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
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  facet_wrap(~server_name) + 
  theme_bw() + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", 
       y = "",
       title = "Men'sRoland Garros 2019-20 Serve Locations on Deuce Court") 
  
ggsave('atp_serve_loc_on_deuce.jpg',
       width=7.25, height=4,
       dpi = 400)

# plot_one_court_data %>%
#   filter(server_name=='R.FEDERER') %>%
#   View()

# plot_one_court_data %>%
#   group_by(server_name, court_side) %>%
#   summarise(count=n())


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
#       Serve locations of right vs. left handed returners      -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

courtside.labs <- c("Advantage Court", "Deuce Court")
names(courtside.labs) <- c("AdCourt", "DeuceCourt")

handedness.labs <- c("Returner: Left-Handed", "Returner: Right-Handed")
names(handedness.labs) <- c("left-handed", "right-handed")

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
  theme_bw() + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x = "", 
       y = "",
       title = "Men's 2nd Serve Locations against Returner Handedness") 

ggsave('atp_serve_loc_against_returner_handedness.jpg',
       width=6, height=5,
       dpi = 400)

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
  theme_bw() + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", 
       y = "",
       title = 'Safe Serve Locations\n on Deuce Court') 


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
  theme_bw() + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", 
       y = "",
       title = 'Risky Serve Locations\n on Deuce Court') 




# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
#                  Serve locations on BreakPoint                -----    
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
  #filter(court_side =='DeuceCourt') %>%
  filter(x_coord > -2) %>%
  filter(is_break_point==1) %>%
  filter(server_name %in% players_of_interest) %>%
  ggplot(aes(x = x_coord, 
             y = y_coord)) +
  draw_half_tennis_court() +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon",
                  show.legend = F, 
                  bins = 15, 
                  alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10') +
  #facet_wrap(~ court_side + returner_hand ) + 
  facet_grid(court_side~server_name) + 
  theme_bw() + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x = "", 
       y = "",
       title = "Men's 2nd Serve Locations against Returner Handedness") 

ggsave('atp_serve_loc_against_returner_handedness.jpg',
       width=6, height=5,
       dpi = 400)


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -----               Repeat for WTA players                    -----    
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
players_of_interest <- c('S.KENIN', 'S.HALEP', 'C.GARCIA',
                         'E.SVITOLINA', 'A.BARTY', 'P.KVITOVA')
training_data <- read.csv('./processed_wta_roland_garros_tracking_data.csv',
                          stringsAsFactors = FALSE)

# -- WTA players serve location against Returner Handedness -----
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
  theme_bw() + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x = "", 
       y = "",
       title = "Women's 2nd Serve Locations against Returner Handedness") 

ggsave('wta_serve_loc_against_returner_handedness.jpg',
       width=6, height=5,
       dpi = 400)


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
  theme_bw() + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5))+
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
  theme_bw() + 
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'),
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", 
       y = "",
       title = 'Roland Garros 2019-20 Serve Locations on Deuce Court') 

ggsave('wta_serve_loc_on_deuce.jpg',
       width=7.25, height=4,
       dpi = 400)
