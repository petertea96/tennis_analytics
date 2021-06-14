# -- Investigate the anteroposterior and lateral variability of ball strike location on serve
# -- Ball position at impact: Lateral position from center mark and 
# -- Distance forward from baseline

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_project/")
atp_pbp_df <- read.csv('./collect_data/data/atp_processed_roland_garros_tracking_data.csv',
                       na.strings=c("","NA"))
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/ggplot_theme.R")


#players_of_interest <- c('R.NADAL', 'R.FEDERER', 'N.DJOKOVIC',
#                         'D.THIEM', 'S.TSITSIPAS', 'A.ZVEREV')


atp_plot_serve_impact_df <-
atp_pbp_df %>%
  filter(is_track_avail) %>%
  distinct() %>%
  #filter(server_name %in% players_of_interest) %>%
  #filter(server_hand == 'right-handed') %>%
  select(court_side, serve_num, server_hand,
         x_ball_at_serve, y_ball_at_serve, z_ball_at_serve,
         which_side, intended_serve_dir, server_name,
         serve_impact_from_center) %>%
  mutate(dist_inside_baseline = ifelse(abs(x_ball_at_serve) > 11.88, 0 ,
                                       11.88 - abs(x_ball_at_serve)))


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- How much do ATP players step inside the court? -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Tenuous atm but...
#   * Wide serves tend to have largest distance inside baseline, followed by T and Body
#   * Players jump more inside the baseline on Ad court and on 1st serve

atp_plot_serve_impact_df %>%
  filter(abs(dist_inside_baseline) < 2) %>%
  filter(!is.na(intended_serve_dir)) %>%
  filter(server_hand == 'right-handed') %>%
  ggplot( aes(x = dist_inside_baseline, fill=as.factor(intended_serve_dir))) +
  geom_density( aes(y = ..density.., fill=as.factor(intended_serve_dir)),
                alpha = 0.5)

atp_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(abs(dist_inside_baseline) < 5) %>%
  ggplot( aes(x = dist_inside_baseline, fill=as.factor(serve_num))) +
  geom_density( aes(y = ..density.., fill=as.factor(serve_num)),
                alpha = 0.3)



atp_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(abs(dist_inside_baseline) < 5) %>%
  ggplot( aes(x = dist_inside_baseline, fill=as.factor(court_side))) +
  geom_density( aes(y = ..density.., fill=as.factor(court_side)),
                alpha = 0.3)


atp_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(abs(dist_inside_baseline) < 5) %>%
  ggplot( aes(x = dist_inside_baseline, fill=as.factor(serve_num))) +
  geom_density( aes(y = ..density.., fill=as.factor(serve_num)),
                alpha = 0.3
  )


# -- boxplot
serve_num.labs <- c("First Serve", "Second Serve")
names(serve_num.labs) <- c(1, 2)
atp_plot_serve_impact_df$intended_serve_dir <- factor(atp_plot_serve_impact_df$intended_serve_dir,
                                                      levels = c('T', 'Body', 'Wide'))
atp_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(abs(dist_inside_baseline) < 5) %>%
  #filter(court_side == 'DeuceCourt') %>%
  filter(!is.na(intended_serve_dir)) %>%
  mutate(serve_num = as.factor(serve_num)) %>%
  ggplot( aes(y = dist_inside_baseline, 
              x = intended_serve_dir,
              fill=intended_serve_dir)) +
  geom_boxplot(alpha = 0.5) +
  # facet_grid(~ serve_num,
  #            labeller = labeller(serve_num = serve_num.labs)) + 
  facet_grid(~ court_side) + 
  peter_theme(family_font = 'Tahoma') + 
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text.x = element_text(size = 10, face = 'bold'),
        legend.position = "none") #+
  # labs(x = 'Lateral Ball Position at\nServe Impact', y = 'Lateral Position\n(Metres)',
  #      fill = '',
  #      title = "Right-Handed Men's Impact\n Position on Deuce Court"
  #      #caption = 'Data: Roland Garros\n2019-20'
  # )

atp_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(abs(dist_inside_baseline) < 5) %>%
  ggplot( aes(x = dist_inside_baseline, fill=as.factor(serve_num))) +
  geom_boxplot( aes(y = ..density.., fill=as.factor(serve_num)),
                alpha = 0.3)

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- How much lateral movement is there on ATP serves ? -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

# -- Look at darn outleirs...

atp_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(serve_impact_from_center > 3) %>%
  View()


serve_num.labs <- c("First Serve", "Second Serve")
names(serve_num.labs) <- c(1, 2)

# -- Histograms
atp_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(court_side == 'DeuceCourt') %>%
  filter(!is.na(intended_serve_dir)) %>%
  filter(serve_impact_from_center < 3) %>%
  mutate(serve_num = as.factor(serve_num)) %>%
  ggplot( aes(x = serve_impact_from_center, fill=as.factor(intended_serve_dir))) +
  geom_density( aes(y = ..density.., fill=as.factor(intended_serve_dir)),
                alpha = 0.5) +
  facet_grid(~ serve_num,
             labeller = labeller(serve_num = serve_num.labs)) + 
  peter_theme(family_font = 'Tahoma') + 
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  labs(x = 'Lateral Ball Position at\nServe Impact (Metres)', y = 'Density',
       fill = '',
       title = "Right-Handed Men's Impact\n Position on Deuce Court"
       #caption = 'Data: Roland Garros\n2019-20'
       )



# -- Boxplots
atp_plot_serve_impact_df$intended_serve_dir <- factor(atp_plot_serve_impact_df$intended_serve_dir,
                                                      levels = c('T', 'Body', 'Wide'))
atp_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(court_side == 'DeuceCourt') %>%
  filter(!is.na(intended_serve_dir)) %>%
  filter(serve_impact_from_center < 3) %>%
  mutate(serve_num = as.factor(serve_num)) %>%
  ggplot( aes(y = serve_impact_from_center, 
              x = intended_serve_dir,
              fill=intended_serve_dir)) +
  geom_boxplot(alpha = 0.5) +
  facet_grid(~ serve_num,
             labeller = labeller(serve_num = serve_num.labs)) + 
  peter_theme(family_font = 'Tahoma') + 
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text.x = element_text(size = 10, face = 'bold'),
        legend.position = "none") +
  labs(x = 'Serve Direction', y = 'Lateral Position\n(Metres)',
       fill = '',
       title = "Right-Handed Men's Impact\n Position on Deuce Court"
       #caption = 'Data: Roland Garros\n2019-20'
  )

ggsave('lateral_displacement_serve_deuce_atp.jpg',
       width=5, height=3.5,
       dpi = 280)



atp_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(court_side == 'AdCourt') %>%
  filter(!is.na(intended_serve_dir)) %>%
  filter(serve_impact_from_center < 3) %>%
  mutate(serve_num = as.factor(serve_num)) %>%
  ggplot( aes(x = serve_impact_from_center, fill=as.factor(intended_serve_dir))) +
  geom_density( aes(y = ..density.., fill=as.factor(intended_serve_dir)),
                alpha = 0.5) +
  facet_grid(~ serve_num,
             labeller = labeller(serve_num = serve_num.labs)) + 
  peter_theme(family_font = 'Tahoma') +
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  labs(x = 'Lateral Ball Displacement at\nServe Impact (M)', y = 'Density',
       fill = '',
       title = 'ATP Right-Hander Serve Lateral\nDisplacement on Ad. Court',
       caption = 'Data: Roland Garros\n2019-20')


ggsave('lateral_displacement_serve_ad_atp.jpg',
       width=7, height=5,
       dpi = 400)

# -- What about for left handers (I.e. Rafa): Same Deal!!

atp_plot_serve_impact_df %>%
  filter(server_hand == 'left-handed') %>%
  filter(server_name == 'R.NADAL') %>%
  filter(court_side == 'DeuceCourt') %>%
  filter(!is.na(intended_serve_dir)) %>%
  filter(serve_impact_from_center < 3) %>%
  mutate(serve_num = as.factor(serve_num)) %>%
  ggplot( aes(x = serve_impact_from_center, fill=as.factor(intended_serve_dir))) +
  geom_density( aes(y = ..density.., fill=as.factor(intended_serve_dir)),
                alpha = 0.5) +
  facet_grid(~ serve_num,
             labeller = labeller(serve_num = serve_num.labs)) + 
  peter_theme(family_font = 'Tahoma') + 
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  labs(x = 'Lateral Ball Displacement at\nServe Impact (M)', y = 'Density',
       fill = '',
       title = 'ATP Left-Hander Serve Lateral\nDisplacement on Deuce Court',
       caption = 'Data: Roland Garros\n2019-20')



atp_plot_serve_impact_df %>%
  filter(server_hand == 'left-handed') %>%
  filter(server_name == 'R.NADAL') %>%
  filter(court_side == 'AdCourt') %>%
  filter(!is.na(intended_serve_dir)) %>%
  filter(serve_impact_from_center < 3) %>%
  mutate(serve_num = as.factor(serve_num)) %>%
  ggplot( aes(x = serve_impact_from_center, fill=as.factor(intended_serve_dir))) +
  geom_density( aes(y = ..density.., fill=as.factor(intended_serve_dir)),
                alpha = 0.5) +
  facet_grid(~ serve_num,
             labeller = labeller(serve_num = serve_num.labs)) + 
  peter_theme(family_font = 'Tahoma') +
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  labs(x = 'Lateral Ball Displacement at\nServe Impact (M)', y = 'Density',
       fill = '',
       title = 'ATP Left-Hander Serve Lateral\nDisplacement on Ad. Court',
       caption = 'Data: Roland Garros\n2019-20')


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- Repeat for WTA players -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
wta_pbp_df <- read.csv('./collect_data/data/wta_processed_roland_garros_tracking_data.csv',
                       na.strings=c("","NA"))

wta_plot_serve_impact_df <-
  wta_pbp_df %>%
  filter(is_track_avail) %>%
  #filter(server_name %in% players_of_interest) %>%
  #filter(server_hand == 'right-handed') %>%
  select(court_side, serve_num, server_hand,
         x_ball_at_serve, y_ball_at_serve, z_ball_at_serve,
         which_side, intended_serve_dir, server_name,
         serve_impact_from_center) %>%
  mutate(dist_inside_baseline = ifelse(abs(x_ball_at_serve) > 11.88, 0 ,
                                       11.88 - abs(x_ball_at_serve)))



wta_plot_serve_impact_df %>%
  filter(server_hand == 'right-handed') %>%
  filter(court_side == 'DeuceCourt') %>%
  filter(!is.na(intended_serve_dir)) %>%
  filter(serve_impact_from_center < 3) %>%
  mutate(serve_num = as.factor(serve_num)) %>%
  ggplot( aes(x = serve_impact_from_center, fill=as.factor(intended_serve_dir))) +
  geom_density( aes(y = ..density.., fill=as.factor(intended_serve_dir)),
                alpha = 0.5) +
  facet_grid(~ serve_num,
             labeller = labeller(serve_num = serve_num.labs)) + 
  peter_theme(family_font = 'Tahoma') + 
  theme(strip.background =element_rect(fill="#f7e3c3"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  labs(x = 'Lateral Ball Displacement at\nServe Impact (M)', y = 'Density',
       fill = '',
       title = 'WTA Right-Hander Serve Lateral\nDisplacement on Deuce Court',
       caption = 'Data: Roland Garros\n2019-20')



# -- Player differences in ball impact location -----
wta_plot_serve_impact_df %>%
  group_by(server_name) %>%
  summarise(mean_long = mean(dist_inside_baseline),
            mean_lat = mean(serve_impact_from_center)) %>%
  arrange(desc(mean_long)) %>%
  View()

wta_players <- c('N.OSAKA', 'I.SWIATEK', 'S.WILLIAMS', 'V.AZARENKA',
                 'S.KENIN', 'P.KVITOVA')

plot_data <- wta_plot_serve_impact_df %>%
  filter(server_name %in% wta_players) %>%
  filter(serve_impact_from_center<5) 

plot_data$server_name <- factor(plot_data$server_name,
                                   levels = rev(c('I.SWIATEK', 'P.KVITOVA', 'V.AZARENKA',
                                              'N.OSAKA', 'S.WILLIAMS', 'S.KENIN')))

plot_data %>%
  ggplot( aes(x = serve_impact_from_center, y = server_name)) +
  geom_boxplot(fill = 'indianred')  +
  labs(x = "Lateral Impact (Metres)", 
       y = "",
       title = 'Women Serve Lateral Impact',
       caption = 'Roland Garros\n2019-20'
  ) + 
  peter_theme(family_font = 'Tahoma')


plot_data2 <- wta_plot_serve_impact_df %>%
  filter(server_name %in% wta_players) %>%
  filter(dist_inside_baseline<5) 

plot_data2$server_name <- factor(plot_data2$server_name,
                                levels = rev(c('I.SWIATEK', 'P.KVITOVA', 'V.AZARENKA',
                                               'N.OSAKA', 'S.WILLIAMS', 'S.KENIN')))
plot_data2 %>%
  filter(server_name %in% wta_players) %>%
  filter(dist_inside_baseline<5) %>%
  ggplot( aes(x = dist_inside_baseline, y = server_name)) +
  geom_boxplot(fill = 'indianred') +
  labs(x = "Longitudinal Impact (Metres)", 
       y = "",
       title = 'Women Serve Longitudinal Impact',
       caption = 'Roland Garros\n2019-20'
  ) + 
  peter_theme(family_font = 'Tahoma')



# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# -- How much do players vary their serve impact location? -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
result_df <-
atp_plot_serve_impact_df %>%
  filter(abs(dist_inside_baseline) < 5) %>%
  filter(serve_impact_from_center < 3) %>%
  group_by(server_name, court_side) %>%
  summarise(avg_long = mean(dist_inside_baseline),
            sd_long = sd(dist_inside_baseline),
            avg_lat = mean(serve_impact_from_center),
            sd_lat = sd(serve_impact_from_center)
            )


result_df %>%
  arrange(desc(avg_long)) %>%
  View()

result_df %>%
  arrange(desc(avg_lat)) %>%
  View()

result_df %>%
  arrange(desc(sd_long)) %>%
  View()

result_df %>%
  arrange(desc(sd_lat)) %>%
  View()

