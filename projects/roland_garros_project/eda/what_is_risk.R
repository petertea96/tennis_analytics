# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# --                EDA for Quantifying Risky Serves            -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# Serve Speed densities by Serve Number
# Serve Speed densities by Faults
# Serve Speed vs Net clearance
# Serve Net Clearance densities

library(dplyr)
library(ggplot2)
library(ggridges)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_project/")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_project/src/ggplot_theme.R")

training_data <- read.csv('./collect_data/data/atp_processed_roland_garros_tracking_data.csv',
                          stringsAsFactors = FALSE)


table(training_data$server_name)

# R.FEDERER
# D.THIEM
# S.TSITSIPAS
# A.ZVEREV
# N.DJOKOVIC
# B.PAIRE
# D.SHAPOVALOV
# J.TSONGA

players_of_interest <- c('R.NADAL', 'R.FEDERER', 'N.DJOKOVIC',
                         'D.THIEM', 'S.TSITSIPAS', 'A.ZVEREV'
                         #'B.PAIRE', 'J.TSONGA', 'D.SHAPOVALOV'
                         )


# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# --                    Serve Speed                             -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

plot_speed_data <- training_data %>%
  #filter(serve_speed_kph > 0) %>%
  filter(serve_speed_kph > 100) %>%
  #filter(serve_num == 1) %>% 
  filter(server_name %in% players_of_interest)

avg_speed <- plot_speed_data %>%
  group_by(server_name) %>%
  summarise(avg_speed = mean(serve_speed_kph))

plot_speed_data$server_name <- factor(plot_speed_data$server_name,
                                      levels = avg_speed$server_name[order(avg_speed$avg_speed)]
                                      )
# -- Plot against Serve Number
ggplot(data = plot_speed_data,
       aes(x = serve_speed_kph, y = server_name)) + 
  geom_density_ridges(aes(fill = as.factor(serve_num)),
                      alpha = 0.5) +
  labs(x = "Speed (KM/H)", 
       y = "",
       title = 'Serve Speed Densities',
       fill = 'Serve Number'#,
       #caption = 'Roland Garros\n2019-20'
       ) +
  peter_theme(family_font = 'Tahoma')

ggsave('./eda/plots/serve_speeds.jpg',
       width=6, height=4,
       dpi = 300)


ggplot(data = plot_speed_data %>% filter(server_name == 'N.DJOKOVIC'),
       aes(x = serve_speed_kph)) + 
  geom_histogram(aes(fill = as.factor(serve_num)),
                      alpha = 0.5) +
  labs(x = "Speed (KM/H)", 
       y = "",
       title = 'Serve Speed Densities',
       fill = 'Serve Number',
       caption = 'Roland Garros\n2019-20') +
  peter_theme(family_font = 'Tahoma')
ggplot(data = plot_speed_data %>% filter(server_name == 'D.THIEM'),
       aes(x = serve_speed_kph)) + 
  geom_histogram(aes(fill = as.factor(serve_num)),
                 alpha = 0.5) +
  labs(x = "Speed (KM/H)", 
       y = "",
       title = 'Serve Speed Densities',
       fill = 'Serve Number',
       caption = 'Roland Garros\n2019-20') +
  peter_theme(family_font = 'Tahoma')


# -- Plot against Fault indicators
ggplot(data = plot_speed_data,
       aes(x = serve_speed_kph, y = server_name)) + 
  geom_density_ridges(aes(fill = as.factor(is_fault)),
                      alpha = 0.5) +
  labs(x = "Speed (KM/H)", 
       y = "Server",
       title = 'Serve Speed Densities',
       fill = 'Is Fault')
ggsave('speed_faults.jpg',
       width=7.25, height=5,
       dpi = 400)  
  
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# --                    Net Clearance                           -----
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

# -- Serve Speed vs. Net clearance
training_data %>%
  filter(serve_speed_kph > 0) %>%
  filter(serve_speed_kph > 100) %>%
  #filter(serve_num == 1) %>% 
  filter(server_name %in% players_of_interest) %>%
  ggplot(aes(x = serve_speed_kph, y = z_net_serve- 1.07)) +
  geom_point(alpha = 0.3) + 
  facet_wrap(~server_name) + 
  #geom_hline(yintercept = 0, show.legend = TRUE, linetype = "dashed", color = "red") +
  labs(x = "Serve Speed (KM/H)", 
       y = "Net Clearance (m)",
       title = 'Serve Net Clearance vs. Serve Speed',
       fill = 'Serve Number')

ggsave('speed_vs_net_clearance.jpg',
       width=7.25, height=5,
       dpi = 400)

plot_net_clearance_data <- training_data %>%
  filter(serve_speed_kph > 0) %>%
  filter(serve_speed_kph > 100) %>%
  #filter(serve_num == 1) %>% 
  filter(server_name %in% players_of_interest)


plot_net_clearance_data$server_name <- factor(plot_net_clearance_data$server_name,
                                      levels = avg_speed$server_name[order(avg_speed$avg_speed)]
)

ggplot(data = plot_net_clearance_data,
       aes(x = z_net_serve - 1.07, y = server_name)) + 
  geom_density_ridges(aes(fill = as.factor(serve_num)),
                      alpha = 0.5) +
  scale_x_continuous(limits = c(-1, 1)) + 
  geom_vline(xintercept = 0, show.legend = TRUE, linetype = "dashed", color = "black") +
  labs(x = "Net Clearance (m)", 
       y = "Server",
       title = 'Serve Net Clearance Densities',
       fill = 'Serve Number')
ggsave('net_clearance.jpg',
       width=7.25, height=5,
       dpi = 400)


  
  


