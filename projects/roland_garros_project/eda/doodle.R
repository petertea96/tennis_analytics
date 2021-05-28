### Doodle script
## Peter Tea
## Last Updated: March 16th, 2021
# This script is for EDA

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

atp_pbp_df <- read.csv('atp_processed_roland_garros_tracking_data.csv')

# -- which players appear most frequently in the data?
sort(table(atp_pbp_df$server_name),decreasing = TRUE) %>%
  head(10)

tsitsipas_df <- atp_pbp_df %>%
  filter(server_name == 'S.TSITSIPAS')

thiem_df <- atp_pbp_df %>%
  filter(server_name == 'D.THIEM')

nadal_df <- atp_pbp_df %>%
  filter(server_name == 'R.NADAL')

djokovic_df <- atp_pbp_df %>%
  filter(server_name == 'N.DJOKOVIC')


### || ### || ### || ### || ### || ### || ###
##### Explore net clearance #####
### || ### || ### || ### || ### || ### || ###
# *** Can we distinguish serves by net clearance? ***

# -- Look at Differences in net clearance between 1st vs 2nd serve
# atp_pbp_df %>%
#   group_by(server_name, serve_num) %>%
#   summarise(avg_net_height = mean(z_net_serve, na.rm = TRUE)) %>%
#   View()
# There's around a 10cm difference between 1st and 2nd serve in ball net height
# -- Most players (except Krajinovic, Jannik) has net height around 1.12m on 1st serve, and
#    above 1.2m on 2nd serve

ggplot(data = atp_pbp_df, 
       aes(x = z_net_serve, fill = as.factor(serve_num))) +
  geom_density( aes(y = ..density.., fill=as.factor(serve_num)), alpha = 0.6) + 
  labs(fill="Serve Number")

# -- Meanwhile, Differences in net clearance between Ad. vs Deuce court are minuscule
# atp_pbp_df %>%
#   group_by(server_name, court_side) %>%
#   summarise(avg_net_height = mean(z_net_serve, na.rm = TRUE)) %>%
#   View()

ggplot(data = atp_pbp_df, 
       aes(x = z_net_serve, fill = as.factor(court_side))) +
  geom_density( aes(y = ..density.., fill=as.factor(court_side)), alpha = 0.6) + 
  labs(fill="Court Side")

# -- What is the average serve-ball height for all classified serve types?
# Seems like all serve types have the same shape of ball-serve height!!!
ggplot(data = atp_pbp_df %>% filter((serve_type != 'NA') & (serve_type !='Unclassified') ), 
       aes(x=z_net_serve, fill = as.factor(serve_type))) +
  geom_density( aes(y = ..density.., fill=as.factor(serve_type)), alpha = 0.6) + 
  labs(fill="Serve Type")

atp_pbp_df$serve_speed_kph <- as.numeric(gsub("([0-9]+).*$", "\\1", atp_pbp_df$serve_speed_kph))
ggplot(data = atp_pbp_df %>% filter((serve_type == 'Flat') | (serve_type =='Kick') ), 
       aes(x=serve_speed_kph, fill = as.factor(serve_type))) +
  geom_density( aes(y = ..density.., fill=as.factor(serve_type)), alpha = 0.6) + 
  labs(fill="Serve Speed")


# -- What if we categorise accuracy/power with 1 foot over the net?
table(ifelse(atp_pbp_df$z_net_serve > 1.22,1,0))
summary(atp_pbp_df$z_net_serve)

table(ifelse(tsitsipas_df$z_net_serve > 1.22,1,0))

tsitsipas_df$net_clear <- ifelse(tsitsipas_df$z_net_serve > 1.22,1,0)

tsitsipas_df %>%
  group_by(serve_dir, serve_num) %>%
  summarise(sum_safe_serves = sum(net_clear, na.rm = TRUE)) %>%
  arrange(serve_num, sum_safe_serves)
# Stefanos on 2nd serve has highest net clearance on his body serves
# Meanwhile on 1st serve, he has highest accuracy on Wide serves

# -- Quickly plot served-ball height at net compared on 1st vs 2nd serve (For stefanos)
ggplot(data = tsitsipas_df, 
       aes(x=z_net_serve, fill = as.factor(serve_num))) +
  geom_density( aes(y = ..density.., fill=as.factor(serve_num)), alpha = 0.6) + 
  labs(fill="Serve Number")

ggplot(data = thiem_df, 
       aes(x=z_net_serve, fill = as.factor(serve_num))) +
  geom_density( aes(y = ..density.., fill=as.factor(serve_num)), alpha = 0.6) + 
  labs(fill="Serve Number")
# ===

### || ### || ### || ### || ### || ### || ###
##### Serve Type vs. Serve direction #####
### || ### || ### || ### || ### || ### || ###
atp_pbp_df %>%
  filter(serve_type != 'NA') %>%
  filter(serve_dir != '') %>%
  group_by(serve_type, serve_dir) %>%
  summarise(count = n())


### || ### || ### || ### || ### || ### || ###
##### Serve Directions on Breakpoint #####
### || ### || ### || ### || ### || ### || ###

# -- Serve directions on break point
tsitsipas_df$serve_dir <- factor(tsitsipas_df$serve_dir,
                                 levels = c('T', 'Body', 'Wide',''))
thiem_df$serve_dir <- factor(thiem_df$serve_dir,
                             levels = c('T', 'Body', 'Wide',''))

# Stef has a slight preference going Wide (> 50%) on Ad. court
tsitsipas_df %>%
  filter(is_break_point=='True') %>%
  filter(serve_dir != '') %>%
  group_by(serve_dir, serve_side, serve_num) %>%
  summarise(count = n()) %>%
  arrange(serve_side, serve_num)

# Domi has a slight preference going Wide (> 50%) on Ad. on 1st serve
thiem_df %>%
  filter(is_break_point=='True') %>%
  filter(serve_dir != '') %>%
  group_by(serve_dir, serve_side, serve_num) %>%
  summarise(count = n()) %>%
  arrange(serve_side, serve_num)



### || ### || ### || ### || ### || ### || ###
### Serve stance distance from center #####
### || ### || ### || ### || ### || ### || ###
# Seems like players stand further away on Ad Court
# Some players exaggerate how far away they stand from center line
# Maybe this lets player defend with their forehand on the Ad. side.

atp_pbp_df %>%
  group_by(server_name, court_side) %>%
  summarise(avg_distance = mean(abs(y_ball_at_serve), na.rm = TRUE)) %>%
  View()

ggplot(data = atp_pbp_df %>% mutate(y_ball_at_serve = abs(y_ball_at_serve)), 
       aes(x=y_ball_at_serve, fill = as.factor(court_side))) +
  geom_density( aes(y = ..density.., fill=as.factor(court_side)), alpha = 0.6) + 
  labs(fill="Court Side")




# -- What about on serve numbers?
# -- Not as clear here
ggplot(data = atp_pbp_df %>% mutate(y_ball_at_serve = abs(y_ball_at_serve)), 
       aes(x=y_ball_at_serve, fill = as.factor(court_side))) +
  geom_density( aes(y = ..density.., fill=as.factor(court_side)), alpha = 0.6) + 
  facet_wrap(~serve_num)+
  labs(fill="Court Side")

atp_pbp_df %>%
  group_by(server_name, serve_num) %>%
  summarise(avg_distance = mean(abs(y_ball_at_serve), na.rm = TRUE)) %>%
  View()


# -- Horizontal distance (x distance) is not much of a factor.
ggplot(data = atp_pbp_df %>% mutate(x_ball_at_serve = abs(x_ball_at_serve)), 
       aes(x=x_ball_at_serve, fill = as.factor(court_side))) +
  geom_density( aes(y = ..density.., fill=as.factor(court_side)), alpha = 0.6) + 
  labs(fill="Court Side")


# Tsitsipas creates more angle when he's on Ad court???
# The further away Tsitsipas is from the centerline, the more likely he
# is to serve Wide (although, this isn't overwhelming evidence (52%))
ggplot(data = tsitsipas_df %>% mutate(y_ball_at_serve = abs(y_ball_at_serve)), 
       aes(x=y_ball_at_serve, fill = as.factor(court_side))) +
  geom_density( aes(y = ..density.., fill=as.factor(court_side)), alpha = 0.6) + 
  labs(fill="Court Side")

# Tsitsipas serves on his most extreme distances away from center line
tsitsipas_df %>% 
  mutate(y_ball_at_serve = abs(y_ball_at_serve)) %>%
  filter(y_ball_at_serve > 1.5) %>%
  group_by(court_side, serve_dir) %>%
  summarise(count = n())

# -- Djokovic is always doing some weird shit
ggplot(data = djokovic_df %>% mutate(y_ball_at_serve = abs(y_ball_at_serve)), 
       aes(x=y_ball_at_serve, fill = as.factor(court_side))) +
  geom_density( aes(y = ..density.., fill=as.factor(court_side)), alpha = 0.6) + 
  labs(fill="Court Side")

# -- Nadal's tendencies look like stefanos' (Surprising since he's left handed tho!!)
ggplot(data = nadal_df %>% mutate(y_ball_at_serve = abs(y_ball_at_serve)), 
       aes(x=y_ball_at_serve, fill = as.factor(court_side))) +
  geom_density( aes(y = ..density.., fill=as.factor(court_side)), alpha = 0.6) + 
  labs(fill="Court Side")



ggplot(data = thiem_df %>% mutate(y_ball_at_serve = abs(y_ball_at_serve)), 
       aes(x=y_ball_at_serve, fill = as.factor(court_side))) +
  geom_density( aes(y = ..density.., fill=as.factor(court_side)), alpha = 0.6) + 
  labs(fill="Court Side")



### || ### || ### || ### || ### || ### || ###
##### Plot serve peak height #####
### || ### || ### || ### || ### || ### || ###
# The peak height for all players is consistently the initial height
# at which a serve is struck.

ggplot(data = atp_pbp_df, 
       aes(x=z_peak_serve, fill = as.factor(serve_num))) +
  geom_density( aes(y = ..density.., fill=as.factor(serve_num)), alpha = 0.6) + 
  labs(fill="Serve Number")


atp_pbp_df %>%
  filter(serve_num==2) %>%
  select(z_peak_serve, z_ball_at_serve) %>% 
  View()

sum(atp_pbp_df$z_peak_serve == atp_pbp_df$z_ball_at_serve, na.rm = T)
# seems like height of strike is always the peak serve height? WTF..


