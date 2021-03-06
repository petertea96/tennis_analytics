# -- Are players who fall 2 sets to love more likely to come all the way back now than they 
# were in the past? 
# -- Seems like no..

# Which players have comeback down 2 sets to love the most?

library(ggplot2)
library(dplyr)

setwd("/Users/petertea/tennis_analytics/prototypes/comeback_down_two_sets_to_love/")
source("/Users/petertea/tennis_analytics/prototypes/comeback_down_two_sets_to_love/src/collect_set_data.R")

# -- Collect all historic data
atp_match_score_data_path <- "/Users/petertea/Documents/tennis_data/tennis_atp/"
historic_set_data <- collect_entire_set_data(1968:2020, atp_match_score_data_path)

# -- quality checks
historic_set_data %>%
  filter(five_set_complete_comeback == 1) %>%
  filter(winner == 'Aaron Krickstein') %>%
  View()


# -- quality checks
historic_set_data %>%
  filter(five_set_complete_comeback == 1) %>%
  filter(winner == 'Roger Federer') %>%
  View()

# Count how many times a player has completed a 'down-2-sets-to-love' comeback
historic_set_data  %>%
  filter(five_set_complete_comeback == 1) %>%
  group_by(winner) %>%
  summarise(n = n()) %>%
  arrange(desc(n))



# --> Save file..
saveRDS(historic_set_data, file = "historic_set_data.rds")




# For each year, calculate total instances of interest
summary_dat<- historic_set_data %>% 
  group_by(year) %>%
  summarise(matches = n(),
            tot_straight_sets = sum(straight_set),
            tot_complete_comebacks = sum(five_set_complete_comeback),
            tot_almost_comebacks = sum(five_set_incomplete_comeback),
            push_to_five = sum(five_set_complete_comeback) + sum(five_set_incomplete_comeback),
            tot_love_2 = sum(love_2))

# Divide to get proportions
summary_dat <- summary_dat %>%
  mutate(prop_push_to_five = push_to_five / matches,
         prop_complete_comebacks = tot_complete_comebacks/matches,
         prop_almost_comebacks = tot_almost_comebacks/matches) 

summary_dat %>% View()

# -- Plot our findings...
# --All surfaces
ggplot(data = summary_dat, aes(x = year, y = prop_push_to_five)) +
  geom_point() +
  geom_smooth(method='auto')

ggplot(data = summary_dat, aes(x = year, y = prop_complete_comebacks)) +
  geom_point() +
  geom_smooth(method='auto')


ggplot(data = summary_dat, aes(x = year, y = prop_almost_comebacks)) +
  geom_point() +
  geom_smooth(method='auto')

# Does dropping the first set matter at all???
# --- Losing first set analysis
historic_set_data <- historic_set_data %>%
  mutate(winner_lost_1st_set = ifelse(first_set_winner == 2, 1,0))

historic_set_data %>%
  group_by(year, surface) %>%
  summarise(prop_drop_1st_set_and_win = sum(winner_lost_1st_set)/n()) %>%
  ggplot(aes(x = year, y = prop_drop_1st_set_and_win)) +
  geom_point(colour = 'blue', fill = 'green') + geom_line(color = 'red', linetype = 2)



# ASIDE:
# -- Calculate datasets by court surface
# By surface
summary_dat_hard<- historic_set_data %>% 
  filter(surface == 'Hard') %>%
  group_by(year) %>%
  summarise(matches = n(),
            tot_straight_sets = sum(straight_set),
            tot_complete_comebacks = sum(five_set_complete_comeback),
            tot_almost_comebacks = sum(five_set_incomplete_comeback),
            push_to_five = sum(five_set_complete_comeback) + sum(five_set_incomplete_comeback),
            tot_love_2 = sum(love_2))

summary_dat_clay<- historic_set_data %>% 
  filter(surface == 'Clay') %>%
  group_by(year) %>%
  summarise(matches = n(),
            tot_straight_sets = sum(straight_set),
            tot_complete_comebacks = sum(five_set_complete_comeback),
            tot_almost_comebacks = sum(five_set_incomplete_comeback),
            push_to_five = sum(five_set_complete_comeback) + sum(five_set_incomplete_comeback),
            tot_love_2 = sum(love_2))

summary_dat_grass<- historic_set_data %>% 
  filter(surface == 'Grass') %>%
  group_by(year) %>%
  summarise(matches = n(),
            tot_straight_sets = sum(straight_set),
            tot_complete_comebacks = sum(five_set_complete_comeback),
            tot_almost_comebacks = sum(five_set_incomplete_comeback),
            push_to_five = sum(five_set_complete_comeback) + sum(five_set_incomplete_comeback),
            tot_love_2 = sum(love_2))



summary_dat_clay <- summary_dat_clay %>%
  mutate(prop_push_to_five = push_to_five / matches,
         prop_complete_comebacks = tot_complete_comebacks/matches,
         prop_almost_comebacks = tot_almost_comebacks/matches) 

summary_dat_grass <- summary_dat_grass %>%
  mutate(prop_push_to_five = push_to_five / matches,
         prop_complete_comebacks = tot_complete_comebacks/matches,
         prop_almost_comebacks = tot_almost_comebacks/matches) 

summary_dat_hard <- summary_dat_hard %>%
  mutate(prop_push_to_five = push_to_five / matches,
         prop_complete_comebacks = tot_complete_comebacks/matches,
         prop_almost_comebacks = tot_almost_comebacks/matches) 




# -- clay plots
ggplot(data = summary_dat_clay, aes(x = year, y = prop_push_to_five)) +
  geom_point() +
  #geom_smooth(method='lm')
  geom_smooth(method='auto')

ggplot(data = summary_dat_clay, aes(x = year, y = prop_complete_comebacks)) +
  geom_point() +
  geom_smooth(method='auto')
  

ggplot(data = summary_dat_clay, aes(x = year, y = prop_almost_comebacks)) +
  geom_point() +
  geom_smooth(method='auto')

# -- hard plots
ggplot(data = summary_dat_hard, aes(x = year, y = prop_push_to_five)) +
  geom_point() +
  #geom_smooth(method='lm')
  geom_smooth(method='auto')

ggplot(data = summary_dat_hard, aes(x = year, y = prop_complete_comebacks)) +
  geom_point() +
  geom_smooth(method='auto')


ggplot(data = summary_dat_hard, aes(x = year, y = prop_almost_comebacks)) +
  geom_point() +
  geom_smooth(method='auto')


# -- grass plots
ggplot(data = summary_dat_grass, aes(x = year, y = prop_push_to_five)) +
  geom_point() +
  #geom_smooth(method='lm')
  geom_smooth(method='auto')

ggplot(data = summary_dat_grass, aes(x = year, y = prop_complete_comebacks)) +
  geom_point() +
  geom_smooth(method='auto')


ggplot(data = summary_dat_grass, aes(x = year, y = prop_almost_comebacks)) +
  geom_point() +
  geom_smooth(method='auto')

