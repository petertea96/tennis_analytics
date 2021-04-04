library(dplyr)

dat <- read.csv("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/summary_points_avail_df.csv")

dat %>%
  group_by(year, atp_flag) %>%
  summarise(tot_num_points_avail = sum(num_points_avail),
            tot_rallies = sum(num_points))



### Count number of matches for each player
# Note: Djokovic somehow played more matches than Nadal...

dat <- read.csv('./summary_points_avail_df.csv')

dat2 <- dat %>%
  mutate(player1 = gsub('\\s+', '', player1),
         player2 = gsub('\\s+', '', player2),
         proportion_avail = num_points_avail/num_points) 

dat %>%
  group_by(player1, atp_flag, year) %>%
  summarise(num_matches = n()) %>% View()


dat %>%
  group_by(player2, atp_flag, year) %>%
  summarise(num_matches = n()) %>% View()


p1 <- dat2 %>%
  filter(proportion_avail > 0.8) %>%
  group_by(player1, atp_flag) %>%
  summarise(num_matches = n())

p2 <- dat2 %>%
  filter(proportion_avail > 0.8) %>%
  group_by(player2, atp_flag) %>%
  summarise(num_matches = n())

p1 %>%
  full_join(p2, by = c('player1' = 'player2', 'atp_flag')) %>%
  rowwise() %>%
  mutate(match_tot = sum(num_matches.x, num_matches.y, na.rm = TRUE)) %>%
  arrange(desc(match_tot)) %>%
  View()


djoko <- dat2 %>% filter(player1 == 'N.DJOKOVIC') %>% arrange(year)
nadal <- dat2 %>% filter(player2 == 'R.NADAL') %>% arrange(year)
nadal1 <- dat2 %>% filter(player1 == 'R.NADAL') %>% arrange(year)

dat %>% filter(player2 == 'J.SINNER') %>% View()
dat %>% filter(player1 == 'J.SINNER') %>% View()


