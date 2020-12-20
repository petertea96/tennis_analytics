library(dplyr)
library(ggplot2)
library(mgcv)
library(lme4)

filename <- "/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/predict_serve_direction/process_serve_location_pbp/processed_modified_body_serve_slam_pointbypoint.rds"
slam_data <- readRDS(filename)


training_data <- slam_data %>%
  select(ServeNumber, Speed_KMH, serve_location, server_won, server_name,
         returner_name, court_side, server_pressure, player_last_serve_loc,
         player_last_serve2_loc, year, tournament)


## -- Bar Charts of serve location %
bar_chart_data <- training_data %>%
  filter(complete.cases(.)) %>%
  group_by(server_name, serve_location) %>%
  summarise(tot = n()) %>% 
  left_join(training_data %>%
              filter(complete.cases(.)) %>%
              group_by(server_name) %>%
              summarise(total_serves = n())) %>%
  mutate(freq = tot/total_serves) %>%
  select(server_name, serve_location, freq) 

# -- Which players to look at?
training_data %>%
  filter(complete.cases(.)) %>%
  group_by(server_name) %>%
  summarise(total_serves = n()) %>%
  arrange(desc(total_serves)) %>%
  filter(server_name %in% c('Jannik Sinner', 'David Ferrer'))

players_to_see <- c('Rafael Nadal', 'Roger Federer', 'Novak Djokovic', 'Andy Murray',
                    'Kevin Anderson', 'Daniil Medvedev',
                    'John Isner', 'Kei Nishikori', 'Milos Raonic', 'Juan Martin del Potro')

# Who had highest proportion to serves down the T:
# Ans: Anderson, Murray, Kyrgios, delpo, Schwartzman
bar_chart_data %>%
  filter(serve_location == 'T') %>%
  arrange(desc(freq)) %>% View()

# Who had highest proportion to serves Wide:
# Ans: Raonic, Roger, Medvedev, Gasquet, Paire
bar_chart_data %>%
  filter(serve_location == 'W') %>%
  arrange(desc(freq)) %>% View()

# Who had highest proportion to serves Body:
# Ans: Jannik Sinner, David Ferrer
bar_chart_data %>%
  filter(serve_location == 'B') %>%
  arrange(desc(freq)) %>% View()


### Makes sense to look at proportions against the same player?
server_names <- c('Rafael Nadal', 'Novak Djokovic', 'Kevin Anderson')


# Data of 3 servers against Federer
four_servers_data <- training_data %>%
  filter(server_name %in% server_names) %>%
  filter(returner_name == 'Roger Federer') %>%
  filter(complete.cases(.))

four_servers_barchart_data <- four_servers_data %>%
  group_by(server_name, serve_location) %>%
  summarise(tot = n()) %>% 
  left_join(four_servers_data  %>%
              group_by(server_name) %>%
              summarise(total_serves = n())) %>%
  mutate(freq = tot/total_serves) %>%
  select(server_name, serve_location, freq)

ggplot(data=four_servers_barchart_data, 
       aes(x=server_name, y=freq, fill = serve_location)) +
  geom_bar(stat="identity", width=0.25) 
  

# -- Add player height data
player_height_data <- read.csv('/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/official_atp_height_2020.csv')

full_slam_data <- slam_data %>%
  
  left_join(player_height_data %>% select(player_handedness, player_name),
            by = c('server_name' = 'player_name')) %>%
  rename('server_handedness' = 'player_handedness') %>%
  
  left_join(player_height_data %>% select(player_handedness, player_name),
            by = c('returner_name' = 'player_name')) %>%
  rename('returner_handedness' = 'player_handedness') %>%
  filter(complete.cases(.))
  






# -- Look at serve locations against left handed returners
full_slam_data %>%
  group_by(serve_location, court_side, returner_handedness) %>%
  summarise(num_aces = sum(server_ace),
            num_df = sum(server_df),
            ratio = ((num_aces+1)/num_df))


# -- Get frequencies of each serve location
left_counts <- full_slam_data %>%
  filter(server_handedness == 'left-handed') %>%
  group_by(serve_location, court_side, ServeNumber) %>%
  summarise(n=n())

right_counts <- full_slam_data %>%
  filter(server_handedness == 'right-handed') %>%
  group_by(serve_location, court_side, ServeNumber) %>%
  summarise(n=n())

# -- Ignore Server pressure for now (On Deuce court, the score will ALWAYS be "tied")


left_counts %>%
filter(court_side == 'Advantage') %>%
filter(ServeNumber==1) %>%
group_by(serve_location) %>%
summarise(tot=sum(n)) %>%
mutate(freq = tot/sum(tot))

left_freq_table <- data.frame(matrix(NA, nrow=4, ncol = 4))

counter_index <- 1
for(court.side in unique(left_counts$court_side) ){
  
    
    for(serve.number in unique(left_counts$ServeNumber)){
      
      freq <- left_counts %>%
        filter(court_side == court.side) %>%
        filter(ServeNumber==serve.number) %>%
        group_by(serve_location) %>%
        summarise(tot=sum(n)) %>%
        mutate(freq = tot/sum(tot)) %>%
        .$freq
      
      situation <- paste(court.side, serve.number, sep='_')
      left_freq_table[counter_index,1] <- situation
      left_freq_table[counter_index,2:4] <- freq
      counter_index <- counter_index + 1
    }
    
}  

colnames(left_freq_table) <- c('situation', 'B', 'W', 'T')



right_freq_table <- data.frame(matrix(NA, nrow=4, ncol = 4))
counter_index <- 1
for(court.side in unique(right_counts$court_side) ){
  
  
  for(serve.number in unique(right_counts$ServeNumber)){
    
    freq <- right_counts %>%
      filter(court_side == court.side) %>%
      filter(ServeNumber==serve.number) %>%
      group_by(serve_location) %>%
      summarise(tot=sum(n)) %>%
      mutate(freq = tot/sum(tot)) %>%
      .$freq
    
    situation <- paste(court.side, serve.number, sep='_')
    right_freq_table[counter_index,1] <- situation
    right_freq_table[counter_index,2:4] <- freq
    counter_index <- counter_index + 1
  }
  
}  

colnames(right_freq_table) <- c('situation', 'B', 'W', 'T')


full_slam_data %>%
  filter(match_id == '2019-usopen-1311') %>% 
  filter(server_name == 'Denis Shapovalov') %>%
  #filter(as.integer(PointNumber) > 100) %>%
  filter(ServeNumber == 2) %>%
  mutate(serve_loc_num = ifelse(serve_location == 'B', 1,
                                ifelse(serve_location=='W',2,3))) %>%
  ggplot(aes(x = as.integer(PointNumber), y = as.integer(serve_loc_num),
             colour = as.factor(court_side))) +
  geom_point()+
  geom_line()

full_slam_data %>%
  filter(match_id == '2019-usopen-1311') %>% 
  filter(server_name == 'Denis Shapovalov') %>%
  filter(PointNumber < 100) %>%
  View()

