### Model, assuming that all players are equal

# || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- ||
# -- Load libraries
library(dplyr)

# -- Source own functions
# --> Add Deuce/Ad. court side column
source("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/src/ad_or_deuce.R")
source("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/src/classify_pressure_score.R")
# || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- ||

data_path <-  "/Users/petertea/Documents/tennis_data/tennis_slam_pointbypoint/"
year <- 2019
tournament <- 'usopen'

# -- Read in match pbp data + meta-data
pbp_file_name <- paste(data_path, 
                       as.character(year),
                       "-", 
                       as.character(tournament), 
                       "-points.csv", 
                       sep="")

pbp_data <- read.csv(pbp_file_name)
# 47, 893 rows

metadata_file_name <- paste(data_path, 
                            as.character(year), 
                            "-", 
                            as.character(tournament), 
                            "-matches.csv", 
                            sep=""  )
meta_data <- read.csv(metadata_file_name)

# only ATP data:
pbp_data_player_1 <- pbp_data %>%
  mutate(match_id_integer = as.integer(substr(match_id,13,16))) %>%
  filter(match_id_integer < 2001) %>%
  # 29, 823 rows

  filter(PointServer == 1) %>%
  # 14, 931 rows 
  
  left_join(meta_data %>% select(match_id, player1, player2),
            by = 'match_id') %>%
  rename('server' = 'player1',
         'returner' = 'player2') %>%
  select(match_id, SetNo, PointWinner, server, returner, ServeWidth, ServeNumber,
         ServeWidth, Speed_KMH, P1Score, P2Score,
         P1DoubleFault, P2DoubleFault, P1Ace, P2Ace,) %>%
  mutate(won_point = ifelse(PointWinner == 1, 1, 0))


player_height_data <- read.csv('/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/official_atp_height_2020.csv')

# -- Change demon's name
pbp_data_player_1$returner <- plyr::revalue(pbp_data_player_1$returner , 
                                            c('Alex de Minaur' = 'Alex De Minaur',
                                              'Albert Ramos Vinolas' = 'Albert Ramos'))

pbp_data_player_1 <- pbp_data_player_1 %>%
  left_join(player_height_data %>% select(player_handedness, player_name),
            by = c('returner' = 'player_name')) 

# check if missing any player handedness
pbp_data_player_1[!complete.cases(pbp_data_player_1),]
sum(complete.cases(pbp_data_player_1))

tot_num_serves <- nrow(pbp_data_player_1)
current_score <- paste(pbp_data_player_1$P1Score, '-', pbp_data_player_1$P2Score, sep = ' ')

# Nudge score 
current_score <- current_score[c(tot_num_serves, 1:(tot_num_serves - 1 ) )]

server_score <- pbp_data_player_1$P1Score[c(tot_num_serves, 1:(tot_num_serves -1 ))]
returner_score <- pbp_data_player_1$P2Score[c(tot_num_serves, 1:(tot_num_serves -1 ))]

pbp_data_player_1$current_score <- current_score
pbp_data_player_1$server_score <- server_score
pbp_data_player_1$returner_score <- returner_score

# -- Add grouped score
pbp_data_player_1$pressure_score <- mapply(pbp_data_player_1$server_score,
                                           pbp_data_player_1$returner_score,
                                           FUN = classify_pressure_score)

# Add side of court served
pbp_data_player_1$court_side <- mapply(pbp_data_player_1$server_score,
                                       pbp_data_player_1$returner_score,
                                       FUN = ad_or_deuce)


sum(pbp_data_player_1$P1DoubleFault) + sum(pbp_data_player_1$P2DoubleFault)

pbp_data_player_1_mod_serve_location <- pbp_data_player_1 %>% 
  filter(!(is.null(ServeWidth)))%>%
  filter(!is.na(ServeWidth)) %>%
  filter(nchar(as.character(ServeWidth)) > 0) %>%
  mutate(serve_location = ifelse( grepl(pattern = 'C', x = as.character(ServeWidth)), 'T',
                                  ifelse(grepl(pattern = 'W', x = as.character(ServeWidth)), 'W',
                                         ifelse('B' == as.character(ServeWidth),'B', 'NA'
                                         ))))


pbp_data_player_1_mod_serve_location <- pbp_data_player_1_mod_serve_location %>%
  mutate(racquet_side = ifelse(court_side == 'Deuce' & player_handedness == 'right-handed' & serve_location == 'T',
                               'backhand', 
                               ifelse(court_side == 'Deuce' & player_handedness == 'right-handed' & serve_location == 'W',
                                      'forehand',
                                      ifelse(court_side == 'Advantage' & player_handedness == 'right-handed' & serve_location == 'T',
                                             'forehand',
                                             ifelse(court_side == 'Advantage' & player_handedness == 'right-handed' & serve_location == 'W',
                                                    'backhand',
                                                    ifelse(court_side == 'Deuce' & player_handedness == 'left-handed' & serve_location == 'T',
                                                           'forehand',
                                                           ifelse(court_side == 'Deuce' & player_handedness == 'left-handed' & serve_location == 'W',
                                                                  'backhand',
                                                                  ifelse(court_side == 'Advantage' & player_handedness == 'left-handed' & serve_location == 'T',
                                                                         'backhand',
                                                                         ifelse(court_side == 'Advantage' & player_handedness == 'left-handed' & serve_location == 'W',
                                                                                'forehand',
                                                                                ifelse(serve_location=='B', 'body', 'idk'))))))))))


# Frequency of each serve location & & court_Side & Serve No
pbp_data_player_1_mod_serve_location %>%
  filter(ServeNumber > 0) %>%
  group_by(serve_location, court_side, ServeNumber) %>%
  summarise(count = n(), win_percentage = sum(won_point) / n()) %>%
  arrange(count, court_side)

pbp_data_player_1_mod_serve_location %>%
  group_by(court_side) %>%
  summarise(count = n())

pbp_data_player_1_mod_serve_location[!complete.cases(pbp_data_player_1_mod_serve_location),]

pbp_data_player_1_mod_serve_location %>%
  group_by(ServeNumber, court_side, racquet_side) %>%
  summarize(winperc = sum(won_point)/ n())


pbp_data_player_1_mod_serve_location %>%
  filter(player_handedness == 'left-handed') %>%
  .$returner %>%
  unique()


pbp_data_player_1_mod_serve_location %>%
  filter(player_handedness == 'left-handed') %>%
  group_by(ServeNumber, court_side, serve_location) %>%
  summarize(winperc = sum(won_point)/ n())


pbp_data_player_1_mod_serve_location %>%
  filter(player_handedness == 'right-handed') %>%
  group_by(ServeNumber, court_side, serve_location) %>%
  summarize(winperc = sum(won_point)/ n())



# average_model <- glm(won_point ~ Speed_KMH + as.factor(ServeNumber) + 
#                        player_handedness + 
#                      court_side + serve_location  + court_side*player_handedness*serve_location,
#                      data = pbp_data_player_1_mod_serve_location,
#                      family = 'binomial')

average_model <- glm(won_point ~ Speed_KMH  + racquet_side +
                     serve_location + court_side + serve_location*racquet_side,
                     data = pbp_data_player_1_mod_serve_location,
                     family = 'binomial')



