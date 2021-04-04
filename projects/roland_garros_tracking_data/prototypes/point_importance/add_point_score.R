library(dplyr)
# Delete this

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")

atp_pbp_df <- read.csv('./eda/atp_roland_garros_19_20.csv')

atp_pbp_df <- atp_pbp_df %>%
  filter(year != 2018) 

# atp_pbp_df %>%
#   filter(match_id == 'year_2020_SM001_tracking_data.json') %>%
#   View()

# Add server names
playerid_df <- read.csv('collect_data/player_id.csv',stringsAsFactors = FALSE)
playerid_df$id <- as.integer(playerid_df$id)


atp_pbp_df$server_id <- as.integer(as.character(atp_pbp_df$server_id))
atp_pbp_df$returner_id <- as.integer(as.character(atp_pbp_df$returner_id))
atp_pbp_df$point_winner_id <- as.integer(as.character(atp_pbp_df$point_winner_id))

atp_pbp_df <- atp_pbp_df %>%
  mutate(serve_side = ifelse( point_num %% 2 == 0, 'Ad', 'Deuce')) %>%
  left_join(playerid_df, by = c('server_id' = 'id')) %>%
  rename(server_name = name) %>%
  left_join(playerid_df, by = c('returner_id' = 'id')) %>%
  rename(returner_name = name)


player_names <- c('R.NADAL', 'N.DJOKOVIC', 'R.FEDERER',
                  'D.THIEM', 'S.TSITSIPAS','A.ZVEREV')

atp_to_test <- atp_pbp_df %>%
  mutate(is_fault = ifelse(point_end_type %in% c('Faulty Serve', 'DoubleFault'), TRUE, FALSE),
         is_doublefault = ifelse(point_end_type == 'DoubleFault', TRUE, FALSE),
         is_tiebreak = ifelse(game_num > 12, TRUE, FALSE)) %>%
  select(point_ID, point_num, game_num, set_num, year, 
         server_name, returner_name, 
         server_id, returner_id, point_winner_id,
         is_break_point, is_break_point_converted,
         is_fault, is_doublefault, is_tiebreak,
         serve_side, serve_num, match_id) %>%
  filter(server_name %in% player_names | returner_name %in% player_names)


### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ###
### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ###
### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ###

#initial_server_score <- 0
#initial_returner_score <- 0

server_score_vec <- c()
#server_score_vec <- c(server_score_vec, initial_server_score)

returner_score_vec <- c()
#returner_score_vec <- c(returner_score_vec, initial_returner_score)

#point_id <- 299
for (point_id in 1:(nrow(atp_to_test))){
  
  # -- If first point of a game, set both scores to 0
  point_num <- atp_to_test[point_id, 'point_num']
  
  if(point_num == 1){
    server_score_vec <- c(server_score_vec, 0)
    returner_score_vec <- c(returner_score_vec, 0)
    next
  }
  
  current_server_score <- server_score_vec[point_id -1]
  current_returner_score <- returner_score_vec[point_id-1]
  
  # -- If 1st Serve Fault, score does not change
  is_fault <- atp_to_test[point_id-1, 'is_fault']
  is_doublefault <- atp_to_test[point_id-1, 'is_doublefault']
  if(is_fault & !is_doublefault){
    server_score_vec <- c(server_score_vec, current_server_score)
    returner_score_vec <- c(returner_score_vec, current_returner_score)
    next
  }
  
  server_id <- atp_to_test[point_id-1, 'server_id']
  returner_id <- atp_to_test[point_id-1, 'returner_id']
  winner_id <- atp_to_test[point_id-1, 'point_winner_id']
  
  # Deal with Tiebreaks
  is_tiebreak <- atp_to_test[point_id-1, 'is_tiebreak']
  if(is_tiebreak){
    next_server_id <- atp_to_test[point_id, 'server_id']
    
    did_change_server <- !next_server_id == server_id
    
    
    if(server_id == winner_id){
      update_server_score <- current_server_score + 1
      
      if(did_change_server){
        server_score_vec <- c(server_score_vec, current_returner_score)
        returner_score_vec <- c(returner_score_vec, update_server_score)
      } else{
        server_score_vec <- c(server_score_vec, update_server_score)
        returner_score_vec <- c(returner_score_vec, current_returner_score)
      }
      
    } else{
      update_returner_score <- current_returner_score + 1
      
      if(did_change_server){
        server_score_vec <- c(server_score_vec, update_returner_score)
        returner_score_vec <- c(returner_score_vec, current_server_score)
      } else{
        server_score_vec <- c(server_score_vec, current_server_score)
        returner_score_vec <- c(returner_score_vec, update_returner_score)
      }
      
    }
    
    next
  }
  
  if(server_id == winner_id){
    update_server_score <- current_server_score + 1
    server_score_vec <- c(server_score_vec, update_server_score)
    returner_score_vec <- c(returner_score_vec, current_returner_score)
  } else{
    update_returner_score <- current_returner_score + 1
    server_score_vec <- c(server_score_vec, current_server_score)
    returner_score_vec <- c(returner_score_vec, update_returner_score)
    
  }
}

atp_to_test$s_score = server_score_vec
atp_to_test$r_score = returner_score_vec


saveRDS(atp_to_test, file = "atp_7_players.rds")

atp_to_test <- readRDS("atp_7_players.rds")


## Add columns for Game score and Set scores
dummy_data <- atp_to_test %>%
  filter(match_id == 'year_2020_SM001_tracking_data.json')


beginning_point_ids <- which((dummy_data$point_num == 1) & (dummy_data$serve_num == 1) )
last_point_ids <- beginning_point_ids - 1

last_points_df <- dummy_data[last_point_ids,] 

last_points_df$s_game_won <- ifelse(last_points_df$s_score > last_points_df$r_score,
                                    1,0)
last_points_df$r_game_won <- ifelse(last_points_df$s_game_won ==0,
                                    1,0)

# player1 <- 'R.NADAL'
# player2 <- 'N.DJOKOVIC'


s_cum_games <- c()
r_cum_games <- c()
for(set_number in unique(last_points_df$set_num)){
  
  server_game_vec <- c(0)
  returner_game_vec <- c(0)
  
  set_pbp <- last_points_df %>%
    filter(set_num == set_number)
  
  for(point_id in 1:nrow(set_pbp)){
    
    
    # if(set_pbp[point_id,'game_num'] == 1){
    #   server_game_vec <- c(server_game_vec, 0)
    #   returner_game_vec <- c(returner_game_vec, 0)
    #   
    # }
    
    if (set_pbp[point_id,'s_game_won'] == 1){
      server_game_vec <- c(server_game_vec, returner_game_vec[point_id])
      returner_game_vec <- c(returner_game_vec, server_game_vec[point_id] + 1)
      
    }
    
    if(set_pbp[point_id,'r_game_won'] == 1){
      server_game_vec <- c(server_game_vec, returner_game_vec[point_id] +1)
      returner_game_vec <- c(returner_game_vec, server_game_vec[point_id])
      
    }
    
  }
  
  s_cum_games <- c(s_cum_games, server_game_vec)
  r_cum_games <- c(r_cum_games, returner_game_vec)
  
}

s_cum_games
r_cum_games

set_ending_score_ids <- which( ((s_cum_games>=6) & (abs(s_cum_games - r_cum_games) >=2)) |  ((r_cum_games>=6) & (abs(s_cum_games - r_cum_games) >=2)) )

s_cum_games[-c(0, set_ending_score_ids)]
r_cum_games[-c(0, set_ending_score_ids)]

last_points_df$s_cum_games_won <- s_cum_games[-c(0, set_ending_score_ids)]

last_points_df %>%
  group_by(match_id, set_num, server_name) %>%
  summarise(num_games = sum(s_game_won, na.rm = T) + sum(r_game_won, na.rm = T)) %>% View()

atp_to_test %>%
  left_join(last_points_df %>%
              select(match_id, point_ID, 
                     s_game_won, r_game_won)) %>% View()


atp_to_test %>%
  select(point_num, game_num, set_num,is_tiebreak, 
         is_break_point,
         s_score, r_score, is_fault,
         server_id, returner_id, point_winner_id,
         is_fault, is_doublefault) %>%
  filter(game_num>12) %>%
  View()



### Add Point Importance -----
load(file = "/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/point_importance/importance.RData")
source(file = "/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/point_importance/importance.R")

atp_to_test$point_importance <- mapply(importance,
                                      point_x = atp_points$serve_points_won,
                                      point_y = atp_points$return_points_won,
                                      game_x = atp_points$serving_games_won,
                                      game_y = atp_points$returning_games_won,	
                                      set_x = atp_points$serving_sets_won,
                                      set_y = atp_points$returning_sets_won,
                                      tiebreak = atp_points$tiebreak,
                                      bestof3 = atp_points$bestof3
)


importance(point_x = 6,
           point_y = 6,
           game_x = 6,
           game_y = 6,
           set_x = 2,
           set_y = 2,
           tiebreak = TRUE,
           bestof3 = FALSE)

importance(point_x = 1,
           point_y = 2,
           game_x = 4,
           game_y = 5,
           set_x = 1,
           set_y = 2,
           tiebreak = FALSE,
           bestof3 = FALSE)

importance(point_x = 2,
           point_y = 3,
           game_x = 6,
           game_y = 5,
           set_x = 2,
           set_y = 2,
           tiebreak = FALSE,
           bestof3 = FALSE)


