library(dplyr)
# Delete this

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/")


### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ###
### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ###
### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ### ***** ###




add_cum_games <- function(one_match){
  # one_match: DataFrame

  # Arbitrarily set the 1st server as 'player1' and the other as 'player2'
  player1 <- one_match[1, 'server_name']
  player2 <- one_match[1, 'returner_name']
  
  # Ids for the beginning point of each game 
  beginning_point_ids <- which((one_match$point_num == 1) & (one_match$serve_num == 1) )
  
  # Ids for the last point of each game
  last_point_ids <- beginning_point_ids - 1
  
  # Dataframe of all last points played
  last_points_df <- one_match[last_point_ids,] 
  
  # Add indicator columns of whether server or returner won the last point of each game
  last_points_df$s_game_won <- ifelse(last_points_df$s_score > last_points_df$r_score,
                                      1,0)
  last_points_df$r_game_won <- ifelse(last_points_df$s_game_won ==0,
                                      1,0)
  
  
  # Calculate the Cumulative number of games won
  p1_cum_games <- c()
  p2_cum_games <- c()
  for(set_number in unique(last_points_df$set_num)){
    
    p1_game_vec <- c(0)
    p2_game_vec <- c(0)
    
    set_pbp <- last_points_df %>%
      filter(set_num == set_number)
    
    for(point_id in 1:nrow(set_pbp)){
      
      
      
      # if(set_pbp[point_id,'game_num'] == 1){
      #   server_game_vec <- c(server_game_vec, 0)
      #   returner_game_vec <- c(returner_game_vec, 0)
      #   
      # }
      
      if (set_pbp[point_id,'s_game_won'] == 1){
        winner_name <- set_pbp[point_id,'server_name']
        
      }
      
      if(set_pbp[point_id,'r_game_won'] == 1){
        winner_name <- set_pbp[point_id,'returner_name']
        
      }
      
      if(winner_name == player1){
        p1_game_vec <- c(p1_game_vec, p1_game_vec[point_id] +1)
        p2_game_vec <- c(p2_game_vec, p2_game_vec[point_id])
      } else{
        p1_game_vec <- c(p1_game_vec, p1_game_vec[point_id])
        p2_game_vec <- c(p2_game_vec, p2_game_vec[point_id]+1)
      }
      
      
    }
    
    p1_cum_games <- c(p1_cum_games, p1_game_vec)
    p2_cum_games <- c(p2_cum_games, p2_game_vec)
    
  }
  
  # Remove Cumulative games that denote final game line of a set
  # Note: Roland Garros uses an ADVANTAGE SET
  last_set <- one_match[nrow(one_match),'set_num']
  last_game <- one_match[nrow(one_match),'game_num']
  
  last_game_ids <- one_match %>%
    group_by(set_num) %>%
    summarise(last_game = max(game_num) +1) %>%
    .$last_game %>%
    cumsum()
  
  p1_cum_games_to_add <- p1_cum_games[-c(last_game_ids)]
  p2_cum_games_to_add <- p2_cum_games[-c(last_game_ids)]
  
  #set_ending_score_ids <- which( ((p1_cum_games>=6) & (abs(p1_cum_games - p2_cum_games) >=2)) |  ((p2_cum_games>=6) & (abs(p1_cum_games - p2_cum_games) >=2))  ) 
  #| ( (p1_cum_games ==7) & (p2_cum_games ==7) ) | ( (p2_cum_games ==7) & (p1_cum_games ==7) )
  
  #p1_cum_games_to_add <- p1_cum_games[-c(0, set_ending_score_ids)]
  #p2_cum_games_to_add <- p2_cum_games[-c(0, set_ending_score_ids)]
  
  # if((last_set == 5) & (last_game>=14) ){
  #   print(paste('CHECK THIS OUT: ', as.character(unique(one_match$match_id))))
  #   p1_cum_games_to_add <- p1_cum_games_to_add[-length(p1_cum_games_to_add)]
  #   p2_cum_games_to_add <- p2_cum_games_to_add[-length(p2_cum_games_to_add)]
  # }
  
  
  games_df <- one_match[beginning_point_ids,]
  games_df$player1 <- player1
  games_df$player2 <- player2
  games_df$p1_cum_games <- p1_cum_games_to_add 
  games_df$p2_cum_games <- p2_cum_games_to_add 
  
  
  added_cum_games_df <-   one_match %>% 
    left_join(games_df %>%
                select(game_num, set_num, 
                       player1, player2, p1_cum_games, p2_cum_games)) 
  
  
  # -- add cumulative sets won
  
  set_change_id <- which(added_cum_games_df$set_num != dplyr::lag(added_cum_games_df$set_num ) )
  last_points_in_set_df <- added_cum_games_df[set_change_id-1,]
  
  p1_cum_sets <- c(0)
  p2_cum_sets <- c(0)
  for (set_index in 1: nrow(last_points_in_set_df)){
    
     if(last_points_in_set_df[set_index, 's_score'] > last_points_in_set_df[set_index, 'r_score']){
       set_winner_name <- last_points_in_set_df[set_index, 'server_name']

     } 
    
    if (last_points_in_set_df[set_index, 's_score'] < last_points_in_set_df[set_index, 'r_score']){
      set_winner_name <- last_points_in_set_df[set_index, 'returner_name']
      
    } 
    
    if(set_winner_name == player1){
      p1_cum_sets <- c(p1_cum_sets, p1_cum_sets[set_index] + 1 )
      p2_cum_sets <- c(p2_cum_sets, p2_cum_sets[set_index] )
      
    }
    
    if(set_winner_name == player2){
      p1_cum_sets <- c(p1_cum_sets, p1_cum_sets[set_index] )
      p2_cum_sets <- c(p2_cum_sets, p2_cum_sets[set_index]+1 )
      
    }
    
    
  }
  
  changeover_set_df <- added_cum_games_df[c(1,set_change_id),]
  changeover_set_df$p1_cum_sets <- p1_cum_sets
  changeover_set_df$p2_cum_sets <- p2_cum_sets
  
  
  added_cum_games_df <- added_cum_games_df %>% 
    left_join(changeover_set_df %>%
                select( set_num, 
                       player1, player2, p1_cum_sets, p2_cum_sets))
  
  
  return(added_cum_games_df)
  
}


atp_to_test <- readRDS("atp_7_players.rds")

atp_to_test$server_name <- ifelse(atp_to_test$server_name == 'PH.HERBERT', 
                                  'P.HERBERT', atp_to_test$server_name) 
atp_to_test$returner_name <- ifelse(atp_to_test$returner_name == 'PH.HERBERT', 'P.HERBERT',
                                    atp_to_test$returner_name) 

datalist = list()

all_unique_match_ids <- as.character(unique(atp_to_test$match_id))

bad_data <- c()
for(a_match_id in 1:length(all_unique_match_ids)){
  one_match_df <- atp_to_test %>%
    filter(match_id == all_unique_match_ids[a_match_id])
  
  # Remove duplicate rows
  one_match_df <- one_match_df %>%
    distinct()
  
  first_point_each_game <- one_match_df %>%
    group_by(set_num, game_num) %>%
    summarise(first_point = min(point_num)) %>%
    .$first_point
  
  if(!all(first_point_each_game==1)){
    bad_data <- c(bad_data, all_unique_match_ids[a_match_id])
    next
  }
  
  datalist[[a_match_id]] <- add_cum_games(one_match_df)
  
}

hopefully_processed_pbp_data = do.call(rbind, datalist)


testing <- hopefully_processed_pbp_data %>%
  filter(match_id == 'year_2020_SM031_tracking_data.json') %>%
  select(point_ID, year, server_name, returner_name, is_break_point, 
         s_score, r_score, player1:p2_cum_sets)


# -- Add Tie Break column
#tb_ind <- (hopefully_processed_pbp_data$game_num == 13) & (hopefully_processed_pbp_data$set_num !=5)

hopefully_processed_pbp_data %>%
  filter(is_tiebreak) %>% View()

player_names <- c('R.NADAL', 'N.DJOKOVIC', 'R.FEDERER',
                  'D.THIEM', 'S.TSITSIPAS','A.ZVEREV')

server_data <- hopefully_processed_pbp_data %>%
  filter(server_name %in% player_names)

server_point <- server_data$s_score 
returner_point <- server_data$r_score

server_game <- ifelse(server_data$server_name == server_data$player1,
                      server_data$p1_cum_games, server_data$p2_cum_games)
returner_game <- ifelse(server_data$server_name == server_data$player1,
                      server_data$p2_cum_games, server_data$p1_cum_games)

server_set <- ifelse(server_data$server_name == server_data$player1,
                    server_data$p1_cum_sets, server_data$p2_cum_sets)

returner_set <- ifelse(server_data$server_name == server_data$player1,
                     server_data$p2_cum_sets, server_data$p1_cum_sets)
 
tb <- server_data$is_tiebreak

 
#hopefully_processed_pbp_data
imp_vec<- mapply(importance,
       point_x = server_point,
       point_y = returner_point,
       game_x = server_game,
       game_y = returner_game,	
       set_x = server_set,
       set_y = returner_set,
       tiebreak = tb,
       bestof3 = rep(FALSE, nrow(server_data)))

point_importance <- unlist(imp_vec)
null_cases <- sapply(imp_vec, length) == 0

imp_vec[which(null_cases)] <- sample(point_importance, 
                                     size = sum(null_cases), replace = TRUE)

importance_vec <- unlist(imp_vec)

server_data[which(importance_vec>0.2),] %>%View()

