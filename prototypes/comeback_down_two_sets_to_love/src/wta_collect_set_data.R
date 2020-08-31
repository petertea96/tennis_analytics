# Sackmann's tennis_atp data
library(dplyr)

# Collect who won given a single set score
wta_who_won_set <- function(score){
  # ***
  # Given a set score (factor), who won the set?
  # ***
  score <- as.character(score)
  separate_scores <- unlist(strsplit(as.character(score), '-'))
  
  player1_score <- substr(separate_scores[1], start = 1, stop = 1)
  player2_score <- substr(separate_scores[2], start = 1, stop = 1)
  
  who_won <- ifelse(as.numeric(player1_score) > as.numeric(player2_score),
                    1,2)
  return(who_won)
  
}

# Collect who won each set for an entire match
# -- test function
#who_won_set('6-7(4)')



wta_get_set_vector <- function(entire_score){
  
  score_line <- unlist(strsplit(as.character(entire_score), ' '))
  
  who_won_vector <- c()
  
  for(index in 1:length(score_line)){
    score <- score_line[index]
    
    who_won_vector[index] <- wta_who_won_set(score=score)
  }
  
  return(who_won_vector)
  
}

# Collect tidy dataframe for one match
wta_get_set_tidy_df <- function(entire_score, year, tournament, round,
                            player1_name, player2_name, surface){
  
  set_winners <- wta_get_set_vector(entire_score)
  
  numsets <- length(set_winners)
  
  first_set_winner <- set_winners[1]
  second_set_winner <- set_winners[2]
  
  # -- If sets went beyond 3
  third_set_winner <- ifelse(numsets ==3, 
                              set_winners[3],
                              0)
 
  
  straight_set <- ifelse( (first_set_winner == second_set_winner),
                          1,0)
  
  
  
  summary_data <- data.frame('year' = year,
                             'tournament' = tournament,
                             'round' = round,
                             'surface' = surface,
                             'player1' = player1_name,
                             'player2' = player2_name,
                             'first_set_winner' = first_set_winner,
                             'second_set_winner' = second_set_winner,
                             'third_set_winner' = third_set_winner,
                             'straight_set' = straight_set,
                             stringsAsFactors = FALSE
  )
  
  return(summary_data)
  
}


wta_get_set_year <- function(match_data, year){
  # Assumes match_data has removed RETIRED matches
  
  player1_list <- as.character(match_data$winner_name)
  player2_list <- as.character(match_data$loser_name)
  tournament_list <- as.character(match_data$tourney_name)
  round_list <- as.character(match_data$round)
  score_list <-  as.character(match_data$score)
  surface_list <-  as.character(match_data$surface)
  
  datalist = list()
  
  for(index in 1:nrow(match_data)){
    player1_name <- player1_list[index]
    player2_name <- player2_list[index]
    tournament_name <- tournament_list[index]
    round_id <- round_list[index]
    score <- score_list[index]
    surface <- surface_list[index]
    
    
    datalist[[index]] <- wta_get_set_tidy_df(entire_score = score, year = year,
                                         tournament = tournament_name, 
                                         round = round_id, surface = surface,
                                         player1_name = player1_name, 
                                         player2_name = player2_name)
  }
  
  all_data <- do.call(rbind, datalist)
  
  return(all_data)
  
}



wta_collect_entire_set_data <- function(year_list){
  data_path <- "https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_"
  datalist = list()
  
  for(index in 1:length(year_list)){
    
    year <- year_list[index]
    filename <- paste(data_path, year, '.csv', sep = '')
    match_data <- read.csv(filename,stringsAsFactors = FALSE)
    
    match_data <- read.csv(filename,stringsAsFactors = FALSE)
    
    five_set_matches <- match_data %>%
      filter((tourney_level == 'G')) %>%
      # -- Only completed matches
      filter(!grepl(score, pattern='RET')) %>%
      filter(!grepl(score, pattern='W/O')) %>%
      filter(!grepl(score, pattern='ABD')) %>%
      filter(!grepl(score, pattern='DEF')) %>%
      
      # -- Remove matches with less than 2 sets played
      filter(stringr::str_count(score,pattern = '-') >= 2) %>%
      
      filter(!is.na(score)) %>%
      select(tourney_id, tourney_name, surface, winner_name,
             loser_name, score, round, surface)
    
    datalist[[index]] <- wta_get_set_year(match_data = five_set_matches, 
                                      year = year)
  }
  
  
  all_data <- do.call(rbind, datalist)
  
  return(all_data)
  
  
}
# test it out
#test=wta_collect_entire_set_data(1968:1980)


