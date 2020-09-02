### Purpose:
# This script has functions that take a year's match results data, and outputs the players who won
# each set as a tidy dataframe. It also includes indicator variables on whether the match 
# included a 'down-2-sets-to-love' comeback

# Sackmann's tennis_atp data
library(dplyr)

# Collect who won a set, given a single set score
who_won_set <- function(score){
  # ***
  # Given a set score (factor), who won the set?
  # Ex: 6-7 implies that 'player 2' won this set
  # ***
  
  # Args:
  # -----
  # score [str]: A single set score
  
  # Returns:
  # --------
  # [int]: Player 1 or Player 2 who won the set
  
  
  score <- as.character(score)
  
  # -- Remove any parentheses in the score string
  score <- gsub("\\s*\\([^\\)]+\\)","", score)
  
  separate_scores <- unlist(strsplit(as.character(score), '-'))
  
  #player1_score <- substr(separate_scores[1], start = 1, stop = 1)
  #player2_score <- substr(separate_scores[2], start = 1, stop = 1)
  player1_score <- tryCatch(
    as.numeric(separate_scores[1]),
    error = NA
  )
    
    
  player2_score <- tryCatch(
    as.numeric(separate_scores[2]),
    error = NA
  )
  who_won <- ifelse(player1_score > player2_score,
                    1,2)
  return(who_won)
  
}
# -- test function
#who_won_set('6-7(4)')



# Collect who won each set for an entire match
# For all set scores in a match, output a vector of players who won the sets
get_set_vector <- function(entire_score){
  
  # Args:
  # -----
  # entire score [str]: single string of all set scores (separated by a space)
  
  # Returns:
  # --------
  # vector [int]: Player 1 or Player 2 who won the set
  
  # Example: "4-6 4-6 6-3 6-3 6-2" --> 2 2 1 1 1
  # entire_score = '6-7(6) 6-4 6-2 2-1 RET'
  
  score_line <- unlist(strsplit(as.character(entire_score), ' '))
  
  retired_flag <- any( c('RET', 'W/O', 'ABD', 'DEF') %in% score_line )
  
  if(retired_flag){
    score_line <- score_line[-length(score_line)]
  }
  
  who_won_vector <- c()
  
  for(index in 1:length(score_line)){
    score <- score_line[index]
    
    who_won_vector[index] <- who_won_set(score=score)
  }
  
  if(retired_flag){
    who_won_vector[length(who_won_vector)] <- 1
  }
  
  return(who_won_vector)
  
}

# -- test function
# data_path <- "/Users/petertea/Documents/tennis_data/tennis_atp/"
# year <- 2014
# filename <- paste(data_path, 'atp_matches_', year, '.csv', sep = '')
# match_data <- read.csv(filename,stringsAsFactors = FALSE)
# 
# five_set_matches <- match_data %>%
# filter(best_of == 5) %>%
#  filter((tourney_level == 'G')) %>%
#-- Only completed matches
#   filter(!grepl(score, pattern='RET')) %>%
#   filter(!grepl(score, pattern='W/O')) %>%
#   filter(!grepl(score, pattern='ABD')) %>%
#   filter(!grepl(score, pattern='DEF')) %>%
# 
#   # -- Remove matches with less than 3 sets played
#   filter(stringr::str_count(score,pattern = '-') >= 3) %>%
#   filter(!is.na(score)) %>%
#   select(tourney_id, tourney_name, surface, winner_name,
#          loser_name, score, round, surface)
# 
# entire_score <- five_set_matches[23, 'score']
# test2 <- get_set_vector(entire_score)
# test2


# Collect tidy dataframe for one match
get_set_tidy_df <- function(entire_score, year, tournament, round,
                            player1_name, player2_name, surface){
  
  # Args:
  # -----
  # entire score [str]: single string of all set scores (separated by a space)
  # year [int]
  # tournament [str]
  # round [str]
  # player1_name [str]: match winner name
  # player2_name [str]: match loser name
  # surface [str]: surface of court
  
  # Returns:
  # --------
  # DataFrame w/ 1 row
  
  set_winners <- get_set_vector(entire_score)
  
  numsets <- length(set_winners)
  
  first_set_winner <- set_winners[1]
  second_set_winner <- set_winners[2]
  third_set_winner <- set_winners[3]
  
  
  # -- If sets went beyond 3
  fourth_set_winner <- ifelse(numsets >= 4, 
                              set_winners[4],
                              0)
  fifth_set_winner <- ifelse(numsets == 5,
                             set_winners[5],
                             0)
  # -- Indicatory variable if match was a straight set victory
  straight_set <- ifelse( (first_set_winner == second_set_winner) &(first_set_winner == third_set_winner),
                          1,0)
  
  # -- Indicator if match loser fought back down 2 sets to love, but still lost match
  five_set_incomplete_comeback <-ifelse((first_set_winner == second_set_winner) &
                                          (numsets == 5) &
                                          (fifth_set_winner == first_set_winner),
                                        1,0
  )
  
  # -- Indicator if match loser fought back down 2 sets to love, and won the match
  five_set_complete_comeback <-ifelse((first_set_winner == second_set_winner) &
                                        (numsets == 5) &
                                        (fifth_set_winner != first_set_winner),
                                      1,0)
  

  
  summary_data <- data.frame('year' = year,
                             'tournament' = tournament,
                             'round' = round,
                             'surface' = surface,
                             'winner' = player1_name,
                             'loser' = player2_name,
                             'first_set_winner' = first_set_winner,
                             'second_set_winner' = second_set_winner,
                             'third_set_winner' = third_set_winner,
                             'fourth_set_winner' = fourth_set_winner,
                             'fifth_set_winner' = fifth_set_winner,
                             'straight_set' = straight_set,
                             'five_set_incomplete_comeback' = five_set_incomplete_comeback,
                             'five_set_complete_comeback' = five_set_complete_comeback,
                             'score'= entire_score,
                             stringsAsFactors = FALSE
  )
  
  return(summary_data)
  
}

# --test it out
# get_set_tidy_df(entire_score, year = 2014, tournament = 'Australian Open',
#                round = 'R128', player1_name = 'Gonzalez',
#                player2_name = 'Fognini', surface = 'Hard')


get_set_year <- function(match_data, year){
  # Assumes match_data has removed RETIRED matches
  
  # Args:
  # -----
  # match_data [DataFrame]: One year's atp data
  # year [str]: Year of DataFrame
  
  # Returns:
  # --------
  # Processed DataFrame
  

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
    
    
    datalist[[index]] <- get_set_tidy_df(entire_score = score, year = year,
                                         tournament = tournament_name, 
                                         round = round_id, surface = surface,
                                         player1_name = player1_name, 
                                         player2_name = player2_name)
  }
  
  all_data <- do.call(rbind, datalist)
  
  return(all_data)
  
}

#test3 = get_set_year(five_set_matches, 2014)
#sum(test3$five_set_complete_comeback)


# -- Collect on all available data
collect_entire_set_data <- function(year_list, data_path){
  datalist = list()
  
  for(index in 1:length(year_list)){
    
    year <- year_list[index]
    filename <- paste(data_path, 'atp_matches_', year, '.csv', sep = '')
    match_data <- read.csv(filename,stringsAsFactors = FALSE)
    
    match_data <- read.csv(filename,stringsAsFactors = FALSE)
    
    five_set_matches <- match_data %>%
      filter( (best_of == 5) | (stringr::str_count(score, '-') >= 5) ) %>%
      # *** Note: Some best of 5 set matches are erroneously labelled as 3
      
      # -- Grand Slams is not the only time we see best of 5 sets
      #filter((tourney_level == 'G')) %>%
      # -- Only completed matches
      #filter(!grepl(score, pattern='RET')) %>%
      #filter(!grepl(score, pattern='W/O')) %>%
      #filter(!grepl(score, pattern='ABD')) %>%
      #filter(!grepl(score, pattern='DEF')) %>%
      
      # -- Remove matches with less than 3 sets played
      filter(stringr::str_count(score,pattern = '-') >= 3) %>%
      
      filter(!is.na(score)) %>%
      select(tourney_id, tourney_name, surface, winner_name,
             loser_name, score, round, surface)
    
    datalist[[index]] <- get_set_year(match_data = five_set_matches, 
                                      year = year)
  }
  
  
  all_data <- do.call(rbind, datalist)
  
  return(all_data)
  
  
}

# test it out
# data_path <- "/Users/petertea/Documents/tennis_data/tennis_atp/"
# test5 <- collect_entire_set_data(1975, data_path)
# 
# test5[rowSums(is.na(test5)) > 0,] %>% View()

