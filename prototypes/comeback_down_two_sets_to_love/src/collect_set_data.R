# Sackmann's tennis_atp data
library(dplyr)

# Collect who won given a single set score
who_won_set <- function(score){
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



get_set_vector <- function(entire_score){
  
  score_line <- unlist(strsplit(as.character(entire_score), ' '))
  
  who_won_vector <- c()
  
  for(index in 1:length(score_line)){
    score <- score_line[index]
    
    who_won_vector[index] <- who_won_set(score=score)
  }
  
  return(who_won_vector)
  
}

# -- test function
#data_path <- "/Users/petertea/Documents/Github/tennis_data/tennis_atp/"
#year <- 2014
#filename <- paste(data_path, 'atp_matches_', year, '.csv', sep = '')
#match_data <- read.csv(filename,stringsAsFactors = FALSE)

five_set_matches <- match_data %>%
  #filter(best_of == 5) %>%
  filter((tourney_level == 'G')) %>%
  # -- Only completed matches
  filter(!grepl(score, pattern='RET')) %>%
  filter(!grepl(score, pattern='W/O')) %>%
  filter(!grepl(score, pattern='ABD')) %>%
  filter(!grepl(score, pattern='DEF')) %>%
  
  # -- Remove matches with less than 3 sets played
  filter(stringr::str_count(score,pattern = '-') >= 3) %>%
  filter(!is.na(score)) %>%
  select(tourney_id, tourney_name, surface, winner_name,
         loser_name, score, round, surface)

#entire_score <- five_set_matches[23, 'score']
#test2 <- get_set_vector(entire_score)
#test2

# Collect tidy dataframe for one match
get_set_tidy_df <- function(entire_score, year, tournament, round,
                            player1_name, player2_name, surface){
  
  set_winners <- get_set_vector(entire_score)
  
  numsets <- length(set_winners)
  
  first_set_winner <- set_winners[1]
  second_set_winner <- set_winners[2]
  third_set_winner <- set_winners[3]
  
  
  # -- If sets went beyond 3
  fourth_set_winner <- ifelse(numsets >=4, 
                              set_winners[4],
                              0)
  fifth_set_winner <- ifelse(numsets == 5,
                             set_winners[5],
                             0)
  
  straight_set <- ifelse( (first_set_winner == second_set_winner) &(first_set_winner == third_set_winner),
                          1,0)
  
  five_set_incomplete_comeback <-ifelse((first_set_winner == second_set_winner) &
                                          (numsets == 5) &
                                          (fifth_set_winner == first_set_winner),
                                        1,0
  )
  five_set_complete_comeback <-ifelse((first_set_winner == second_set_winner) &
                                        (numsets == 5) &
                                        (fifth_set_winner != first_set_winner),
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
                             'fourth_set_winner' = fourth_set_winner,
                             'fifth_set_winner' = fifth_set_winner,
                             'straight_set' = straight_set,
                             'five_set_incomplete_comeback' = five_set_incomplete_comeback,
                             'five_set_complete_comeback' = five_set_complete_comeback,
                             stringsAsFactors = FALSE
  )
  
  return(summary_data)
  
}

# --test it out
#get_set_tidy_df(entire_score, year = 2014, tournament = 'Australian Open',
#                round = 'R128', player1_name = 'Gonzalez', 
#                player2_name = 'Fognini', surface = 'Hard')


get_set_year <- function(match_data, year){
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
    
    
    datalist[[index]] <- get_set_tidy_df(entire_score = score, year = year,
                                         tournament = tournament_name, 
                                         round = round_id, surface = surface,
                                         player1_name = player1_name, 
                                         player2_name = player2_name)
  }
  
  all_data <- do.call(rbind, datalist)
  
  return(all_data)
  
}

test3 = get_set_year(five_set_matches, 2014)
sum(test3$five_set_complete_comeback)


# -- Collect on all available data
collect_entire_set_data <- function(year_list){
  data_path <- "/Users/petertea/Documents/Github/tennis_data/tennis_atp/"
  datalist = list()
  
  for(index in 1:length(year_list)){
    
    year <- year_list[index]
    filename <- paste(data_path, 'atp_matches_', year, '.csv', sep = '')
    match_data <- read.csv(filename,stringsAsFactors = FALSE)
    
    match_data <- read.csv(filename,stringsAsFactors = FALSE)
    
    five_set_matches <- match_data %>%
      filter((tourney_level == 'G')) %>%
      # -- Only completed matches
      filter(!grepl(score, pattern='RET')) %>%
      filter(!grepl(score, pattern='W/O')) %>%
      filter(!grepl(score, pattern='ABD')) %>%
      filter(!grepl(score, pattern='DEF')) %>%
      
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
#test5 <- collect_entire_set_data(1975)

#test5[rowSums(is.na(test5)) > 0,] %>% View()

