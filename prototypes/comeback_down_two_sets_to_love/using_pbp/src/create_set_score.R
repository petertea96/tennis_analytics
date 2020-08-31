# -- Write function that creates a SetWinner column
# For last point of each set, set it equal to the player who won the point
# Ensure that within a set, atleast 6 games were played
# We'll say that the last player to win the point won the set?
# Look at finishing scores too... But what about tie breaks???
# argh...

library(dplyr)
source("~/Documents/Github/serve_speeds/src/grouped_score.R")



get_set_data <- function(slam_point_data, match_id, year, tournament,
                         player1_name, player2_name){
  # *****
  # slam_point_data: dataframe of one grand slam match
  # match_id: str of match 
  # year: int of year
  # tournament: str of tournament name
  
  # --Note:
  # *SetWinner is an indicator on if player 1 or player 2 won the set
  # *****
  
  
  # Indices of last point of each set except last one
  last_point_of_set_indices <- which(diff(slam_point_data$SetNo) == 1) 
  
  
  # Check if last point played was a game point or break point
  n <- nrow(slam_point_data)
  new_P1Score <- slam_point_data$P1Score[c(n, 1:(n -1 ))]
  new_P2Score <- slam_point_data$P2Score[c(n, 1:(n -1 ))]
  slam_point_data$P1Score <- new_P1Score
  slam_point_data$P2Score <- new_P2Score
  
  
  slam_point_data$grouped_score <- mapply(slam_point_data$P1Score,
                                          slam_point_data$P2Score,
                                          FUN = get_grouped_score)
  
  final_point <- slam_point_data[nrow(slam_point_data), 'grouped_score']
  
  # IF final point of match was a break or game point, then it will be considered
  # as a final set point
  if( (final_point == 'Game Point') |  (final_point == 'Break Point')){
    last_point_of_set_indices <-c(last_point_of_set_indices, nrow(dummy_data))
  }
  
  # Order of player who won the sets in the match
  set_sequence <- dummy_data[last_point_of_set_indices,] %>% select(PointWinner)
  player_winner_seq <- set_sequence$PointWinner
  
  # -- Number of observed sets in the match
  numsets <- length(unique(slam_point_data$SetNo))
  
  # ***** Questionable... ***** #
  # --Fix error of 2 sets (in ATP)
  #if((numsets == 3) & (length(player_winner_seq) < 3)){
  #  player_winner_seq[3] <- getmode(player_winner_seq)
  #}
  
  # --Fix error of 4 sets (in ATP)
  #if((numsets == 4) & (length(player_winner_seq) == 3) ){
  #  player_winner_seq[4] <- getmode(player_winner_seq)
  #}
  # ***** Questionable... ***** #
  
  first_set_winner <- player_winner_seq[1]
  second_set_winner <- player_winner_seq[2]
  third_set_winner <- player_winner_seq[3]
  
  
  # -- If sets went beyond 3
  fourth_set_winner <- ifelse(numsets >=4, 
                              player_winner_seq[4],
                              0)
  fifth_set_winner <- ifelse(numsets == 5,
                             player_winner_seq[5],
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
  
  
  # -- Did we get complete data?
  if(length(player_winner_seq) == 0){
    complete = 0
  } else if(max(table(player_winner_seq)) < 3){
    complete = 0
  } else{
    complete = 1
  }
  
  
  summary_data <- data.frame('match_id' = match_id,
                             'year' = year,
                             'tournament' = tournament,
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
                             'complete_data' = complete,
                             stringsAsFactors = FALSE
  )
  
  return(summary_data)
}




# -- Test set
year = 2012
tournament = 'wimbledon'
path_to_data <- '/Users/petertea/Documents/Sports-Analytics/Sackmann/tennis_slam_pointbypoint/'

points_file_name <- paste(as.character(year), "-", as.character(tournament), "-points.csv", sep="")
matches_file_name <- paste(as.character(year), "-", as.character(tournament), "-matches.csv", sep=""  )

points_data <- read.csv(paste(path_to_data, points_file_name, sep = ''))
matches_data <- read.csv(paste(path_to_data, matches_file_name, sep = ''))

dummy_data <- points_data %>%
  filter(match_id == '2012-wimbledon-1701')

#test1 = get_set_data(slam_data = dummy_data ,match_id = '2015-wimbledon-1133',
#             year = year, tournament = tournament,
#             player1_name = 'Vanni',
#             player2_name ='Ward')

# -- 2015-wimbledon-1133
# -- This isn't complete data
# Match was complete in real life, but we're missing data mid 4th set (match went to 5th set; Ward def. Vanni)


# Indices of last point of each set except last one
to_see = which(diff(dummy_data$SetNo) == 1) 


# Check if last point played was a game point or break point
# --> Add grouped score column
source("~/Documents/Github/serve_speeds/src/grouped_score.R")

n <- nrow(dummy_data)
new_P1Score <- dummy_data$P1Score[c(n, 1:(n -1 ))]
new_P2Score <- dummy_data$P2Score[c(n, 1:(n -1 ))]
dummy_data$P1Score <- new_P1Score
dummy_data$P2Score <- new_P2Score


dummy_data$grouped_score <- mapply(dummy_data$P1Score,
                                   dummy_data$P2Score,
                                   FUN = get_grouped_score)

final_point <- dummy_data[nrow(dummy_data), 'grouped_score']

if( (final_point == 'Game Point') |  (final_point == 'Break Point')){
  to_see <-c(to_see, nrow(dummy_data))
}

setwinners <- dummy_data[to_see,] %>% select(PointWinner)









## --

# -- Test set
year = 2019
tournament = 'wimbledon'
path_to_data <- '/Users/petertea/Documents/Sports-Analytics/Sackmann/tennis_slam_pointbypoint/'

points_file_name <- paste(as.character(year), "-", as.character(tournament), "-points.csv", sep="")
matches_file_name <- paste(as.character(year), "-", as.character(tournament), "-matches.csv", sep=""  )

points_data <- read.csv(paste(path_to_data, points_file_name, sep = ''))
matches_data <- read.csv(paste(path_to_data, matches_file_name, sep = ''))

dummy_data2 <- points_data %>%
  filter(match_id == '2019-wimbledon-1701')

