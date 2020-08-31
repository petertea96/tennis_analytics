get_score <- function(score){
  # Function to convert scores to 4 categories
  
  # Note this doesn't write out all possible scenarios.
  # I just coded the serve scores observed for Zverev's service games
  
  # ** Australian Open in 2019 implemented a best of 10 tie-break...
  
  game_point = c('40 - 0', '40 - 15', '40 - 30', 'AD - 40')
  break_point = c('0 - 40', '15 - 40', '30 - 40', '40 - AD',
                  '1 - 5', '1 - 6', '3 - 5', '2 - 5')
  setup_point = c('40 - 40', '30 - 30', '30 - 15', '15 - 30',
                  '30 - 0', '0 - 30')
  neutral_point = c('0 - 0', '15 - 0', '0 - 15', '15 - 15',
                    '2 - 0', '1 - 1', '0 - 1')
  
  res <-  ifelse(score %in% game_point, 'Game',
                 ifelse(score %in% break_point, 'Break',
                        ifelse(score %in% setup_point, 'Setup',
                               'Neutral')))
  
  return(res)
}

get_score_linkedin <- function(score){
  # Function to convert scores to 6 categories
  
  # Note this doesn't write out all possible scenarios.
  # I just coded the serve scores observed for Zverev's service games
  
  game_point <- c('40 - 0', '40 - 15', '40 - 30', 'AD - 40',
                  '6 - 1', '6 - 2', '6 - 3', '6 - 4', '6 - 5',
                  '7 - 6', '8 - 7', '9 - 8', '10 - 9', '11 - 10',
                  '12 - 11', '13 - 12', '14 - 13', '15 - 14', '16 - 15'
                  )
  break_point <- c('0 - 40', '15 - 40', '30 - 40', '40 - AD',
                  '0 - 6', '1 - 6', '2 - 6', '3 - 6', '4 - 6', '5 - 6',
                  '6 - 7', '7 - 8', '8 - 9', '9 - 10', '10 - 11',
                  '11 - 12', '12 - 13', '13 - 14', '14 - 15', '15 - 16'
                  )
  first_point <- c('0 - 0',
                   '7 - 0', '0 - 7',
                   '7 - 1', '1 - 7',
                   '7 - 2', '2 - 7',
                   '7 - 3', '3 - 7',
                   '7 - 4', '4 - 7',
                   '7 - 5', '5 - 7', 
                   '8 - 6', '6 - 8',
                   '9 - 7', '7 - 9',
                   '10 - 8', '8 - 10',
                   '11 - 9', '9 - 11',
                   '12 - 10', '10 - 12',
                   '13 - 11', '11 - 13',
                   '14  - 12', '12 - 14',
                   '15 - 13', '13 - 15',
                   '16 - 14', '14 - 16',
                   '17 - 15', '15 - 17'
                   )
  
  behind_point <- c('0 - 15','0 - 30','15 - 30',
                    '0 - 1', '0 - 2', '0 - 3', '0 - 4', '0 - 5',
                    '1 - 2', '1 - 3', '1 - 4', '1 - 5',
                    '2 - 3', '2 - 4', '2 - 5',
                    '3 - 4', '3 - 5',
                    '4 - 5'
                    )
  
  ahead_point <- c('15 - 0', '30 - 0', '30 - 15', 
                   '1 - 0',
                   '2 - 0', '2 - 1',
                   '3 - 0', '3 - 1', '3 - 2',
                   '4 - 0', '4 - 1', '4 - 2', '4 - 3',
                   '5 - 1', '5 - 2', '5 - 3', '5 - 4'
                   )
  
  even_point <- c('15 - 15', '30 - 30', '40 - 40', 
                  '1 - 1', '2 - 2', '3 - 3', '4 - 4',
                  '5 - 5', '6 - 6', '7 - 7', '8 - 8',
                  '9 - 9', '10 - 10', '11 - 11', '12 - 12',
                  '13 - 13', '14 - 14')
  

  
  res <-  ifelse(score %in% game_point, 'Game Point',
                 ifelse(score %in% break_point, 'Break Point',
                        ifelse(score %in% first_point, 'First Point',
                               ifelse(score %in% behind_point, 'Behind',
                                      ifelse(score %in% ahead_point, 'Ahead',
                                             ifelse(score %in% even_point, 'Even', 'ERROR'))
                               ))))
  
  return(res)
  
  
}
