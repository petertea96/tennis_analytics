# -- Date: Sep 6th, 2020
# -- Purpose: Group scoring based on pressure situations


classify_pressure_score <- function(server_score,
                                    returner_score){
  
  # Function Goal:
  # Function to convert scores to 6 categories
  
  # Args:
  # -----
  # server_score [fct]: 
  # returner_score [fct]: 
  
  # Returns:
  # --------
  # str
  
  # -- Scores at Advantage
  if(server_score == 'AD'){
    return('game_point')
  }
  
  if(returner_score == 'AD'){
    return('break_point')
  }
  
  # -- Change type of variable to numeric
  if( is.factor(server_score) ){
    server_score <- as.numeric(levels(server_score))[server_score] 
  }
  if( is.factor(returner_score) ){
    returner_score <- as.numeric(levels(returner_score))[returner_score] 
  }
  
  
  # -- Identify Remaining Game Points
  if( ( (server_score == 40) & (returner_score < 40) ) |( (server_score == 6) & (returner_score <=5)  ) | 
      ( (server_score > 6) & (returner_score > 6) & ( (server_score - returner_score) == 1  ))
  ){
    return('game_point')
  }
    
  # Examples:   
  # game_point <- c('40 - 0', '40 - 15', '40 - 30', 'AD - 40',
  #                 '6 - 1', '6 - 2', '6 - 3', '6 - 4', '6 - 5',
  #                 '7 - 6', '8 - 7', '9 - 8', '10 - 9', '11 - 10',
  #                 '12 - 11', '13 - 12', '14 - 13', '15 - 14', '16 - 15')
    
  
  # -- Identify Remaining Break Points
  if( ( (returner_score == 40) & (server_score < 40) ) |( (returner_score == 6) & (server_score <=5)  ) | 
      ( (server_score > 6) & (returner_score > 6) & ( (returner_score - server_score) == 1  ))
  ){
    return('break_point')
  }  

  # Examples: 
  # break_point <- c('0 - 40', '15 - 40', '30 - 40', '40 - AD',
  #                  '0 - 6', '1 - 6', '2 - 6', '3 - 6', '4 - 6', '5 - 6',
  #                  '6 - 7', '7 - 8', '8 - 9', '9 - 10', '10 - 11',
  #                  '11 - 12', '12 - 13', '13 - 14', '14 - 15', '15 - 16')
  
  
  # -- Identify First Points
  if( ( (server_score == 0) & (returner_score == 0)) | 
      ((server_score == 7) & (returner_score <= 5)) |
      ((server_score >= 7) & (returner_score >= 7) & (abs(server_score - returner_score) == 2)) 
  ){
    return('first_point')
  }
  
  
  # -- Identify points where Server is behind (not including break points)
  
  if(server_score < returner_score){
    return('behind')
  }
  
  # Examples:
  # behind_point <- c('0 - 15','0 - 30','15 - 30', '0 - 1', '0 - 2', '0 - 3', '0 - 4', 
  #                   '0 - 5', '1 - 2', '1 - 3', '1 - 4', '1 - 5', '2 - 3', '2 - 4', 
  #                   '2 - 5', '3 - 4', '3 - 5', '4 - 5')
  
  
  # -- Identify points where Serve is ahead (not including game points)
  if(server_score > returner_score){
    return('ahead')
  }
  
  # Examples:
  # ahead_point <- c('15 - 0', '30 - 0', '30 - 15', '1 - 0', '2 - 0', '2 - 1', '3 - 0', 
  #                  '3 - 1', '3 - 2', '4 - 0', '4 - 1', '4 - 2', '4 - 3', '5 - 1', 
  #                  '5 - 2', '5 - 3', '5 - 4')
  
  


  # -- Identify points where Score is tied (not including 1st point)
  # Examples: 
  # first_point <- c('0 - 0', 
  #                  '7 - 0', '0 - 7', '7 - 1', '1 - 7', '7 - 2', '2 - 7',
  #                  '7 - 3', '3 - 7', '7 - 4', '4 - 7', '7 - 5', '5 - 7', 
  #                  '8 - 6', '6 - 8', '9 - 7', '7 - 9', '10 - 8', '8 - 10',
  #                  '11 - 9', '9 - 11', '12 - 10', '10 - 12', '13 - 11', '11 - 13',
  #                  '14  - 12', '12 - 14', '15 - 13', '13 - 15', '16 - 14', '14 - 16',
  #                  '17 - 15', '15 - 17')
  if(server_score == returner_score){
    return('even')
  }
  
  # Examples: 
  # even_point <- c('15 - 15', '30 - 30', '40 - 40','1 - 1', '2 - 2', '3 - 3', 
  #                 '4 - 4', '5 - 5', '6 - 6', '7 - 7', '8 - 8', '9 - 9', '10 - 10', 
  #                 '11 - 11', '12 - 12', '13 - 13', '14 - 14')
  
  # -- If none, return NULL
  return(NULL)
}
