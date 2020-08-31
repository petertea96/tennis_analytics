# -- Ad court or Deuce Court?

ad_or_deuce <- function(server_score, returner_score){
  # -- should add a tiebreak flag argument?
  # tie_break = FALSE
  
  # -- Obvious case first
  if( (server_score == 'AD') | (returner_score == 'AD') ){
    return('Advantage')
  }
  
  server_score = as.numeric(levels(server_score))[server_score]
  returner_score = as.numeric(levels(returner_score))[returner_score]
  
  # -- Non-Tie break scores
  non_tie_break <- c(15,30,40)
  
  if( (server_score %in% non_tie_break) | (returner_score %in% non_tie_break) ){
    score_num <- point_number(server_score) + point_number(returner_score)
    
    court_side <- ifelse(score_num %% 2 == 0, 'Deuce', 'Advantage')
  
  } else{
    
    score_num <- server_score + returner_score
    court_side <- ifelse(score_num %% 2 == 0, 'Deuce', 'Advantage')
    
  }
  
  return(court_side)

}

point_number <- function(score){
  
  point_num <- ifelse(score == 0, 0,
                      ifelse(score == 15, 1,
                             ifelse(score == 30, 2,
                                    ifelse(score == 40, 3, score)
                                    )
                             )
                      )
  
  return(point_num)
  }
  
