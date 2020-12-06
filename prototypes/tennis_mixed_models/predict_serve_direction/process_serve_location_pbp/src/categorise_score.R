categorise_score <- function(server_score, returner_score){
  # Function to convert scores to 6 categories
  # 1. Break
  # 2. Advantage
  # 3. Tied
  # 4. Ahead
  # 5. Behind
  
  
  # -- Obvious case first
  if( (server_score == 'AD')  ){
    return('Advantage')
  }
  
  if (returner_score == 'AD'){
    return('Break')
  }
  
  # -- Ensure we're working with numeric variable, if score is not 'AD'
  if(is.factor(server_score)){
    server_score = as.numeric(levels(server_score))[server_score]
    returner_score = as.numeric(levels(returner_score))[returner_score]
  } else if(is.character(server_score)){
    server_score = as.numeric(server_score)
    returner_score = as.numeric(returner_score)
  }
  
  if( (server_score == 40) & (returner_score < 40) ){
    return('Advantage')
  }
  
  if( (returner_score == 40) & (server_score < 40) ){
    return('Break')
  }
  
  if(server_score == returner_score){
    return('Tied')
  }
  
  if((server_score - returner_score) >=15){
    return('Ahead')
  }
  
  if((returner_score - server_score) >=15){
    return('Behind')
  }
  

  
# 
#   # -- Tie break situations
  if( (server_score == 6) & (returner_score <= 5) ){
    return('Advantage')
  }

  if( (server_score >= 6) & (returner_score >= 6) & ((server_score - returner_score) == 1) ){
    return('Advantage')
  }
  
  if( (returner_score == 6) & (server_score <= 5) ){
    return('Break')
  }
  
  if( (returner_score >= 6) & (server_score >= 6) & ((returner_score - server_score) == 1) ){
    return('Break')
  }
  
  if( (returner_score >= 6) & (server_score >= 6) & ((returner_score - server_score) == 2) ){
    return('Tied')
  }
  if( (returner_score >= 6) & (server_score >= 6) & ((server_score - returner_score) == 2) ){
    return('Tied')
  }
  
  if(server_score > returner_score){
    return('Ahead')
  }
  
  if(server_score < returner_score){
    return('Behind')
  }
  
  return(NULL)
  
}
