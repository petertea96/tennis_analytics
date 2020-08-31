get_grouped_score <- function(server_score,
                              returner_score){
  # --> Game Point
  if(server_score == 'AD'){
    return('Game Point')
  }
  
  # --> Break Point
  if(returner_score == 'AD'){
    return('Break Point')
  }
  
  server_score = as.numeric(levels(server_score))[server_score]
  returner_score = as.numeric(levels(returner_score))[returner_score]
  
  # --> Game Point
  if(( (server_score == 40) | (server_score == 6) ) & (server_score > returner_score) ){
    return('Game Point')
  }
  
  if( (server_score > 6) & ( (server_score - returner_score) == 1 ) ){
    return('Game Point')
  }
  
  
  # --> Break Point
  if( ( (returner_score == 40) | (returner_score == 6) ) & (returner_score > server_score) ){
    return('Break Point')
  }
  
  if( (returner_score > 6) & ( (returner_score - server_score) == 1 ) ){
    return('Break Point')
  }
  
  # --> Even / 1st point
  if(server_score == returner_score) {
    if(server_score ==0 ){
      return('First Point')
    } else{
      return('Even')
    }
  }
  
  if( (server_score > 6) & (returner_score > 6) & ( abs(server_score - returner_score) == 2) ){
    return('First Point')
  }
  
  
  #--> Ahead
  if(server_score > returner_score){
    return('Ahead')
  }
  
  # -->Behind
  if(server_score < returner_score){
    return('Behind')
  }
  
  return(NULL)
  
}
