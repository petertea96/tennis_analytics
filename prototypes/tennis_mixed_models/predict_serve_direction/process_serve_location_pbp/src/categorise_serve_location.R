categorise_serve_location <- function(location){
  
  if( (is.null(location)) | (is.na(location)) ){
    return(NA)
  }
  
  if ( !(nchar(as.character(location)) > 0) ){
    return(NA)
  }
  
  serve_location = ifelse( grepl(pattern = 'C', x = as.character(location)), 'T',
                           ifelse(grepl(pattern = 'W', x = as.character(location)), 'W',
                                  ifelse('B' == as.character(location),'B', NA
                                  )))

  
  # serve_location = ifelse(grepl(pattern = 'B', x = as.character(location)), 'B',
  #                         ifelse(as.character(location) == 'C', 'T',
  #                                ifelse(as.character(location) == 'W', 'W', NA)))

  return(serve_location)
  
}


### Binary Serve Direction
serve_loc_hand <- function(returner_hand, serve_loc, serve_court){
  if( (is.null(serve_loc)) | (is.na(serve_loc)) ){
    return(NULL)
  }
  
  if(serve_court == 'Deuce'){
    if(returner_hand == 'right-handed'){
      if(serve_loc == 'T'){
        return('backhand')
      } else if (serve_loc %in% c('B', 'W')){
        return('forehand')
      }
      
    } else if (returner_hand == 'left-handed'){
      if(serve_loc == 'W'){
        return('backhand')
      } else if (serve_loc %in% c('B', 'T')){
        return('forehand')
      }
    }
    
  } else if (serve_court == 'Advantage'){
    if(returner_hand == 'right-handed'){
      if(serve_loc == 'W'){
        return('backhand')
      } else if (serve_loc %in% c('B', 'T')){
        return('forehand')
      }
      
    } else if (returner_hand == 'left-handed'){
      if(serve_loc == 'T'){
        return('backhand')
      } else if (serve_loc %in% c('B', 'W')){
        return('forehand')
      }
    }
    
  }
  
  return(NULL)
  
}
