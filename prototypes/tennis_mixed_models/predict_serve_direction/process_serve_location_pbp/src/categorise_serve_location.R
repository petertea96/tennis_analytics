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
  
  return(serve_location)
  
}



