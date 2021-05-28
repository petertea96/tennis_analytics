# -- Categorise Serve direction
# -- This function has already been written in Python,
#    but now I'll write it in R. I've had to compute "intended serve 
#    directions" on Net Faults, which now necessitates the generation of
#    a the categorised serve direction.


categorise_serve_direction <- function(y_coordinate){
  
  # Args: y_coordinate [int]
  # Returns: chr
  
  #Assumes Serve bounce coordinate is given in metres
  #Note: (0,0,0) are the coordinates at the middle of the net.
  #Dimension of court: 23.77 m in length (x), and 8.23 m wide (y) -- for single's court
    
  #Classifies ball bounce coordinate as: Wide, Body, or T

  if (is.na(y_coordinate)){
    return(NA)
  }
  
  # Court is 8.23 m wide
  one_third_length = 4.115/3
  
  # Tenuous at the moment
  # What if a player really miss-hits the ball, and it bounces to the opposite side of the court?
  if ( (y_coordinate <= one_third_length) & (y_coordinate >= -one_third_length) ){
    serve_dir = 'T'
  } else if( ( (y_coordinate < 2*one_third_length) & (y_coordinate > one_third_length) ) | ( (y_coordinate > -2*one_third_length) & (y_coordinate < -one_third_length) )){
    serve_dir = 'Body'
    
  } else if ((y_coordinate >= 2*one_third_length) | ( y_coordinate <= -2*one_third_length )){
    serve_dir = 'Wide'
  } else {
    serve_dir = NA
    
  }

  
  return(serve_dir)

}

# categorise_serve_direction(0)
# categorise_serve_direction(-0.1)
