# -- Functions required to impute intended serve locations

get_net_height <- function(y_coordinate){
  # -- Get net height, given the y net coordinate.
  
  y_coordinate <- abs(y_coordinate)
  
  m = (1.07 - 0.91) / (4.115 - 0)
  b = 1.07 - m*4.115
  
  net_height <- m*y_coordinate + b 
  
  return(net_height)
}

#get_net_height(-4.115)



# -- Use physics to impute intended serve bounce locations (x,y).
# -- Draw a 3-d line (https://www.geeksforgeeks.org/equation-of-a-line-in-3d/)
# -- Note: This ignores the fact that ball has spin. Generally, we find that this
#    imputation method tends to overshoot the actual serve bounce x-coordinate.
#    We also assume ball trajectgory path is linear...

get_intended_serve_bounce_loc <- function(x_ball_at_serve, 
                                          y_ball_at_serve, 
                                          z_ball_at_serve,
                                          z_net_serve, 
                                          y_net_serve){
  
  direction_vec <- c(0 - x_ball_at_serve, 
                     y_net_serve - y_ball_at_serve, 
                     z_net_serve - z_ball_at_serve)
  
  z_term <- (0 - z_net_serve) / direction_vec[3] 
  intended_x <- z_term*direction_vec[1] + 0
  intended_y <- z_term*direction_vec[2] + y_net_serve
  
  return(c(intended_x, intended_y))
  
}



impute_next_location <- function(x1,y1, x2, y2, fwd = 1.5){
  # Function used for plotting direction arrows...
  if ( is.na(x1) | is.na(x2) | is.na(y1) | is.na(y2)  ){
    return(c(NA, NA))
  }
  
  if (x2 >= 0){
    x3 <- x2 + fwd 
  } else {
    x3 <- x2 - fwd
  }
  
  # -- slope
  m <- (y2 - y1)/ (x2 - x1)
  
  # -- intercept
  b <- y1 - m*x1
  
  y3 <- m*x3 + b
  
  return(c(x3,y3))
}

#impute_next_location(NA, 1,1,1)
