# -- Helpful functions that help run or extract what I need for Bayesian HMNL


# -- Scale continuous columns of training set
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}


# -- Softmax Function
softmax <- function(par){
  # https://rpubs.com/FJRubio/softmax#:~:text=The%20softmax%20function%20is%20the,1exp(xj).
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}

# -- Get the most frequent prediction
# -- Assumes a (V x N) matrix where V is the number of posterior iterations and 
#    N is the sample size of the training data.
get_column_prediction <- function(x){
  pred <- which.max(table(x))
  
  pred
  
}