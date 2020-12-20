lookup <- function(name, list, default){
  
  if(name %in% names(list)){
    list[[name]]
  }
  else{
    default
  }
}

variables <- function(winners, losers, margin, data){
  list(
    model.frame(winners, data, na.action = na.pass)[,1],
    model.frame(losers, data, na.action = na.pass)[,1],
    model.frame(margin, data, na.action = na.pass)[,1]
  )
}

#' Joint Additive MOV Elo Ratings
#'
#' This function calculates MOV Elo ratings using a joint additive model that combines information for both the win result and MOV result.
#'
#' @param winners. Character vector or formula specifying the winners of each result
#' @param losers. Character vector or formula specifying the losers of each result
#' @param margin. Numeric vector vector or formula specifying the margin of victory, given as winner score - loser score
#' @param k.margin. Numeric value of the learning rate to be applied to the MOV
#' @param k.win. Numeric value of the learning rate to be applied to the win result
#' @param scale.margin. Numeric scaling factor applied in the expectation step for the MOV
#' @param scale.win. Numeric scaling factor applied in the expectation step for the win prediction
#' @param data. Data frame containing winner, loser, and margin variables if using a data/formula specification.
#' @param default. Numeric value of the initial rating to assign to new competitors
#'
#' @return A data frame with Elo ratings before and after each event result.
#'
#' @section Details:
#' Datasets should be ordered from first game result to last.
#' Competitors must be uniquely and consistently identified in the winner and loser vectors.
#' Missing values in the MOV variable will be omitted and will throw a warning. 
#' 
#' The E-step for the joint additive model involves a linear model for the MOV and a logistic model for the win expectation. The expected margin for player \eqn{i} 
#' against \eqn{j} is:
#' \deqn{\hat{MOV} = \frac{R_i - R_j}{\sigma_{margin}}}.
#' For the win outcome:
#' \deqn{\hat{W} = \frac{1}{1+10^({R_j - R_i}{\sigma_{win}})}}.
#' In the standard Elo system, \eqn{sigma_{win} = 400}. A reasonable choice for \eqn{\sigma_{margin}} is 10 to 30 times the standard deviation of the MOV variable.
#' The U-step for the joint additive model involves updates based on the residual for the MOV and win prediction. In terms of the \eqn{i}th player, 
#' \deqn{R_{i+1} = R_i + K_{margin} (MOV_{ij} - \hat{MOV}_{ij}) + K_{win} (W_{ij} - \hat{W}_{ij})}.
#' Where \eqn{W_{ij}} is a 0-1 indicator for the win result. The unknown parameters are the constant learning rates \eqn{K_{margin}} and \eqn{K_{win}}. Typical values for \eqn{K_{win}} in the standard Elo system range from 20 to 40. A reasonable choice for \eqn{K_{margin}} is the standard deviation of the MOV variable.
#' @examples
#' # Grand Slam MOV Elo Rating
#' ratings <- joint_additive(~ winner, ~loser, ~ game_margin, data = atp_games, k.margin = 1.5, k.win = 24, scale.margin = 75, scale.win = 400)
#' @export
joint_additive <- function(winners, losers,
                           Date, margin, k.margin, k.win, scale.margin, scale.win, data, default = 1500) {		
  
  ratings <- list()
  
  if(missing(data)){
    winners <- as.character(winners)
    losers <- as.character(losers)
  }
  else{
    v <- variables(winners, losers, margin, data)						
    winners <- as.character(v[[1]])
    losers <- as.character(v[[2]])
    margin <- v[[3]]
  }
  
  if(any(is.na(margin))){
    
    warning("Missing values in MOV found and will be excluded.")
    
    exclude <- is.na(margin)
    winners <- winners[!exclude]
    losers <- losers[!exclude]
    margin <- margin[!exclude]
  }
  
  nlength <- length(winners)
  
  results <- data.frame(
    winner = winners,
    loser = losers,
    Date = Date,
    winner_margin = margin,
    winner_before_elo = numeric(nlength),
    loser_before_elo = numeric(nlength),
    win_prediction = numeric(nlength),
    margin_prediction = numeric(nlength),
    winner_elo = numeric(nlength), 
    loser_elo = numeric(nlength),
    stringsAsFactors = F
  )
  
  
  for (i in 1:nlength) {
    
    cur_winner <- winners[i]
    cur_loser <- losers[i]
    
    winner_elo <- lookup(cur_winner, ratings, default)
    loser_elo <- lookup(cur_loser, ratings, default)
    
    winner_margin <- (winner_elo - loser_elo) / scale.margin
    winner_prob <- 1/(1 + 10^(-1 * (winner_elo - loser_elo)/scale.win))
    
    winner_update <- k.margin * (margin[i] - winner_margin)  + k.win * (1 - winner_prob)
    
    loser_update <-  -1 * k.margin * (margin[i] - winner_margin) + k.win * (0 - (1 - winner_prob))
    
    ratings[[cur_winner]] = winner_elo + winner_update 
    ratings[[cur_loser]] = loser_elo + loser_update 	   
    results$margin_prediction[i] <- winner_margin
    results$win_prediction[i] <- winner_prob
    results$winner_elo[i] <- ratings[[cur_winner]]
    results$loser_elo[i] <- ratings[[cur_loser]]
    results$winner_before_elo[i] <- winner_elo
    results$loser_before_elo[i] <- loser_elo	    
    
  }
  
  results		
}




multiplicative <- function(winners, losers,Date, margin, k.win, scale.margin, scale.win, alpha,  data, default = 1500) {		
  
  ratings <- list()
  
  if(missing(data)){
    winners <- as.character(winners)
    losers <- as.character(losers)
  }
  else{
    v <- variables(winners, losers, margin, data)						
    winners <- as.character(v[[1]])
    losers <- as.character(v[[2]])
    margin <- v[[3]]
  }
  
  if(any(is.na(margin))){
    
    warning("Missing values in MOV found and will be excluded.")
    
    exclude <- is.na(margin)
    winners <- winners[!exclude]
    losers <- losers[!exclude]
    margin <- margin[!exclude]
  }
  
  nlength <- length(winners)
  
  results <- data.frame(
    winner = winners,
    loser = losers,
    Date = Date,
    winner_margin = margin,
    winner_before_elo = numeric(nlength),
    loser_before_elo = numeric(nlength),
    win_prediction = numeric(nlength),
    winner_elo = numeric(nlength), 
    loser_elo = numeric(nlength),
    stringsAsFactors = F
  )
  
  
  for (i in 1:nlength) {
    
    cur_winner <- winners[i]
    cur_loser <- losers[i]
    
    winner_elo <- lookup(cur_winner, ratings, default)
    loser_elo <- lookup(cur_loser, ratings, default)
    
    winner_margin <- abs(margin[i] / scale.margin)
    winner_prob <- 1/(1 + 10^(-1 * (winner_elo - loser_elo)/scale.win))
    
    winner_update <- k.win * (1 + winner_margin)^alpha * (1 - winner_prob)
    loser_update <-  -winner_update
    
    ratings[[cur_winner]] = winner_elo + winner_update 
    ratings[[cur_loser]] = loser_elo + loser_update 	   
    results$win_prediction[i] <- winner_prob
    results$winner_elo[i] <- ratings[[cur_winner]]
    results$loser_elo[i] <- ratings[[cur_loser]]
    results$winner_before_elo[i] <- winner_elo
    results$loser_before_elo[i] <- loser_elo	    
    
  }
  
  results		
}


logistic <- function(winners, losers, Date, margin, k.win, scale.margin, scale.win, alpha,  data, default = 1500) {		
  
  ratings <- list()
  
  if(missing(data)){
    winners <- as.character(winners)
    losers <- as.character(losers)
  }
  else{
    v <- variables(winners, losers, margin, data)						
    winners <- as.character(v[[1]])
    losers <- as.character(v[[2]])
    margin <- v[[3]]
  }
  
  if(any(is.na(margin))){
    
    warning("Missing values in MOV found and will be excluded.")
    
    exclude <- is.na(margin)
    winners <- winners[!exclude]
    losers <- losers[!exclude]
    margin <- margin[!exclude]
  }
  
  nlength <- length(winners)
  
  results <- data.frame(
    winner = winners,
    loser = losers,
    Date = Date,
    winner_margin = margin,
    winner_before_elo = numeric(nlength),
    loser_before_elo = numeric(nlength),
    win_prediction = numeric(nlength),
    winner_elo = numeric(nlength), 
    loser_elo = numeric(nlength),
    stringsAsFactors = F
  )
  
  
  for (i in 1:nlength) {
    
    cur_winner <- winners[i]
    cur_loser <- losers[i]
    
    winner_elo <- lookup(cur_winner, ratings, default)
    loser_elo <- lookup(cur_loser, ratings, default)
    
    winner_prob <- 1/(1 + alpha^(-1 * (winner_elo - loser_elo)/scale.win))
    margin_prob <- 1/(1 + alpha^(-margin[i]/scale.margin))
    
    winner_update <- k.win * (margin_prob - winner_prob)
    
    loser_update <-  -winner_update
    
    ratings[[cur_winner]] = winner_elo + winner_update 
    ratings[[cur_loser]] = loser_elo + loser_update 	   
    results$win_prediction[i] <- winner_prob
    results$winner_elo[i] <- ratings[[cur_winner]]
    results$loser_elo[i] <- ratings[[cur_loser]]
    results$winner_before_elo[i] <- winner_elo
    results$loser_before_elo[i] <- loser_elo	    
    
  }
  
  results		
}
