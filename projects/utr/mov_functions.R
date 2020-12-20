### Functions to apply Kovalchik's MOV elo models

source('gig_functions.R')
library(dplyr)

# From a model df of elo results (formulated in GIG format),
# return player elo rating at end of a specific day
get_gig_elo <- function(model_df, player_id, cur_date, default=1500){
  
  player_id <- as.character(player_id)
  
  last_entry <- model_df %>%
    filter(Date <= cur_date) %>% 
    filter(winner == player_id | loser == player_id) %>% tail(1)
  
  if(nrow(last_entry) == 0){
    return(default)
  }
  
  if (last_entry$winner == player_id){
    return(last_entry$winner_elo)
  } else if(last_entry$loser == player_id){
    return(last_entry$loser_elo)
  } 
  
  return(NULL)
  
  
}

# Apply joint additive elo model
# From paper: 
# k.margin = 2.5
# k.win = 24
# scale.margin = 75
# scale.win = 400
save_ja_validaton_error <- function(training_dataset,
                                    validation_dataset,
                                    k.margin, k.win,
                                    scale.margin, scale.win){
  
  ja_train_model = joint_additive(winners=training_dataset$winnerid, 
                                  losers=training_dataset$loserid, 
                                  Date = training_dataset$date,
                                  margin = training_dataset$games_diff, 
                                  k.margin = k.margin, 
                                  k.win = k.win, 
                                  scale.margin = scale.margin,
                                  scale.win = scale.win , default = 1500)
  
  testing <- validation_dataset %>%
    rowwise() %>%
    mutate(winner_elo = get_gig_elo(ja_train_model, winnerid, date),
           loser_elo =  get_gig_elo(ja_train_model, loserid, date))
  
  test_misclass <- sum(testing$winner_elo < testing$loser_elo)/nrow(testing)
  test_brier <- mean((winprob(testing$winner_elo,
                               testing$loser_elo) - 1)^2)
  
  return(c(test_misclass, test_brier))
}


save_mov_mult_validaton_error <- function(training_dataset,
                                    validation_dataset,
                                    k.win=20,
                                    scale.margin=4, scale.win=400, alpha=1){
  
  mult_train_model = multiplicative(winners=training_dataset$winnerid, 
                                    losers=training_dataset$loserid, 
                                    Date = training_dataset$date,
                                    margin = training_dataset$games_diff, 
                                    k.win = k.win, 
                                    scale.margin = scale.margin,
                                    alpha = alpha,
                                    scale.win =scale.win)
  
  
  testing <- validation_dataset %>%
    rowwise() %>%
    mutate(winner_elo = get_gig_elo(mult_train_model, winnerid, date),
           loser_elo =  get_gig_elo(mult_train_model, loserid, date))
  
  test_misclass <- sum(testing$winner_elo < testing$loser_elo)/nrow(testing)
  test_brier <- mean((winprob(testing$winner_elo,
                              testing$loser_elo) - 1)^2)
  
  return(c(test_misclass, test_brier))
}


save_mov_log_validaton_error <- function(training_dataset,
                                         validation_dataset,
                                         k.win = 100, 
                                         scale.margin = 2.5,
                                         alpha = 2,
                                         scale.win = 400){
  
  logistic_mov_train_model = logistic(winners = training_dataset$winnerid, 
                                      losers = training_dataset$loserid, 
                                      Date = training_dataset$date,
                                      margin = training_dataset$games_diff, 
                                      k.win = k.win, 
                                      scale.margin = scale.margin,
                                      alpha = alpha,
                                      scale.win = scale.win)
  
  testing <- validation_dataset %>%
    rowwise() %>%
    mutate(winner_elo = get_gig_elo(logistic_mov_train_model, winnerid, date),
           loser_elo =  get_gig_elo(logistic_mov_train_model, loserid, date))
  
  test_misclass <- sum(testing$winner_elo < testing$loser_elo)/nrow(testing)
  test_brier <- mean((winprob(testing$winner_elo,
                              testing$loser_elo) - 1)^2)
  
  return(c(test_misclass, test_brier))
}



### Feeble attempt at Parameter Tuning

# boot_reps <- 8
# k.margin.vec <- seq(2.5,4.5, by =0.5)
# k.win.vec <- seq(23,25, by = 1)
# scale.margin.vec <- 75 #seq(60,80, by = 5)
# scale.win.vec <- 400
# 
# param_search_matrix <- expand.grid(k.margin = k.margin.vec,
#                                    k.win = k.win.vec,
#                                    scale.margin = scale.margin.vec,
#                                    scale.win = scale.win.vec)
# 
# MSPR <- matrix(NA, nrow=length(k.margin.vec)*length(k.win.vec)*length(scale.margin.vec)*length(scale.win.vec),
#                ncol=boot_reps+4)
# 
# for(boot_ind in 1:boot_reps){
#   resamp <- sample.int(n=nrow(train_data_ordered), 
#                        size=nrow(train_data_ordered), replace=TRUE)
#   training_data_boot <-train_data_ordered[resamp,]
#   
#   validation_boot <- train_data_ordered[-unique(resamp),]
#   
#   val_error_b <- mapply(save_ja_validaton_error, 
#                         MoreArgs = list(training_dataset=training_data_boot,
#                                         validation_dataset=validation_boot),
#                         param_search_matrix$k.margin,
#                         param_search_matrix$k.win,
#                         param_search_matrix$scale.margin,
#                         param_search_matrix$scale.win)
#   
#   MSPR[,boot_ind+4] = val_error_b
#   
#   
# }


### -- Tuning MOV Multiplicative Model
# boot_reps <- 8
# 
# k.win.vec = seq(18,22, by=1) 
# scale.margin.vec = seq(2,6, by = 1)
# alpha.vec = seq(0.75, 1.5, by = 0.25)
# scale.win.vec =400
# 
# param_search_matrix <- expand.grid(k.win = k.win.vec,
#                                    scale.margin = scale.margin.vec,
#                                    alpha = alpha.vec,
#                                    scale.win = scale.win.vec)
# 
# MSPR <- matrix(NA, nrow=nrow(param_search_matrix),
#                ncol=boot_reps+4)
# 
# for(boot_ind in 1:boot_reps){
#   resamp <- sample.int(n=nrow(train_data_ordered),
#                        size=nrow(train_data_ordered), replace=TRUE)
#   training_data_boot <-train_data_ordered[resamp,]
# 
#   validation_boot <- train_data_ordered[-unique(resamp),]
# 
#   val_error_b <- mapply(save_mov_log_validaton_error,
#                         MoreArgs = list(training_dataset=training_data_boot,
#                                         validation_dataset=validation_boot),
#                         param_search_matrix$k.win,
#                         param_search_matrix$scale.margin,
#                         param_search_matrix$scale.win,
#                         param_search_matrix$alpha)
# 
#   MSPR[,boot_ind+4] = val_error_b
# 
# 
# }

