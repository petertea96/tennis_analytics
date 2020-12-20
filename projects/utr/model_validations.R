# -- Apply all candidate methods in CV

setwd("/Users/petertea/tennis_analytics/projects/utr/")
source('gig_functions.R')
source('mov_functions.R')
library(dplyr)
library(lubridate)
library(EloRating)

num_methods <- 6
num_splits <- 2
V <- 5

##########################################################################
# Methods: Default elo, tuned k, tuned double k, mov_joint_additive, mov_multiplicative,
# mov_logistic

# MSPE_data_matrix = matrix(NA, nrow=10, ncol=num_methods)
# sMSE_data_matrix = matrix(NA, nrow=10, ncol=num_methods)
# 
# train_brier_matrix <- matrix(NA, nrow=10, ncol=num_methods) 
# test_brier_matrix <- matrix(NA, nrow=10, ncol=num_methods)
# 
# method_names <- c('default_elo', 'tuned_k')
# 
# colnames(MSPE_data_matrix) <- method_names
# colnames(sMSE_data_matrix) <- method_names
# colnames(train_brier_matrix) <- method_names
# colnames(test_brier_matrix) <- method_names

# Read in previously saved results data
sMSE_data_matrix <- read.csv('./model_performance/sMSE_data_matrix.csv')
MSPE_data_matrix <- read.csv('./model_performance/MSPE_data_matrix.csv')
train_brier_matrix <- read.csv('./model_performance/train_brier_matrix.csv')
test_brier_matrix <- read.csv('./model_performance/test_brier_matrix.csv')

set.seed(23)
for (random_split in 1:num_splits){ 
  
  utr_data <- read.csv(paste("./cross_validation/random_split", 
                             random_split, ".csv", sep = ''))
  
  # -- Convert proper date format
  utr_data$resultmonth <- as.character(utr_data$resultmonth)
  
  # Dates in R need a day... so arbitrarily add day 1 for each date
  utr_data$date <- as.Date(paste0(utr_data$resultmonth,1),
                             format = "%b-%y%d")
  
  for(cv_fold in 1:V){
    # -- Set up Train-Test Split
    training_data <- utr_data %>% dplyr::filter(fold != cv_fold)
    test_data <-  utr_data %>% dplyr::filter(fold == cv_fold)
    
    # -- Order the training data
    train_data_ordered <- training_data %>%
      rowwise() %>%
      mutate(winner_sets_won = sum(c((winnerset1 > loserset1),
                                     (winnerset2 > loserset2),
                                     (winnerset3 > loserset3),
                                     (winnerset4 > loserset4),
                                     (winnerset5 > loserset5))),
             loser_sets_won = sum(c(winnerset1 < loserset1,
                                    winnerset2 < loserset2,
                                    winnerset3 < loserset3,
                                    winnerset4 < loserset4,
                                    winnerset5 < loserset5)),
             winner_games_won = sum(c(winnerset1, winnerset2,
                                      winnerset3,winnerset4,
                                      winnerset5)),
             loser_games_won = sum(c(loserset1, loserset2,
                                     loserset3,loserset4,
                                     loserset5)),
             is_best_of_5 = ifelse(winner_sets_won == 3, 1,0)) %>%
      arrange(date, resultid)
    
    
    # Fit Elo
    # by default, start value is 1000
    start_value = 1500
    default_elo_model <- elo.seq(winner = train_data_ordered$winnerid,
                                 loser = train_data_ordered$loserid,
                                 Date = train_data_ordered$date,
                                 startvalue = start_value,
                                 runcheck = TRUE)

    # Model results
    train_data_ordered <- train_data_ordered %>%
      rowwise() %>%
      mutate(w_elo = extract_elo(default_elo_model,
                                 extractdate = date,
                                 IDs = c(as.character(winnerid))),
             l_elo =extract_elo(default_elo_model,
                                extractdate = date,
                                IDs = c(as.character(loserid))))
    #
    train_misclass <- sum(train_data_ordered$w_elo < train_data_ordered$l_elo)/nrow(train_data_ordered)

    train_brier <- mean((winprob(train_data_ordered$w_elo,
                           train_data_ordered$l_elo) - 1)^2)
    #
    # # Test results
    test_data <- test_data %>%
      rowwise() %>%
      mutate(w_elo = tryCatch(extract_elo(default_elo_model,
                                          extractdate =date,
                                          IDs = c(as.character(winnerid))),
                              error=function(cond) {
                                #message("Here's the original error message:")
                                #message(cond)
                                # Choose a return value in case of error
                                return(start_value)
                              }),

             l_elo =tryCatch(extract_elo(default_elo_model,
                                         extractdate =date,
                                         IDs = c(as.character(loserid))),
                             error=function(cond) {
                               #message("Here's the original error message:")
                               #message(cond)
                               # Choose a return value in case of error
                               return(start_value)
                             })

               )
    #
    test_misclass <- sum(test_data$w_elo < test_data$l_elo)/nrow(test_data)
    test_brier <- mean((winprob(test_data$w_elo,
                                test_data$l_elo) - 1)^2)


    sMSE_data_matrix[(random_split-1)*V+cv_fold, 1] <- train_misclass
    MSPE_data_matrix[(random_split-1)*V+cv_fold, 1] <- test_misclass

    train_brier_matrix[(random_split-1)*V+cv_fold, 1] <- train_brier
    test_brier_matrix[(random_split-1)*V+cv_fold, 1] <- test_brier
  
    # ******************************************************************
    ## Model 2 (Tuned single k elo)   
    # ******************************************************************
    ### -- Tuned K value

    ## optimize k value
    get_optimise_k_elo <- optimizek(default_elo_model,
                                krange = c(10, 100), resolution = 100)



    if(get_optimise_k_elo$best$k %in%  c(10, 100)){
      print(paste('boundary value detected:', cv_fold, random_split))
      return(NULL)

    }

    elo_model_tuned_k <- elo.seq(winner = train_data_ordered$winnerid,
                                 loser = train_data_ordered$loserid,
                                 Date = train_data_ordered$date,
                                 startvalue = start_value,
                                 k = get_optimise_k_elo$best$k)

    train_data_ordered <- train_data_ordered %>%
      rowwise() %>%
      mutate(w_elo = extract_elo(elo_model_tuned_k,
                                 extractdate = date,
                                 IDs = c(as.character(winnerid))),
             l_elo =extract_elo(elo_model_tuned_k,
                                extractdate = date,
                                IDs = c(as.character(loserid))))

    train_misclass <- sum(train_data_ordered$w_elo < train_data_ordered$l_elo)/nrow(train_data_ordered)

    train_brier <- mean((winprob(train_data_ordered$w_elo,
                                 train_data_ordered$l_elo) - 1)^2)
    #
    # # Test results
    test_data <- test_data %>%
      rowwise() %>%
      mutate(w_elo = tryCatch(extract_elo(elo_model_tuned_k,
                                          extractdate =date,
                                          IDs = c(as.character(winnerid))),
                              error=function(cond) {
                                #message("Here's the original error message:")
                                #message(cond)
                                # Choose a return value in case of error
                                return(start_value)
                              }),

             l_elo =tryCatch(extract_elo(elo_model_tuned_k,
                                         extractdate =date,
                                         IDs = c(as.character(loserid))),
                             error=function(cond) {
                               #message("Here's the original error message:")
                               #message(cond)
                               # Choose a return value in case of error
                               return(start_value)
                             })

      )
    #
    test_misclass <- sum(test_data$w_elo < test_data$l_elo)/nrow(test_data)
    test_brier <- mean((winprob(test_data$w_elo,
                                test_data$l_elo) - 1)^2)


    sMSE_data_matrix[(random_split-1)*V+cv_fold, 2] <- train_misclass
    MSPE_data_matrix[(random_split-1)*V+cv_fold, 2] <- test_misclass

    train_brier_matrix[(random_split-1)*V+cv_fold, 2] <- train_brier
    test_brier_matrix[(random_split-1)*V+cv_fold, 2] <- test_brier

    
    ### *********************************************************************
    # TUNED DOUBLE K (considering best of 5/ best of 3 weights)
    # Should have more weight added to a best of 5 match
    #myk <- list(no = 50, yes = 200)
    # -- Add different weighting factor for best of 5 vs best of 3
    train_data_ordered$is_best_of_5 <- ifelse(train_data_ordered$is_best_of_5 == 1,
                                              'yes', 'no')

    choose_double_k <- elo.seq(winner = train_data_ordered$winnerid,
                               loser = train_data_ordered$loserid,
                               Date = train_data_ordered$date,
                               intensity = train_data_ordered$is_best_of_5,
                               startvalue = start_value,
                               #k = myk,
                               runcheck = TRUE)
    #
    # # -- Tune both k values: (should see larger k for best of 5 and lower k for best of 3...)
    mykranges <- list(no = c(15, 30), yes = c(20, 40))
    #
    # # -- Time to run: ~ 2 minutes
    choose_double_k_values <- optimizek(choose_double_k ,
                                        krange = mykranges,
                                        resolution = 5)
    #
    myk <- list(no = choose_double_k_values$best$no,
                yes =choose_double_k_values$best$yes)

    tuned_double_k <- elo.seq(winner = train_data_ordered$winnerid,
                              loser = train_data_ordered$loserid,
                              Date = train_data_ordered$date,
                              intensity = train_data_ordered$is_best_of_5,
                              startvalue = start_value,
                              k = myk,
                              runcheck = TRUE)

    train_data_ordered <- train_data_ordered %>%
      rowwise() %>%
      mutate(w_elo = extract_elo(tuned_double_k,
                                 extractdate = date,
                                 IDs = c(as.character(winnerid))),
             l_elo =extract_elo(tuned_double_k,
                                extractdate = date,
                                IDs = c(as.character(loserid))))

    train_misclass <- sum(train_data_ordered$w_elo < train_data_ordered$l_elo)/nrow(train_data_ordered)

    train_brier <- mean((winprob(train_data_ordered$w_elo,
                                 train_data_ordered$l_elo) - 1)^2)

    # Test results
    test_data <- test_data %>%
      rowwise() %>%
      mutate(w_elo = tryCatch(extract_elo(tuned_double_k,
                                          extractdate =date,
                                          IDs = c(as.character(winnerid))),
                              error=function(cond) {
                                #message("Here's the original error message:")
                                #message(cond)
                                # Choose a return value in case of error
                                return(1000)
                              }),

             l_elo =tryCatch(extract_elo(tuned_double_k,
                                         extractdate =date,
                                         IDs = c(as.character(loserid))),
                             error=function(cond) {
                               #message("Here's the original error message:")
                               #message(cond)
                               # Choose a return value in case of error
                               return(1000)
                             })

      )

    test_misclass <- sum(test_data$w_elo < test_data$l_elo)/nrow(test_data)
    test_brier <- mean((winprob(test_data$w_elo,
                                test_data$l_elo) - 1)^2)


    sMSE_data_matrix[(random_split-1)*V+cv_fold, 3] <- train_misclass
    MSPE_data_matrix[(random_split-1)*V+cv_fold, 3] <- test_misclass

    train_brier_matrix[(random_split-1)*V+cv_fold, 3] <- train_brier
    test_brier_matrix[(random_split-1)*V+cv_fold, 3] <- test_brier
    # 
    # ***************************************************
    ### -- MOV Models
    train_data_ordered$games_diff <- train_data_ordered$winner_games_won - train_data_ordered$loser_games_won
    
    
    # 
    # mov_joint_add_errors <- save_ja_validaton_error(training_dataset = train_data_ordered,
    #                                                 validation_dataset = test_data,
    #                                                 k.margin = 2.5,
    #                                                 k.win = 24,
    #                                                 scale.margin = 75,
    #                                                 scale.win = 400)
    # 
    # MSPE_data_matrix[(random_split-1)*V+cv_fold, 4] <- mov_joint_add_errors[1]
    # test_brier_matrix[(random_split-1)*V+cv_fold, 4] <- mov_joint_add_errors[2]
    
    #-- MOV Multiplicative model
    # k.win.vec = seq(18,22, by=1) 
    # scale.margin.vec = seq(2,6, by = 1)
    # alpha =1
    # scale.win=400
    # # 
    # param_search_matrix <- expand.grid(k.win = k.win.vec,
    #                                    scale.margin = scale.margin.vec,
    #                                    alpha = alpha,
    #                                    scale.win = scale.win)
    # resamp <- sample.int(n=nrow(train_data_ordered),
    #                      size=nrow(train_data_ordered)*0.8, replace=FALSE)
    # cv_training_data <-train_data_ordered[resamp,]
    # 
    # cv_validation_boot <- train_data_ordered[-unique(resamp),]
    # val_error_b <- mapply(save_mov_log_validaton_error,
    #                       MoreArgs = list(training_dataset=cv_training_data,
    #                                       validation_dataset=cv_validation_boot),
    #                       param_search_matrix$k.win,
    #                       param_search_matrix$scale.margin,
    #                       param_search_matrix$scale.win,
    #                       param_search_matrix$alpha)
    # 
    # min_search <- which.min(t(val_error_b)[,2])
    # 
    # k.win_tuned <- param_search_matrix[min_search,1]
    # scale.margin_tuned <- param_search_matrix[min_search,2]
    # 
    # mov_mult_errors <- save_mov_mult_validaton_error(training_dataset = train_data_ordered,
    #                                                  validation_dataset = test_data,
    #                                                  k.win = k.win_tuned,
    #                                                  scale.margin = scale.margin_tuned,
    #                                                  alpha = 1.25,
    #                                                  scale.win =400)
    # # 
    # MSPE_data_matrix[(random_split-1)*V+cv_fold, 5] <- mov_mult_errors[1]
    # test_brier_matrix[(random_split-1)*V+cv_fold, 5] <- mov_mult_errors[2]
    
    # -- MOV Logistic model
    # logistic_mov_train_model = logistic(winners=train_data_ordered$winnerid, 
    #                                     losers=train_data_ordered$loserid, 
    #                                     Date = train_data_ordered$date,
    #                                     margin = train_data_ordered$games_diff, 
    #                                     k.win = 100, 
    #                                     scale.margin = 2.5,
    #                                     alpha = 2,
    #                                     scale.win =400)
    # mov_log_errors <- save_mov_log_validaton_error(training_dataset = train_data_ordered,
    #                                                validation_dataset = test_data)
    # 
    # MSPE_data_matrix[(random_split-1)*V+cv_fold, 6] <- mov_log_errors[1]
    # test_brier_matrix[(random_split-1)*V+cv_fold, 6] <- mov_log_errors[2]    
    

  }
}

method_names <- c('default_elo', 'tuned_k', 'tuned_double_k', 'mov_joint_add', 'mov_mult',
                  'mov_logistic')

colnames(MSPE_data_matrix) <- method_names
#colnames(sMSE_data_matrix) <- method_names
#colnames(train_brier_matrix) <- method_names
colnames(test_brier_matrix) <- method_names

#write.csv(sMSE_data_matrix, './model_performance/sMSE_data_matrix.csv', row.names = FALSE)
write.csv(MSPE_data_matrix, './model_performance/MSPE_data_matrix.csv', row.names = FALSE)
#write.csv(train_brier_matrix, './model_performance/train_brier_matrix.csv', row.names = FALSE)
write.csv(test_brier_matrix, './model_performance/test_brier_matrix.csv', row.names = FALSE)

# Prelim. view of results
colMeans(MSPE_data_matrix)
colMeans(test_brier_matrix)

apply(test_brier_matrix,2,median)
apply(MSPE_data_matrix,2,median)


