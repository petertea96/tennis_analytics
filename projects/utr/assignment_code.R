### UTR Assignment Script

# Chosen Model: Tuned Elo model with varying K depending on best of 3 / 5 matchup

set.seed(824)
setwd("/Users/petertea/tennis_analytics/projects/utr/")

library(dplyr)
library(lubridate)
library(EloRating)



training_data <- read.csv('utr_train_UPDATED.csv')

# -- Convert proper date format
training_data$resultmonth <- as.character(training_data$resultmonth)

# Dates in R need a day... so arbitrarily add day 1 for each date
training_data$date <- as.Date(paste0(training_data$resultmonth,1),
                         format = "%b-%y%d")

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
  arrange(date)

train_data_ordered$is_best_of_5 <- ifelse(train_data_ordered$is_best_of_5 == 1,
                                          'yes', 'no')
# *********************
# -- Train Elo model
# *********************
start_value = 1500
choose_double_k <- elo.seq(winner = train_data_ordered$winnerid,
                           loser = train_data_ordered$loserid,
                           Date = train_data_ordered$date,
                           intensity = train_data_ordered$is_best_of_5,
                           startvalue = start_value,
                           #k = myk,
                           runcheck = TRUE)

mykranges <- list(no = c(15, 30), yes = c(20, 40))

choose_double_k_values <- optimizek(choose_double_k ,
                                    krange = mykranges,
                                    resolution = 5)

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

### Apply model on Test Set
test_data <- read.csv('utr_test_UPDATED.csv')

# -- Convert proper date format
test_data$resultmonth <- as.character(test_data$resultmonth)

# Dates in R need a day... so arbitrarily add day 1 for each date
test_data$date <- as.Date(paste0(test_data$resultmonth,1),
                              format = "%b-%y%d")

test_data <- test_data %>%
  rowwise() %>%
  mutate(w_elo = extract_elo(tuned_double_k,
                             extractdate = date,
                             IDs = c(as.character(player1))),
         l_elo =extract_elo(tuned_double_k,
                            extractdate = date,
                            IDs = c(as.character(player2))))

test_data <- test_data %>%
  mutate( player1winprobability = winprob(w_elo,
                                          l_elo))


assignment_to_submit <- test_data %>%
  select(resultid, resultmonth, player1, player2, player1winprobability)

write.csv(assignment_to_submit, 'PeterTea.csv', row.names = FALSE)





### TEst
train_data_ordered$games_diff <- train_data_ordered$winner_games_won - train_data_ordered$loser_games_won


mult_train_model = multiplicative(winners=train_data_ordered$winnerid, 
                                  losers=train_data_ordered$loserid, 
                                  Date = train_data_ordered$date,
                                  margin = train_data_ordered$games_diff, 
                                  k.win=20,
                                  scale.margin=4, scale.win=400, alpha=1)


testing <- test_data %>%
  rowwise() %>%
  mutate(winner_elo = get_gig_elo(mult_train_model, player1, date),
         loser_elo =  get_gig_elo(mult_train_model, player2, date))


tosee=winprob(testing$winner_elo,
        testing$loser_elo)

hist(tosee)

