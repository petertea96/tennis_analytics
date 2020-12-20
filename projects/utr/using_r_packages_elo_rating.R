### No time to code everything from scratch...
# -- apply elo with these R packages

# ----------------------------------
# -- Extra features to consider?:
# ----------------------------------
# -- is_best_of_5 {0,1}
# -- Difference in games won
# -- Difference un sets won

setwd("/Users/petertea/tennis_analytics/projects/utr/")
library(dplyr)
library(ggplot2)
library(lubridate)

# -- Read in training data
train_data <- read.csv('./utr_train_UPDATED.csv')
# -- Convert proper date format
train_data$resultmonth <- as.character(train_data$resultmonth)

# Dates in R need a day... so arbitrarily add day 1 for each date
train_data$date <- as.Date(paste0(train_data$resultmonth,1),
                           format = "%b-%y%d")

# -- Add columns on number of sets won, number of games won, indicator for
#    best of 3/5

train_data_ordered <- train_data %>%
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

# ***** || ***** || ***** || ***** ||***** || ***** ||***** || ***** || 
### -- EloRating R Package
# ***** || ***** || ***** || ***** ||***** || ***** ||***** || ***** || 
#install.packages('EloRating')
library(EloRating)

seqcheck(winner = train_data_ordered$winnerid, 
         loser = train_data_ordered$loserid, Date = train_data_ordered$date)

res <- elo.seq(winner = train_data_ordered$winnerid, 
               loser = train_data_ordered$loserid, 
               Date = train_data_ordered$date, 
               runcheck = TRUE)
summary(res)

extract_elo(res, extractdate = "2019-03-19",IDs = c('224142'))
# extract_elo gives you elo ratings at end of day?

# We expect the majority of precicted elo of winner to be larger than
# the loser. This is to check:
test_elo <- train_data_ordered %>%
  rowwise() %>%
  mutate(w_elo = extract_elo(res, 
                             extractdate = date, 
                             IDs = c(as.character(winnerid))),
         l_elo =extract_elo(res, 
                            extractdate = date, 
                            IDs = c(as.character(loserid))))
  
sum(test_elo$w_elo > test_elo$l_elo)/nrow(test_elo)


## optimize k value
ores <- optimizek(res, krange = c(10, 100), resolution = 100)
ores$best
plot(ores$complete$k, ores$complete$loglik, type = "l", las = 1, xlab = bquote(italic(k)), ylab = "log likelihood")
abline(v = ores$best$k, col = "red")

res2 <- elo.seq(winner = train_data_ordered$winnerid, 
               loser = train_data_ordered$loserid, 
               Date = train_data_ordered$date,
               k = ores$best$k)

test_elo <- train_data_ordered %>%
  rowwise() %>%
  mutate(w_elo = extract_elo(res2, 
                             extractdate = date, 
                             IDs = c(as.character(winnerid))),
         l_elo =extract_elo(res2, 
                            extractdate = date, 
                            IDs = c(as.character(loserid))))

sum(test_elo$w_elo > test_elo$l_elo)/nrow(test_elo)

# -- Add different weighting factor for best of 5 vs best of 3
train_data_ordered$is_best_of_5 <- ifelse(train_data_ordered$is_best_of_5 == 1,
                                          'yes', 'no')
#myk <- list(no = 50, yes = 200)
res3 <- elo.seq(winner = train_data_ordered$winnerid, 
                loser = train_data_ordered$loserid, 
                Date = train_data_ordered$date, 
                intensity = train_data_ordered$is_best_of_5,
                #k = myk,
                runcheck = TRUE)

# -- Tune both k values: (should see larger k for best of 5 and lower k for best of 3...)
mykranges <- list(no = c(15, 50), yes = c(15, 50))
ores2 <- optimizek(res3, krange = mykranges, resolution = 25)
ores2$best

test_elo <- train_data_ordered %>%
  rowwise() %>%
  mutate(w_elo = extract_elo(res3, 
                             extractdate = date, 
                             IDs = c(as.character(winnerid))),
         l_elo =extract_elo(res3, 
                            extractdate = date, 
                            IDs = c(as.character(loserid))))

sum(test_elo$w_elo > test_elo$l_elo)/nrow(test_elo)

# Optimize K
res4 <- elo.seq(winner = train_data_ordered$winnerid, 
                loser = train_data_ordered$loserid, 
                Date = train_data_ordered$date, 
                intensity = train_data_ordered$is_best_of_5,
                runcheck = TRUE)
mykranges <- list(no = c(10, 150), yes = c(10, 150))
ores2 <- optimizek(res4, krange = mykranges, resolution = 91)

# ***** || ***** || ***** || ***** ||***** || ***** ||***** || ***** || 
### -- GIGTennis R package
# ***** || ***** || ***** || ***** ||***** || ***** ||***** || ***** || 

# -- Joint Additive model: New Elo ranking changes depending on 
#    match outcome AND margin of victory

# -- Multiplicative: Models a non-linear learning rate

#remotes::install_github("GIGTennis/elomov")


random_split = 1
utr_data <- read.csv(paste("./cross_validation/random_split", 
                           random_split, ".csv", sep = ''))

# -- Convert proper date format
utr_data$resultmonth <- as.character(utr_data$resultmonth)

# Dates in R need a day... so arbitrarily add day 1 for each date
utr_data$date <- as.Date(paste0(utr_data$resultmonth,1),
                         format = "%b-%y%d")

cv_fold =1
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

train_data_ordered$games_diff <- train_data_ordered$winner_games_won - train_data_ordered$loser_games_won


### Additive MOV



### Multiplicative MOV

mult_train_model = multiplicative(winners=train_data_ordered$winnerid, 
                                losers=train_data_ordered$loserid, 
                                Date = train_data_ordered$date,
                                margin = train_data_ordered$games_diff, 
                                k.win = 20, 
                                scale.margin = 4,
                                alpha = 1,
                                scale.win =400)


### Logistic MOV

logistic_mov_train_model = logistic(winners=train_data_ordered$winnerid, 
                                    losers=train_data_ordered$loserid, 
                                    Date = train_data_ordered$date,
                                    margin = train_data_ordered$games_diff, 
                                    k.win = 100, 
                                    scale.margin = 2.5,
                                    alpha = 2,
                                    scale.win =400)



