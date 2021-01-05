#### Cross validation code

# * This script saves all iterations of the training and test data to evaluate
#   different algorithms
# -- Save Cross Validation Data Splits
# -- 5 Fold cross validation, repeated TWICE

setwd("/Users/petertea/tennis_analytics/projects/utr/")
library(dplyr)

utr_training_data <- read.csv('./utr_train_UPDATED.csv')

# -- Number of rows for training set
n_training = nrow(utr_training_data) 


# -- set repeated CV 2x, and 5 fold CV
num_splits <- 2
V <- 5

for (random_split in 1:num_splits){ 
  
  folds = floor((sample.int(n_training)-1)*V/n_training) + 1 
  
  utr_training_data$fold <- folds
  
  write.csv(x = utr_training_data,
            file =  paste("/Users/petertea/tennis_analytics/projects/utr/cross_validation/random_split", 
                          random_split, ".csv", sep = ''),
            row.names = FALSE
  )
  
}
  


