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


# Set sampling fraction
sf1 = 0.7
sf2 = 1 - sf1

#  Logic:
#    1. randomly permute (reorder) numbers 1,2,...,n
#    2. take positions of first 70% as training set "set=1"
#    3. take positions of next 30% as validation set "set=2"
