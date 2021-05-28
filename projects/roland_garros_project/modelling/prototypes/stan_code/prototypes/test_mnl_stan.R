### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====       Multinomial Logit Model in STAN       =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("~/tennis_analytics/projects/roland_garros_tracking_data/stan_code")
library(rstan) 
library(bayesplot) 
library(dplyr)
rstan_options(auto_write=TRUE) # writes a compiled Stan program to the disk to avoid recompiling
options(mc.cores = parallel::detectCores()) # uses multiple cores for stan



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ========== Generate Fake Multinomial Logit Data ==========
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- https://stats.stackexchange.com/questions/103728/simulating-multinomial-logit-data-with-r
# -- This is a weird parameterization (estimating all K logit equations)


set.seed(824)
# -- covariate matrix
# X = cbind(rep(1,500),
#           matrix(rnorm(2500), nrow=500, ncol=5))
X = matrix(rnorm(2000), nrow=1000, ncol=2)

# -- Add 4th predictor that is binary
X <- cbind(X,
           rbinom(n=1000, size = 1, prob = 0.25))


# coefficients for each choice 
# -- This includes a 4th term for the intercept
coef_class1 = rep(0, 4)
coef_class2 = rnorm(4)
coef_class3 = c(1,3,4,2.5)
# coef_class1 = rep(0, 3)
# coef_class2 = rnorm(3)
# coef_class3 = c(3,4,2.5)

# vector of probabilities
vProb = cbind(exp(cbind(rep(1,times=1000),X)%*%coef_class1), exp(cbind(rep(1,times = 1000),X)%*%coef_class2), exp(cbind(rep(1,times=1000),X)%*%coef_class3))
# vProb = cbind(exp(X%*%coef_class1), exp(X%*%coef_class2), exp(X%*%coef_class3))



# -- multinomial response draws
Y = t(apply(vProb, 1, rmultinom, n = 1, size = 1))

# -- Get Training / Test split and scale the continuous features
dfM = cbind.data.frame(y = apply(Y, 1, function(x) which(x==1)), X)
colnames(dfM) <- c('y', paste0('Var_', 1:3))

training_set <- dfM[1:800,]

# -- Scale continuous columns of training set
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

training_scaled <- rescale(training_set[,c(2,3)], 
                           training_set[,c(2,3)])

training_scaled <- cbind(y = training_set$y, training_scaled, Var_3 = training_set$Var_3)


test_set <- dfM[801:nrow(dfM),]
# Check out results (compare B2 & B3 to the fitted coefficients)
test_fit <- nnet::multinom(y ~ ., data = training_set)
summary(test_fit)


# Misclassification Errors
pred.class <- predict(test_fit, newdata=test_set)

(mul.misclass.test <- mean(ifelse(pred.class == test_set$y, yes=0, no=1)))

# Test set confusion matrix
table(test_set$y, pred.class, dnn=c("Obs","Pred"))


# -- "nnet" Performance on scaled version
scaled_cont_test <-  rescale(test_set[,c(2,3)], 
                             training_set[,c(2,3)])

test_set_scaled <-  cbind(y = test_set$y, 
                          scaled_cont_test, 
                          Var_3 = test_set$Var_3)

test_fit_scaled <- nnet::multinom(y ~ ., data = training_scaled)

pred.class.scaled <- predict(test_fit_scaled, newdata=test_set_scaled)
(mul.misclass.test.scaled <- mean(ifelse(pred.class.scaled == test_set_scaled$y, yes=0, no=1)))

# -- Test set confusion matrix
table(test_set$y, pred.class, dnn=c("Obs","Pred"))



# -- Try out Bayesian STAN model
fake_mnl <- list(
  K = 3, # Number of Response Categories
  N = 800, # Sample Size
  P = 3, # Number of Predictors
  y = training_set$y, # Response Column
  x = training_set[,-c(1)],
  
  ### -- Test set to make predictions
  N_new = 200,
  x_new = test_set[,-c(1)]
)

test.stan <- stan(file="shapo_stan.stan", data=fake_mnl, iter=1000, chains=4) 


summary(test.stan)$summary
summary(test.stan)$c_summary

plot(test.stan, plotfun="trace")
plot(test.stan, pars = 'beta')
plot(test.stan, plotfun="hist")
plot(test.stan, plotfun="dens")

# -- Evaluate STAN test performance
y_pred <- extract(test.stan)$y_new

get_column_prediction <- function(x){
  pred <- which.max(table(x))
  
  pred
  
}

stan_pred <- apply(MARGIN = 2,
                   X = y_pred,
                   FUN = get_column_prediction)

mean(ifelse(stan_pred == test_set$y, yes=0, no=1))


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
#    ==========   Multinomial.stan (from behavioural psychology)    ==========
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
# -- This stan code implements a different parameterization (K-1 logit equations)

fake_mnl <- list(
  K = 3, # Number of Response Categories
  N = 800, # Sample Size
  y = training_scaled$y, # Response Column
  x1 = training_scaled$Var_1,
  x2 = training_scaled$Var_2,
  x3 = training_scaled$Var_3,
  
  ### -- Test set to make predictions
  N_new = 200,
  x1_test = test_set_scaled$Var_1,
  x2_test = test_set_scaled$Var_2,
  x3_test = test_set_scaled$Var_3
)

test.stan <- stan(file="multinomial.stan", data=fake_mnl, iter=1000, chains=4) 
summary(test.stan)$summary
summary(test.stan)$c_summary

plot(test.stan, plotfun="trace")
plot(test.stan, pars = c('B_0', 'B_1', 'B_2', 'B_3'))
plot(test.stan, plotfun="hist")
plot(test.stan, plotfun="dens")

y_pred <- extract(test.stan)$y_new
colMeans(y_pred)

get_column_prediction <- function(x){
  pred <- which.max(table(x))
  
  pred
  
}

stan_pred <- apply(MARGIN = 2,
                   X = y_pred,
                   FUN = get_column_prediction)

mean(ifelse(stan_pred == test_set$y, yes=0, no=1))


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
#    ==========   Summarize  softmax probs  ==========
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
# -- Calculate the average prob of each class over all 2000 iterations

library(reshape)
y_probs <- extract(test.stan)$p_pred_vals


p1 <- y_probs[,,1]
colnames(p1) <- (paste0('obs_', 1:ncol(p1)))
p1 <- melt(p1)
colnames(p1) <- c('iter', 'obs_id', 'p1')

p2 <- y_probs[,,2]
colnames(p2) <- (paste0('obs_', 1:ncol(p2)))
p2 <- melt(p2)
colnames(p2) <- c('iter', 'obs_id', 'p2')

p3 <- y_probs[,,3]
colnames(p3) <- (paste0('obs_', 1:ncol(p3)))
p3 <- melt(p3)
colnames(p3) <- c('iter', 'obs_id', 'p3')

predicted_class_probs <- p1 %>%
  left_join(p2) %>%
  left_join(p3) %>%
  mutate(tot = p1+ p2 + p3)

predicted_class_probs %>%
  group_by(obs_id) %>%
  summarise(avg_p1 = mean(p1),
            avg_p2 = mean(p2),
            avg_p3 = mean(p3)) %>%
  mutate(tot = avg_p1 + avg_p2 + avg_p3) %>%
  View()


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
#    ==========   SIMULATION TEST NO. 2     ==========
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### #
# This simulation uses a parameterization consistent w/ K-1 logit models

#https://rawgit.com/rtrangucci/class_20170809/master/multinomial-logit/multinomial-logit-regression.html

set.seed(824)
N <- 1500
K <- 3
D <- 3

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

X <- matrix(rnorm(N * D), N, D)
beta <- cbind(matrix(rnorm((K - 1) * D), D, K - 1),0)
alpha <- c(rnorm(K - 1), 0)
mu <- sweep(x = X %*% beta, MARGIN = 2, STATS = alpha, FUN = '+')
mu_soft <- t(apply(mu, 1, softmax))
y <- sapply(1:N, function(x) rmultinom(1, size = 1, prob = mu_soft[x,]))
y <- apply(y, 2, function(x) which(as.logical(x)))


y_train <- y[1:1000]
y_test <- y[1001:1500]

x_train <- X[1:1000,]
x_test <- X[1001:1500,]

# -- Try out STAN
fake_mnl <- list(
  K = 3, # Number of Response Categories
  N = 1000, # Sample Size
  P = 3, # Number of Predictors
  y = y_train, # Response Column
  x1 = x_train[,1],
  x2 = x_train[,2],
  x3 = x_train[,3],
  
  ### -- Test set to make predictions
  N_new = 500,
  x1_test = x_test[,1],
  x2_test = x_test[,2],
  x3_test = x_test[,3]
)

test.stan <- stan(file="multinomial.stan", data=fake_mnl, iter=1000, chains=4) 

plot(test.stan, plotfun="trace")
plot(test.stan, pars = c('B_0', 'B_1', 'B_2', 'B_3'))
# Compare to:
alpha
t(beta)

# -- Compute STAN test error
y_pred <- extract(test.stan)$y_new
#extract(test.stan, pars ='y_new')
colMeans(y_pred)

get_column_prediction <- function(x){
  pred <- which.max(table(x))
  
  pred
  
}

stan_pred <- apply(MARGIN = 2,
                   X = y_pred,
                   FUN = get_column_prediction)

mean(ifelse(stan_pred == y_test, yes=0, no=1))


colnames(x_train) <- paste0('V_', 1:3)
colnames(x_test) <- paste0('V_', 1:3)

training_data <- data.frame(cbind(y_train, x_train))
test_data <- data.frame(cbind(y_test, x_test))
  
# -- Compare with nnet package
test_fit <- nnet::multinom(y_train ~ ., data = training_data)
summary(test_fit)

pred.class <- predict(test_fit, newdata=test_data)
(mul.misclass.test <- mean(ifelse(pred.class == y_test, yes=0, no=1)))
