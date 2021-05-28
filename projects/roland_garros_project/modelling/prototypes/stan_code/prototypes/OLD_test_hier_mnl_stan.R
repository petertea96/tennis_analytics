### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====     Hierarchical Multinomial Logit Model in STAN        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("~/tennis_analytics/projects/roland_garros_tracking_data/stan_code/prototypes/")
source('../src/helper_functions.R')
library(rstan) 
library(bayesplot) 
library(dplyr)
rstan_options(auto_write=TRUE) # writes a compiled Stan program to the disk to avoid recompiling
options(mc.cores = parallel::detectCores()) # uses multiple cores for stan


set.seed(824)


### ### ### ### ### ### ### ### ###
### =====  Simulation No. 1  =====
### ### ### ### ### ### ### ### ###
# -- simulation from: 
# https://rawgit.com/rtrangucci/class_20170809/master/multinomial-logit/multinomial-logit-regression.html
# -- 3 fixed Covariates, 3 response levels.
# -- 2 Random Intercepts

N <- 2500
K <- 3 # No. classes
P <- 3 # No. predictors

# -- group size for each cluster group 
J_effect1 <- 15
J_effect2 <- 10

G <- 3
# ???

X <- matrix(rnorm(N * P), N, P) 
# (N x P) Fixed Effects Model Matrix

beta <- cbind(matrix(rnorm((K - 1) * P), P, K - 1),0)
# (P x K) Fixed slopes. Kth column is set to 0.

alpha <- c(rnorm(K - 1), 0) 
# Fixed Intercepts. Kth intercept is 0.

# -- Create random intercepts
eta_1 <- matrix(rnorm((K - 1) * J_effect1), K - 1, J_effect1)
# (K-1 x 15)

eta_2 <- matrix(rnorm((K - 1) * J_effect2), K - 1, J_effect2)
# (K-1 x 10)


# -- Initialize random intercept for each group
# (K x J) matrix
alpha_1 <- matrix(0, K, J_effect1)
alpha_2 <- matrix(0, K, J_effect2)

# Simulate sd of random effects
sigma_1 <- abs(rnorm(K - 1))
sigma_2 <- abs(rnorm(K - 1))

sigma_inter_eqn <- abs(rnorm(G))
# ???

for (k in 1:(K - 1)) {
  alpha_1[k,] <- sigma_inter_eqn[1] * sigma_1[k] * eta_1[k,]
  alpha_2[k,] <- sigma_inter_eqn[2] * sigma_2[k] * eta_2[k,]
}

# -- Set last row of random intercept matrix to zero.
alpha_1[K,] <- rep(0, J_effect1)
alpha_2[K,] <- rep(0, J_effect2)

# Create group labels for each observation.
idx_1 <- sample(J_effect1, N, replace = T)
idx_2 <- sample(J_effect2, N, replace = T)

mu <- sweep(x = X %*% beta, MARGIN = 2, STATS = alpha, FUN = '+')
mu <- t(t(mu) + alpha_1[, idx_1] + alpha_2[, idx_2])

mu_soft <- t(apply(mu, 1, softmax))
y <- sapply(1:N, function(x) rmultinom(1, size = 1, prob = mu_soft[x,]))
y <- apply(y, 2, function(x) which(as.logical(x)))


## --  For organization, save all info into a single dataframe
training_data <- data.frame(cbind(X, y, idx_1, idx_2))

# -- Fit MNL Hierarchical Model
hier_mnl <- stan_model(file = 'test_hier.stan')

stan_dat <- list(
  N = N,
  N_1 = length(unique(training_data$idx_1)),
  N_2 = length(unique(training_data$idx_2)),
  y = training_data$y,
  id_1 = training_data$idx_1,
  id_2 = training_data$idx_2,
  x1 = training_data$V1,
  x2 = training_data$V2,
  x3 = training_data$V3,
  K = K
)

fit_hier_mnl <- sampling(hier_mnl, data = stan_dat, iter = 1000)


# -- Summary diagnostics of MCMC
summary(fit_hier_mnl)$summary %>% View()
plot(fit_hier_mnl, plotfun="trace")

# Get posterior means & medians
names(extract(fit_hier_mnl))
extract_elements <- extract(fit_hier_mnl)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Check how good were our fixed effect estimates =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

plot(fit_hier_mnl, plotfun="hist", pars = c('B_0', 'B_1', 'B_2', 'B_3'))
plot(fit_hier_mnl, plotfun="dens",  pars = c('B_0', 'B_1', 'B_2', 'B_3'))


# -- Fixed Effects MCMC samples
B_0_samples <- extract_elements$B_0
B_1_samples <- extract_elements$B_1
B_2_samples <- extract_elements$B_2
B_3_samples <- extract_elements$B_3

# Compare Fixed intercept
colMeans(B_0_samples)
apply(B_0_samples, 2, median)
alpha

# Compare Fixed Slopes
apply(B_1_samples, 2, median)
apply(B_2_samples, 2, median)
apply(B_3_samples, 2, median)
beta


### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Check how good were our Random Effects estimates =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
plot(fit_hier_mnl, plotfun="dens", pars = c('v_id1'))

# -- Group 1
rand_intercept_1_samples <- extract_elements$v_id1
dim(rand_intercept_1_samples)

rbind(apply(rand_intercept_1_samples[,,1], 2, median),
      apply(rand_intercept_1_samples[,,2], 2, median)
      )

alpha_1

# -- Group 2
rand_intercept_2_samples <- extract_elements$v_id2
dim(rand_intercept_2_samples)

rbind(apply(rand_intercept_2_samples[,,1], 2, median),
      apply(rand_intercept_2_samples[,,2], 2, median)
)

alpha_2

## - Look at log-likelihood

log_lik <- extract(fit_hier_mnl)$log_lik































### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====     Hierarchical Multinomial Logit Model in STAN No.2       =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("~/tennis_analytics/projects/roland_garros_tracking_data/stan_code/prototypes/")
source('../src/helper_functions.R')
library(rstan) 
library(bayesplot) 
library(dplyr)
rstan_options(auto_write=TRUE) # writes a compiled Stan program to the disk to avoid recompiling
options(mc.cores = parallel::detectCores()) # uses multiple cores for stan


set.seed(824)


### ### ### ### ### ### ### ### ###
### =====  Simulation No. 2  =====
### ### ### ### ### ### ### ### ###
# -- simulation from: 
# https://rawgit.com/rtrangucci/class_20170809/master/multinomial-logit/multinomial-logit-regression.html
# -- 3 fixed Covariates, 3 response levels.
# -- 3 random intercepts varying by Age (5 groups), Ethnicity? (4 groups), Education ( 5 groups)
N <- 2000
K <- 3 # No. classes
D <- 3 # No. predictors
J_age <- 15
J_eth <- 4
J_edu <- 5
G <- 3
X <- matrix(rnorm(N * D), N, D) 
# Fixed Effects Model Matrix

beta <- cbind(matrix(rnorm((K - 1) * D), D, K - 1),0)
# Fixed slopes
alpha <- c(rnorm(K - 1), 0) 
# Fixed Intercepts

# -- Compute random interceps
eta_age <- matrix(rnorm((K - 1) * J_age), K - 1, J_age)
eta_eth <- matrix(rnorm((K - 1) * J_eth), K - 1, J_eth)
eta_edu <- matrix(rnorm((K - 1) * J_edu), K - 1, J_edu)
alpha_age <- matrix(0, K, J_age)
alpha_eth <- matrix(0, K, J_eth)
alpha_edu <- matrix(0, K, J_edu)
sigma_age <- abs(rnorm(K - 1))
sigma_eth <- abs(rnorm(K - 1))
sigma_edu <- abs(rnorm(K - 1))
sigma_inter_eqn <- abs(rnorm(G))
for (k in 1:(K - 1)) {
  alpha_age[k,] <- sigma_inter_eqn[1] * sigma_age[k] * eta_age[k,]
  alpha_eth[k,] <- sigma_inter_eqn[2] * sigma_eth[k] * eta_eth[k,]
  alpha_edu[k,] <- sigma_inter_eqn[3] * sigma_edu[k] * eta_edu[k,]
}
alpha_age[K,] <- rep(0, J_age)
alpha_eth[K,] <- rep(0, J_eth)
alpha_edu[K,] <- rep(0, J_edu)

idx_age <- sample(J_age, N, replace = T)
idx_eth <- sample(J_eth, N, replace = T)
idx_edu <- sample(J_edu, N, replace = T)

mu <- sweep(x = X %*% beta, MARGIN = 2, STATS = alpha, FUN = '+')
mu <- t(t(mu) + alpha_age[, idx_age] + alpha_eth[, idx_eth] + alpha_edu[, idx_edu])

mu_soft <- t(apply(mu, 1, softmax))
y <- sapply(1:N, function(x) rmultinom(1, size = 1, prob = mu_soft[x,]))
y <- apply(y, 2, function(x) which(as.logical(x)))


## --  For organization, save all info into a single dataframe

training_data <- data.frame(cbind(X, y, idx_age, idx_eth, idx_edu))


hier_mnl <- stan_model(file = 'test_hier.stan')

stan_dat <- list(
  N = N,
  N_id = length(unique(training_data$idx_age)),
  y = training_data$y,
  id = training_data$idx_age,
  age_z = training_data$V1,
  age_zq = training_data$V2,
  wz = training_data$V3,
  K = K
)
fit_hier_mnl <- sampling(hier_mnl, data = stan_dat, iter = 1000)

summary(fit_hier_mnl)$summary

plot(fit_hier_mnl, plotfun="trace")

plot(fit_hier_mnl, plotfun="hist")
plot(test.stan, plotfun="dens")

# compare to true values
alpha
beta


