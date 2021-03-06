### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====     Hierarchical Multinomial Logit Model in STAN        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Last updated: March 1st, 2021
# -- Purpose: Simulate multinomial model consistent with STAN code
# -- Check whether STAN results are reasonable (i.e. do fixed and random effects match?)


### ### ### ### ### ### ### ### ### 
### =====   Load libraries   ===== 
### ### ### ### ### ### ### ### ### 
setwd("~/tennis_analytics/projects/roland_garros_tracking_data/stan_code/prototypes/")
source('../src/helper_functions.R')
library(rstan) 
library(bayesplot) 
library(dplyr)
rstan_options(auto_write=TRUE) # writes a compiled Stan program to the disk to avoid recompiling
options(mc.cores = parallel::detectCores()) # uses multiple cores for stan

set.seed(824)


### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====  Simulate Bayesian Hierarchical MNL  =====
### ### ### ### ### ### ### ### ### ### ### ### ### ###

# -- Simulations inspired from : 
# https://rawgit.com/rtrangucci/class_20170809/master/multinomial-logit/multinomial-logit-regression.html
# https://willhipson.netlify.app/post/stan-random-slopes/varying_effects_stan/

N <- 3000 # Number of observations
K <- 3 # Number of response classes
P <- 3 # Number of predictors

# -- Group size for each cluster group 
# -- Let's assume 2 crossed factors
group1_size <- 15
group2_size <- 10


# -- Covariate Data matrix
X <- matrix(rnorm(N * P), N, P) 
# (N x P) Fixed Effects Model Matrix

X[,3] <- rbinom(N,1,0.4)
# --> For fun, change 3rd variable to be binary


# -- Set Fixed effect values
beta <- cbind(matrix(rnorm((K - 1) * P), P, K - 1),0)
# (P x K) Fixed slopes. Kth column is set to 0 for identifiability.

alpha <- c(rnorm(K - 1), 0) 
# Fixed Intercepts. Kth intercept is 0.

# -- Create random intercepts (z_id's)
eta_1 <- matrix(rnorm((K - 1) * group1_size), K - 1, group1_size)
# (K-1 x 15)

eta_2 <- matrix(rnorm((K - 1) * group2_size), K - 1, group2_size)
# (K-1 x 10)


# -- Initialize random intercepts for each group
# (K x J) matrix
random_intercept_1 <- matrix(0, K, group1_size)
random_intercept_2 <- matrix(0, K, group2_size)

# Simulate sd of random intercept effects
sigma_1 <- abs(rnorm(K - 1))
sigma_2 <- abs(rnorm(K - 1))

# -- Try to simulate using https://willhipson.netlify.app/post/stan-random-slopes/varying_effects_stan/
#library(devtools)
#devtools::install_github("rmcelreath/rethinking", force = TRUE)

# Here's the idea:
# B ~ MVN(mu, Sigma) is equivalent to: B = mu + L*a; 
# where L is the Cholesky factorization of Sigma, and a ~ N(0,1) 
# We'll use a non-centered parameterization (i.e. set mu in the model directly)

# -- Crossed Factor # 1
Omega_1 <- rethinking::rlkjcorr(n = 1, K = 2, eta = 4) # Simulate correlations 
Sigma_1 <- diag(sigma_1) %*% Omega_1 %*% diag(sigma_1) # Get full Covariance matrix
random_intercept_1 <- rbind(chol(Sigma_1) %*% eta_1, # Calculate L*a in above formulation
                            rep(0, times = group1_size))
# -- Crossed Factor # 2
Omega_2 <- rethinking::rlkjcorr(n = 1, K = 2, eta = 4)
Sigma_2 <- diag(sigma_2) %*% Omega_2 %*% diag(sigma_2)
random_intercept_2 <- rbind(chol(Sigma_2) %*% eta_2,
                            rep(0, times = group2_size))


# Create group labels for each observation.
group1_id <- sample(group1_size, N, replace = T)
group2_id <- sample(group2_size, N, replace = T)


##### Add a random slope #####
# -- Random Slope
# (3000 x 1) * (1 x 3)
beta_4 <- matrix(c(0.25, 0.5,0),nrow = 1, ncol = 3)
eta_slope <- matrix(rnorm((K - 1) * group2_size), K - 1, group2_size)

sigma_slope <- abs(rnorm(K - 1))
Omega_slope <- rethinking::rlkjcorr(n = 1, K = 2, eta = 4)
Sigma_slope <- diag(sigma_slope) %*% Omega_slope %*% diag(sigma_slope)
random_slope <- rbind(chol(Sigma_2) %*% eta_slope,
                            rep(0, times = group2_size))

x4 <- matrix(rnorm(N ),
             nrow = N, ncol = 1)

to_add_slope <- matrix(0, nrow = N, ncol= 3)
for(i in 1:N){
  id <- group2_id[i]
  slope_term <- beta_4 + random_slope[, id]
  
  to_add_slope[i,] <- x4[i] * slope_term
  
}


##### Put it all together and simulate Y #####

mu <- sweep(x = X %*% beta, MARGIN = 2, STATS = alpha, FUN = '+')
# -- Add alpha (the intercept) to each column

mu <- t(t(mu) + random_intercept_1[, group1_id] + random_intercept_2[, group2_id] + t(to_add_slope) )

# -- Get softmax probabilities for each category
mu_soft <- t(apply(mu, 1, softmax))

y <- sapply(1:N, function(x) rmultinom(1, size = 1, prob = mu_soft[x,]))
y <- apply(y, 2, function(x) which(as.logical(x)))


## --  For organization, save all info into a single dataframe
training_data <- data.frame(cbind(X, y, group1_id, group2_id))

# -- Fit MNL Hierarchical Model
hier_mnl <- stan_model(file = 'random_intercept.stan')

stan_dat <- list(
  N = N,
  N_1 = group1_size,
  N_2 = group2_size,
  y = training_data$y,
  id_1 = training_data$group1_id,
  id_2 = training_data$group2_id,
  x1 = training_data$V1,
  x2 = training_data$V2,
  x3 = training_data$V3,
  x4 = x4[,1],
  K = K
)

# -- Fit STAN Model
fit_hier_mnl <- sampling(hier_mnl, data = stan_dat, iter = 1000)

# ** Note: We simulated sd terms using rnorm... but in STAN we used exp(1)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Check how good were our fixed effect estimates =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Fixed effects
plot(fit_hier_mnl, plotfun="hist", pars = c('B_0', 'B_1', 'B_2', 'B_3', 'B_4'))
alpha
beta
beta_4

# -- Random Intercepts
plot(fit_hier_mnl, plotfun="hist", pars = c('v_id1'))
random_intercept_1


plot(fit_hier_mnl, plotfun="hist", pars = c('v_id2'))
random_intercept_2


plot(fit_hier_mnl, plotfun="hist", pars = c('v_id2B'))
random_slope


plot(fit_hier_mnl, plotfun="hist", pars = c('B_0', 'B_1', 'B_2', 'B_3', 'sigma_id1', 'sigma_id2' ))
#plot(fit_hier_mnl, plotfun="dens",  pars = c('B_0', 'B_1', 'B_2', 'B_3'))


# -- Summary diagnostics of MCMC
summary(fit_hier_mnl)$summary %>% View()

write.csv(summary(fit_hier_mnl)$summary,
          file = 'summary.csv',
          row.names = FALSE)

plot(fit_hier_mnl, plotfun="trace")

# Get posterior means & medians #####
names(extract(fit_hier_mnl))
extract_elements <- extract(fit_hier_mnl)

# -- Fixed Effects MCMC samples
B_0_samples <- extract_elements$B_0
B_1_samples <- extract_elements$B_1
B_2_samples <- extract_elements$B_2
B_3_samples <- extract_elements$B_3
B_4_samples <- extract_elements$B_4

# Compare Fixed Slopes
apply(B_1_samples, 2, median)
apply(B_2_samples, 2, median)
apply(B_3_samples, 2, median)
beta

apply(B_0_samples, 2, median)
alpha

# -- Iffy performance 
apply(B_4_samples, 2, median)
beta_4


### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Check how good were our Random Effects estimates =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# -- Group 1
rand_intercept_1_samples <- extract_elements$v_id1
dim(rand_intercept_1_samples)

rbind(apply(rand_intercept_1_samples[,,1], 2, median),
      apply(rand_intercept_1_samples[,,2], 2, median)
)

random_intercept_1

# -- Group 2
rand_intercept_2_samples <- extract_elements$v_id2
dim(rand_intercept_2_samples)

rbind(apply(rand_intercept_2_samples[,,1], 2, median),
      apply(rand_intercept_2_samples[,,2], 2, median)
)

random_intercept_2


# -- Random Slope
rand_slope_2_samples <- extract_elements$v_id2B
dim(rand_slope_2_samples)

rbind(apply(rand_slope_2_samples[,,1], 2, median),
      apply(rand_slope_2_samples[,,2], 2, median)
)

random_slope

