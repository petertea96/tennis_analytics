data {
  int<lower=1> D; // Number of predictors + intercept 
  int<lower=0> N; // Sample size
  int<lower=1> L; // Number of players
  int<lower=0,upper=1> y[N];
  int<lower=1,upper=L> player_id[N];
  row_vector[D] x[N];
}
parameters {
  real mu[D];
  real<lower=0> sigma[D];
  vector[D] beta[L];
}
model {

  mu ~ normal(0, 5);
  for (l in 1:L)
    beta[l] ~ normal(mu, sigma);
  
  for (n in 1:N)
    y[n] ~ bernoulli_logit(x[n] * beta[player_id[n]]);
}
