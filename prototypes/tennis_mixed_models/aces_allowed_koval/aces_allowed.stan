data {
  int<lower=0> N; // Observations
  int<lower=1> P; // Total players
  int<lower=0> aces[N];
  int<lower=0> serves[N];
  int<lower=1,upper=P> server_index[N];
  int<lower=1,upper=P> receiver_index[N];
}

parameters {
  real beta_intercept;
  vector[P] beta_server_z;
  vector[P] beta_receiver_z;
  real<lower=0> sigma_server;
  real<lower=0> sigma_receiver;
}

transformed parameters {
  vector[P] beta_server;
  vector[P] beta_receiver;	

  beta_server = beta_server_z * sigma_server;
  beta_receiver = beta_receiver_z * sigma_receiver;
}

model {

  beta_intercept ~ std_normal();

  beta_server_z ~ std_normal();
  beta_receiver_z ~ std_normal();

  sigma_server ~ student_t(1, 0, 1);
  sigma_receiver ~ student_t(1, 0, 1);

  for (n in 1:N)
 	aces[n] ~ binomial_logit(serves[n], (beta_intercept + beta_server[server_index[n]] + beta_receiver[receiver_index[n]]));
}