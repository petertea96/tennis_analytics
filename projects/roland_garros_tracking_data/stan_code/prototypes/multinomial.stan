// Another way to fit a Multinomial Logit Regression Model in STAN
// This method uses a K-1 parameterization
data {
  int<lower=2> K; // # Number of Response Categories
  int<lower=1> N; // # Sample size
  int<lower=1,upper=K> y[N]; // # Response Variable
  real x1[N]; // Specify Predictor variables individually...
  real x2[N];
  real x3[N];
  
  // # Set possibility to predict with STAN
  int<lower=1> N_new; // # Number of obs. in predicted set
  
  real x1_test[N_new]; // Specify Predictor variables individually...
  real x2_test[N_new];
  real x3_test[N_new];

}


parameters {
  real B_0[K-1]; // intercepts for each behavior
  real B_1[K-1]; // fixed effect for x1
  real B_2[K-1];	// fixed effect for x2
  real B_3[K-1];	// fixed effect for x3
}



model {
  // priors
  B_0 ~ normal(0,1);
  B_1 ~ normal(0,1);
  B_2 ~ normal(0,1);
  B_3 ~ normal(0,1);
  
  // likelihood
  for ( i in 1:N ) {
      vector[K] p;
      for ( response_level in 1:(K-1) ) 
          p[response_level] = B_0[response_level] + B_1[response_level] * x1[i] + B_2[response_level] * x2[i] + B_3[response_level] * x3[i];
      p[K] = 0;
      y[i] ~ categorical_logit( p );
  }

}

generated quantities{

  // predictions
  vector[N_new] y_new; 
  
  vector[K] p_pred_vals[N_new];
  

  
  for (i in 1:N_new){
    vector[K] p_pred;
    for ( response_level in 1:(K-1) ) {
      p_pred[response_level] = B_0[response_level] + B_1[response_level] * x1_test[i] + B_2[response_level] * x2_test[i] + B_3[response_level] * x3_test[i];
      p_pred[K] = 0;
    }

    y_new[i] = categorical_logit_rng(p_pred);  // categorical_logit_rng gives predicted levels
    p_pred_vals[i] = softmax(p_pred);
    //p_pred_vals[i] = p_pred;
  }
}


