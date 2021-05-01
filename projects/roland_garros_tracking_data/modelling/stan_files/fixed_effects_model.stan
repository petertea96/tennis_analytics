data{
    int N; // Sample size
    int y[N]; // Response Variable
    real x1[N]; // 1st Fixed effect variable
    real x2[N]; // 2nd Fixed effect variable
    real x3[N]; // 3rd Fixed effect variable
    real x4[N]; // 3rd Fixed effect variable
    real x5[N]; // 3rd Fixed effect variable
    real x6[N]; // 3rd Fixed effect variable
    int K; // No. of response group categories
}
parameters{
  // Fixed Effects
    real B_0[K-1];  // intercepts for each response level
    real B_1[K-1];	// fixed effect for variable 1
    real B_2[K-1];	// fixed effect for variable 2
    real B_3[K-1];	// fixed effect for variable 3
    real B_4[K-1];  // fixed effect for variable 1
    real B_5[K-1];  // fixed effect for variable 2
    real B_6[K-1];  // fixed effect for variable 3

}

model{
    
    // priors
    B_0 ~ normal(0,1);
    B_1 ~ normal(0,1);
    B_2 ~ normal(0,1);
    B_3 ~ normal(0,1);
    B_4 ~ normal(0,1);
    B_5 ~ normal(0,1);
    B_6 ~ normal(0,1);

    
    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + B_1[k] * x1[i] + B_2[k] * x2[i] + B_3[k] * x3[i] + B_4[k] * x4[i] + B_5[k] * x5[i] + B_6[k] * x6[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}


  // Save pointwise log-likelihood to calculate LOO-CV and WAIC for model diagnostics
  // Save a vector of length N for the log likelihood values
  // categorical_logit_lpmf generates the likelihood of each observation, conditional
  // on the model. 
generated quantities{
    vector[N] log_lik;
    
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + B_1[k] * x1[i] + B_2[k] * x2[i] + B_3[k] * x3[i] + B_4[k] * x4[i] + B_5[k] * x5[i] + B_6[k] * x6[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
