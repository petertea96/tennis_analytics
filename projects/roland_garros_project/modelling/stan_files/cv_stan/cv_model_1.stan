// COMMON INTERCEPT MODEL
data{
    int N; // Sample size
    int y[N]; // Response Variable
    int K; // No. of response group categories
  
}
parameters{
  // Fixed Effects
    real B_0[K-1];  // intercepts for each response level
}

model{
    
    // priors
    B_0 ~ normal(0,10);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}

