data{
    int N; // Sample size
    int y[N]; // Response Variable
    real x1[N]; // 1st Fixed effect variable
    real x2[N]; // 2nd Fixed effect variable
    real x3[N]; // 3rd Fixed effect variable
    real x4[N]; // 
    real x5[N]; // 
    real x6[N]; // 
    real x7[N]; // 
    real x8[N]; // 
    real x9[N]; // 
    real x10[N]; // 
    real x11[N]; // 
    real x12[N]; // 
    real x13[N]; // 
    int K; // No. of response group categories

    // # Set possibility to predict with STAN
    int<lower=1> N_test; // # Number of obs. in predicted set
  
    real x1_test[N_test]; // Specify Predictor variables individually...
    real x2_test[N_test];
    real x3_test[N_test];
    real x4_test[N_test]; // Specify Predictor variables individually...
    real x5_test[N_test];
    real x6_test[N_test];
    real x7_test[N_test]; // Specify Predictor variables individually...
    real x8_test[N_test];
    real x9_test[N_test];
    real x10_test[N_test]; // Specify Predictor variables individually...
    real x11_test[N_test];
    real x12_test[N_test];
    real x13_test[N_test];
}
parameters{
  // Fixed Effects
    real B_0[K-1];  // intercepts for each response level
    real B_1[K-1];	// fixed effect for variable 1
    real B_2[K-1];	// fixed effect for variable 2
    real B_3[K-1];	// fixed effect for variable 3
    real B_4[K-1];  // 
    real B_5[K-1];  // 
    real B_6[K-1];  // 
    real B_7[K-1];  // 
    real B_8[K-1];  // 
    real B_9[K-1];  // 
    real B_10[K-1];  // 
    real B_11[K-1];  // 
    real B_12[K-1];  // 
    real B_13[K-1];  // 

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
    B_7 ~ normal(0,1);
    B_8 ~ normal(0,1);
    B_9 ~ normal(0,1);
    B_10 ~ normal(0,1);
    B_11 ~ normal(0,1);
    B_12 ~ normal(0,1);
    B_13 ~ normal(0,1);

    
    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + B_1[k] * x1[i] + B_2[k] * x2[i] + B_3[k] * x3[i] + B_4[k] * x4[i] + B_5[k] * x5[i] + B_6[k] * x6[i] + B_7[k] * x7[i] + B_8[k] * x8[i] + B_9[k] * x9[i] + B_10[k] * x10[i] + B_11[k] * x11[i] + B_12[k] * x12[i] + B_13[k] * x13[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}

generated quantities{
  // predictions
  vector[N_test] y_new; 
  vector[K] p_pred_vals[N_test];
  
  for (i in 1:N_test){
    vector[K] p_pred;
    for ( response_level in 1:(K-1) ) {
      p_pred[response_level] = B_0[response_level] + B_1[response_level] * x1_test[i] + B_2[response_level] * x2_test[i] + B_3[response_level] * x3_test[i] + B_4[response_level] * x4_test[i] + B_5[response_level] * x5_test[i] + B_6[response_level] * x6_test[i] + B_7[response_level] * x7_test[i] + B_8[response_level] * x8_test[i] + B_9[response_level] * x9_test[i] + B_10[response_level] * x10_test[i] + B_11[response_level] * x11_test[i] + B_12[response_level] * x12_test[i] + B_13[response_level] * x13_test[i];
      p_pred[K] = 0;
    }

    y_new[i] = categorical_logit_rng(p_pred);  // categorical_logit_rng gives predicted levels
    p_pred_vals[i] = softmax(p_pred);
    //p_pred_vals[i] = p_pred;
  }
}

