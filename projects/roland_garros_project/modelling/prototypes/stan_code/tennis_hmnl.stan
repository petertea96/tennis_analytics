data{
    int N; // Sample size
    int tot_match_num; // Size of 1st cluster variable
    int y[N]; // Response Variable
    int match_id[N]; // ID column of 1st cluster variable
    real x1[N]; // 1st Fixed effect variable
    real x2[N]; // 2nd Fixed effect variable
    real x3[N]; // 3rd Fixed effect variable
    int K; // No. of response group categories
}
parameters{
  // Fixed Effects
    real B_0[K-1];  // intercepts for each response level
    real B_1[K-1];	// fixed effect for variable 1
    real B_2[K-1];	// fixed effect for variable 2
    real B_3[K-1];	// fixed effect for variable 3
    
  // Random Effects
    matrix[K-1,tot_match_num] z_id1; // matrix of group1 standardized random effects
    vector<lower=0>[K-1] sigma_id1; // stddev of group1 random effects
    cholesky_factor_corr[K-1] L_Rho_id1; // correlation matrix of group1 random effects
    

}
transformed parameters{
  
  matrix[tot_match_num,K-1] v_id1;  // matrix of scaled group1 random effects
  
  v_id1 = (diag_pre_multiply(sigma_id1,L_Rho_id1) * z_id1)';

}

model{
    
    // priors
    B_0 ~ normal(0,1);
    B_1 ~ normal(0,1);
    B_2 ~ normal(0,1);
    B_3 ~ normal(0,1);

    // hyper-prior
    to_vector(z_id1) ~ normal(0,1);
    sigma_id1 ~ exponential(1);
    L_Rho_id1 ~ lkj_corr_cholesky(2);
    
    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + B_1[k] * x1[i] + B_2[k] * x2[i] + B_3[k] * x3[i] + v_id1[match_id[i],k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}

generated quantities{
  // Save Training error
  vector[N] pred_y_train;
  vector[K] p_pred_vals[N];
  

  
  for (i in 1:N){
    vector[K] p_pred;
    for ( response_level in 1:(K-1) ) {
      p_pred[response_level] = B_0[response_level] + B_1[response_level] * x1[i] + B_2[response_level] * x2[i] + B_3[response_level] * x3[i] + v_id1[match_id[i],response_level];
      p_pred[K] = 0;
    }

    pred_y_train[i] = categorical_logit_rng(p_pred);  // categorical_logit_rng gives predicted levels
    p_pred_vals[i] = softmax(p_pred);
    //p_pred_vals[i] = p_pred;
  }

}
