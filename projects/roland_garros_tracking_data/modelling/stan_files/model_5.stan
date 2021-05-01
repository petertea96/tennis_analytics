data{
    int N; // Sample size
    int N_1; // Size of 1st cluster variable
    int y[N]; // Response Variable
    int id_1[N]; // ID column of 1st cluster variable
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

    // Random Effects
    matrix[K-1,N_1] z_id1; // matrix of group1 standardized random effects
    vector<lower=0>[K-1] sigma_id1; // stddev of group1 random effects
    cholesky_factor_corr[K-1] L_Rho_id1; // correlation matrix of group1 random effects

    // Random Slope?
    matrix[K-1,N_1] z_id1B; 
    vector<lower=0>[K-1] sigma_id1B;
    cholesky_factor_corr[K-1] L_Rho_id1B; 


}


transformed parameters{
  
  matrix[N_1,K-1] v_id1;  // matrix of scaled group1 random effects
  v_id1 = (diag_pre_multiply(sigma_id1,L_Rho_id1) * z_id1)';


  matrix[N_1,K-1] v_id1B; 
  v_id1B = (diag_pre_multiply(sigma_id1B,L_Rho_id1B) * z_id1B)';

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

    // hyper-prior
    to_vector(z_id1) ~ normal(0,1);
    sigma_id1 ~ exponential(1);
    L_Rho_id1 ~ lkj_corr_cholesky(2);

    to_vector(z_id1B) ~ normal(0,1);
    sigma_id1B ~ exponential(1);
    L_Rho_id1B ~ lkj_corr_cholesky(2);

    
    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = (B_0[k] + v_id1[id_1[i],k]) + B_1[k] * x1[i] + B_2[k] * x2[i] + B_3[k] * x3[i] + B_4[k] * x4[i] + B_5[k] * x5[i] + B_6[k] * x6[i] + B_7[k] * x7[i] + B_8[k] * x8[i] + (B_9[k] + v_id1B[id_1[i],k]) * x9[i] + B_10[k] * x10[i] + B_11[k] * x11[i] + B_12[k] * x12[i] + B_13[k] * x13[i];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}

  // Save pointwise log-likelihood to calculate LOO-CV and WAIC for model diagnostics
  // Save a vector of length N for the log likelihood values
  // categorical_logit_lpmf generates the likelihood of each observation, conditional
  // on the model. 
generated quantities{
    matrix[K-1,K-1] Rho_id1;
    vector[N] log_lik;
    Rho_id1 = L_Rho_id1 * L_Rho_id1';

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = (B_0[k] + v_id1[id_1[i],k]) + B_1[k] * x1[i] + B_2[k] * x2[i] + B_3[k] * x3[i] + B_4[k] * x4[i] + B_5[k] * x5[i] + B_6[k] * x6[i] + B_7[k] * x7[i] + B_8[k] * x8[i] + B_9[k] * x9[i] + B_10[k] * x10[i] + B_11[k] * x11[i] + B_12[k] * x12[i] + B_13[k] * x13[i];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
