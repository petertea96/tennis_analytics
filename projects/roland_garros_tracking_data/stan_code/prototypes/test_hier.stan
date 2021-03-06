data{
    int N; // Sample size
    int N_1; // Size of 1st cluster variable
    int N_2; // Size of 2nd cluster variable
    int y[N]; // Response Variable
    int id_1[N]; // ID column of 1st cluster variable
    int id_2[N]; //ID column of 1st cluster variable
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
    matrix[K-1,N_1] z_id1; // matrix of group1 standardized random effects
    vector<lower=0>[K-1] sigma_id1; // stddev of group1 random effects
    cholesky_factor_corr[K-1] L_Rho_id1; // correlation matrix of group1 random effects
    
    matrix[K-1,N_2] z_id2; // matrix of group2 standardized random effects
    vector<lower=0>[K-1] sigma_id2; // stddev of group2 random effects
    cholesky_factor_corr[K-1] L_Rho_id2; // correlation matrix of group2 random effects
}
transformed parameters{
  
  matrix[N_1,K-1] v_id1;  // matrix of scaled group1 random effects
  matrix[N_2,K-1] v_id2;  // matrix of scaled group2 random effects
  
  v_id1 = (diag_pre_multiply(sigma_id1,L_Rho_id1) * z_id1)';
  v_id2 = (diag_pre_multiply(sigma_id2,L_Rho_id2) * z_id2)';
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
    
    to_vector(z_id2) ~ normal(0,1);
    sigma_id2 ~ exponential(1);
    L_Rho_id2 ~ lkj_corr_cholesky(2);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + B_1[k] * x1[i] + B_2[k] * x2[i] + B_3[k] * x3[i] + v_id1[id_1[i],k] + v_id2[id_2[i],k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    matrix[K-1,K-1] Rho_id1;
    matrix[K-1,K-1] Rho_id2;
    
    vector[N] log_lik;
    
    Rho_id1 = L_Rho_id1 * L_Rho_id1';
    Rho_id2 = L_Rho_id2 * L_Rho_id2';
    


    
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + B_1[k] * x1[i] + B_2[k] * x2[i] + B_3[k] * x3[i] + v_id1[id_1[i],k] +  v_id2[id_2[i],k];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
