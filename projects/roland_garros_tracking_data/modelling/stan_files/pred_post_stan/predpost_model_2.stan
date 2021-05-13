// PLAYER-VARYING INTERCEPT MODEL
data{
    int N; // Sample size
    int N_1; // Size of 1st cluster variable
    int y[N]; // Response Variable
    int id_1[N]; // ID column of 1st cluster variable
    int K; // No. of response group categories
}
parameters{
  // Fixed Effects
    real B_0[K-1];  // intercepts for each response level
    
  // Random Effects
    matrix[K-1,N_1] z_id1; // matrix of group1 standardized random effects
    vector<lower=0>[K-1] sigma_id1; // stddev of group1 random effects
    cholesky_factor_corr[K-1] L_Rho_id1; // correlation matrix of group1 random effects


}
transformed parameters{
  
  matrix[N_1,K-1] v_id1;  // matrix of scaled group1 random effects
  
  v_id1 = (diag_pre_multiply(sigma_id1,L_Rho_id1) * z_id1)';

}

model{
    
    // priors
    B_0 ~ normal(0,3);

    // hyper-prior
    to_vector(z_id1) ~ normal(0,3);
    sigma_id1 ~ cauchy(0, 2.5); // used to be exponential(1)
    L_Rho_id1 ~ lkj_corr_cholesky(2);
    

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + v_id1[id_1[i],k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}

generated quantities{

    matrix[K-1,K-1] Rho_id1;
    int<lower=1,upper=K> y_rep[N];
    Rho_id1 = L_Rho_id1 * L_Rho_id1';


    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + v_id1[id_1[i],k];
        p[K] = 0;
        y_rep[i] = categorical_logit_rng( p );
    }
}
