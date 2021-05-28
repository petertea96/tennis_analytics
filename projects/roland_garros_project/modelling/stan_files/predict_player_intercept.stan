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
    B_0 ~ normal(0,5);

    // hyper-prior
    to_vector(z_id1) ~ normal(0,5);
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


  // Sample from predictive posterior distribution
generated quantities{
    vector[K] p;
    matrix[K-1,K-1] Rho_id1;
    //real player_probs[N_1, 3];
    vector[3] player_probs[N_1];
    vector[N_1] player_preds;
    //int y_test;
    //vector[K] y_test_prob;

    Rho_id1 = L_Rho_id1 * L_Rho_id1';


    for (player_id in 1:N_1){

        for ( resp_lev in 1:(K-1) ) 
            p[resp_lev] = (B_0[resp_lev] + v_id1[player_id,resp_lev]);
        p[K] = 0;

        //y_test_prob = softmax(p);
        //y_test = categorical_logit_rng(p);
        player_probs[player_id] = softmax(p);
        player_preds[player_id] = categorical_logit_rng(p);
    }

}
