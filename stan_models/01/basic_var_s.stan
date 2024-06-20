data{
  // Indices
  int<lower = 1> N; // Nb of observations
  int<lower = 1> N_ind; // Nb of individuals
  int<lower = 1> N_dyad; // Nb of non-directed dyads
  
  // Observables
  array[N] int ind_a; // Individual a
  array[N] int ind_b; // Individual b
  array[N] int dyad; // non-directed dyad
  array[N] int y_ab; // Interactions from a to b
  array[N] int y_ba; // Interactions from b to a
  vector<lower = 0>[N] s_ab; // Sampling effort
}

parameters{
  // Fixed effects
  real D;

  // Individual varying effects
  matrix[2, N_ind] z_K; // z_G_j and z_R_j (horizontal)
  real<lower = 0> s_G; // Variance in G_j
  real<lower = 0> s_R; // Variance in R_j
  cholesky_factor_corr[2] L_ind; // Cholesky factor of corr. matrix
  
  // Dyadic varying effects
  matrix[2, N_dyad] z_T; // z_T_ab and z_T_ba (wide format)
  real<lower = 0> s_T; // Unique variance for directed ties
  cholesky_factor_corr[2] L_dyad; // Cholesky factor of corr. matrix
}

model{
  // Param
    // Individual varying effects 
    matrix[N_ind, 2] K; // K matrix (i.e. Gi and Ri): vertical format
    K = (diag_pre_multiply([s_G, s_R], L_ind) * z_K)';
    
    // Rename individual random effects
    vector[N_ind] G = K[, 1]; // individual giving random effect
    vector[N_ind] R = K[, 2]; // individual receiving random effect
    
  // Dyadic varying effects 
    matrix[N_dyad, 2] T; // T_ab and T_ba (vertical format)
    T = (diag_pre_multiply(rep_vector(s_T, 2), L_dyad) * z_T)';
    
    // Rename dyadic random effects
    vector[N_dyad] T_ab = T[, 1]; // T_ab
    vector[N_dyad] T_ba = T[, 2]; // T_ba
  
  
  // Expected dyadic rates
  vector[N] m_ab;
  vector[N] m_ba;
  
  // Two parallel linear models
  m_ab = exp(D + G[ind_a] + R[ind_b] + T_ab[dyad]);
  m_ba = exp(D + G[ind_b] + R[ind_a] + T_ba[dyad]);
  
  // Priors
    // Fixed effects
    D ~ normal(-1, 1);
    
    // Varying individual effects
    to_vector(z_K) ~ normal(0, 1);
    s_G ~ exponential(1);
    s_R ~ exponential(1);
    L_ind ~ lkj_corr_cholesky(2);
    
    // Varying dyadic effects
    to_vector(z_T) ~ normal(0, 1);
    s_T ~ exponential(1);
    L_dyad ~ lkj_corr_cholesky(2);
  
  // Likelihood
  target += poisson_lpmf(y_ab | m_ab .* s_ab);
  target += poisson_lpmf(y_ba | m_ba .* s_ab);
}

generated quantities{
  // Corr. matrix from cholesky factors
    // Varying individual effects
    matrix[2, 2] c_ind_M;
    c_ind_M = multiply_lower_tri_self_transpose(L_ind);
    real c_GR = c_ind_M[1, 2];
  
    // Varying dyadic effects
    matrix[2, 2] c_dyad_M;
    c_dyad_M = multiply_lower_tri_self_transpose(L_dyad);
    real c_TT = c_dyad_M[1, 2];
}
