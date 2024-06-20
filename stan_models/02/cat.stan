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
  array[N] int x_a; // Value of x for actor
  array[N] int x_b; // Value of x for receiver
}

parameters{
  // Fixed effects
  real D;
  vector[2] L;
  vector[2] M;

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

transformed parameters{
  // Individual varying effects 
    matrix[N_ind, 2] K; // K matrix (i.e. Gi and Ri): vertical format
    K = (diag_pre_multiply([s_G, s_R], L_ind) * z_K)';
    
  // Rename individual random effects
  vector[N_ind] g = K[, 1]; // individual giving random effect
  vector[N_ind] r = K[, 2]; // individual receiving random effect
  
  // Compute gen. giving and receiving
  vector[N_ind] G;
  vector[N_ind] R;
  for (j in 1:N){
    G[ind_a[j]] = L[x_a[j]] + g[ind_a[j]]; // Generalised giving (a)
    G[ind_b[j]] = L[x_b[j]] + g[ind_b[j]]; // Generalised giving (b)
    
    R[ind_a[j]] = M[x_a[j]] + r[ind_a[j]]; // Generalised receiving (a)
    R[ind_b[j]] = M[x_b[j]] + r[ind_b[j]]; // Generalised receiving (b)
  }
    
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
  m_ab = D + G[ind_a] + R[ind_b] + T_ab[dyad];
  m_ba = D + G[ind_b] + R[ind_a] + T_ba[dyad];
}

model{
  // Priors
    // Fixed effects
    D ~ normal(0, 1);
    L ~ normal(0, 1);
    M ~ normal(0, 1);
    
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
  y_ab ~ poisson_log(m_ab);
  y_ba ~ poisson_log(m_ba);
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

