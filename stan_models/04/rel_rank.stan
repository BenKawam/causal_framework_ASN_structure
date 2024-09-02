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
  array[N] int Re_ab; // Relatedness
  vector[N] s_ab; // Sampling effort
  vector[N] Ra_a; // Rank of individual a
  vector[N] Ra_b; // Rank of individual b
}

parameters{
  // Fixed effects
  real D;
  real b_Re;
  vector[2] b_Ra;

  // Individual varying effects
  matrix[2, N_ind] z_K; // z_G and z_R (horizontal)
  real<lower = 0> s_G; // SD of G
  real<lower = 0> s_R; // SD of R
  cholesky_factor_corr[2] L_ind; // Cholesky factor of corr. matrix
  
  // Dyadic varying effects
  matrix[2, N_dyad] z_T; // z_T_ab and z_T_ba (wide format)
  real<lower = 0> s_T; // Unique SD of directed ties
  cholesky_factor_corr[2] L_dyad; // Cholesky factor of corr. matrix
}

model{
  // Priors
    // Fixed effects
    D ~ normal(-1.5, 1);
    b_Ra ~ normal(0, 0.6);
    b_Re ~ normal(0, 0.6);
    
    // Varying individual effects
    to_vector(z_K) ~ normal(0, 1);
    s_G ~ exponential(1);
    s_R ~ exponential(1);
    L_ind ~ lkj_corr_cholesky(3);
    
    // Varying dyadic effects
    to_vector(z_T) ~ normal(0, 1);
    s_T ~ exponential(1);
    L_dyad ~ lkj_corr_cholesky(3);
    
  // Param
    // Individual varying effects 
      matrix[N_ind, 2] K; // K matrix (i.e. G and R): vertical format
      K = (diag_pre_multiply([s_G, s_R], L_ind) * z_K)';
      
      // Rename individual random effects
      vector[N_ind] G = K[, 1]; // individual giving random effect
      vector[N_ind] R = K[, 2]; // individual receiving random effect
      
    // Dyadic varying effects 
      matrix[N_dyad, 2] T; // T_ab and T_ba (vertical format)
      T = (diag_pre_multiply(rep_vector(s_T, 2), L_dyad) * z_T)';
      
      // Rename dyadic random effects
      vector[N_dyad] t_ab = T[, 1];  
      vector[N_dyad] t_ba = T[, 2]; 
      
      // Compute directed ties
      vector[N_dyad] T_ab;
      vector[N_dyad] T_ba;
      for (i in 1:N){
            T_ab[dyad[i]] = log(s_ab[i]) +
                            t_ab[dyad[i]] + 
                            b_Re * Re_ab[i] + 
                            ((Ra_a[i] < Ra_b[i]) ? 
                            b_Ra[1] * (Ra_b[i] - Ra_a[i]) :
                            b_Ra[2] * (Ra_b[i] - Ra_a[i]));
            T_ba[dyad[i]] = log(s_ab[i]) +
                            t_ba[dyad[i]] + 
                            b_Re * Re_ab[i] + 
                            ((Ra_b[i] < Ra_a[i]) ? 
                            b_Ra[1] * (Ra_a[i] - Ra_b[i]) :
                            b_Ra[2] * (Ra_a[i] - Ra_b[i]));
      }
          
    // Expected dyadic rates
    vector[N] m_ab;
    vector[N] m_ba;
    
    // Two parallel linear models
    m_ab = D + G[ind_a] + R[ind_b] + T_ab[dyad];
    m_ba = D + G[ind_b] + R[ind_a] + T_ba[dyad];
  
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

