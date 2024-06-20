r.srm <- function(data = d, # data
                  stats_m, # stan model
                  param = "fixed", # parameter we want posterior samples for
                  param_2 = NA, # vector of param to sample if not default params
                  n_iter = 2000, # MCMC iterations
                  n_chains = 4,
                  n_cores = 4,
                  n_var = 20) # Nb of varying effects to sample
                  {
  
  # Which set of parameters are we interested in?
  if (param == "fixed"){
    if (is.logical(param_2) == TRUE){
      p <-  c("D", "s_G", "s_R", "s_T", "c_GR", "c_TT", "b_G", "b_R", "b_T")
    }
    
    if (is.logical(param_2) == FALSE){
      p <- param_2
    }
    n_loops <- length(data)
  }
  
  if (param == "v_ind"){
    p <- c(paste0("G[", 1:n_var, "]"), paste0("R[", 1:n_var, "]"))
    n_loops <- 1
  }
  
  if (param == "v_dyad"){
    p <- c(paste0("T_ab[", 1:n_var, "]"), paste0("T_ba[", 1:n_var, "]"))
    n_loops <- 1
  }
  
  if (param == "m"){
    p <- c(paste0("m_ab[", 1:n_var, "]"), paste0("m_ba[", 1:n_var, "]"))
    n_loops <- 1
  }
  
  m <- list()
  s_na <- list()
  
  # For each SCM
  for (i in 1:n_loops) {
    
    # Run SRM
    m[[i]] <- stats_m$sample(
      data = data[[i]]$d,
      iter_warmup = n_iter/2,
      iter_sampling = n_iter/2,
      chains = n_chains,
      parallel_chains = n_cores
    )
    
    # Set of parameters to extract
    colnames <- m[[i]] %>% tidy_draws() %>% colnames()
    set_param <- intersect(p, colnames)
    
    # Reformat samples to vertical
    s_na[[i]] <- m[[i]] %>% tidy_draws() %>% select(all_of(set_param)) %>%
      gather(param, value) %>%
      mutate(sim_n = i) %>%
      tibble()
  }
  
  # Output is a vertical tibble
  s_na %>% bind_rows() %>%
    return()
}


r.srm %>% saveRDS("./functions/r.srm.rds")
