ivp.i <- function(target_df, # target valies
                  sample, # Posterior samples
                  xmin = -2.5, # lower limits x
                  xmax = 2.5, # upper limits x
                  n_var = 10) # How many var-effects to sample from
                  {
  
  ## 1. Giving
  # 1.1 Target values
  target_df_1 <- target_df %>%
    select(ind_a, gamma_a) %>%
    rename(ind = ind_a, gamma = gamma_a) %>%
    bind_rows(target_df %>%
                select(ind_b, gamma_b) %>%
                rename(ind = ind_b, gamma = gamma_b)) %>%
    distinct() %>%
    filter(ind < (n_var + 1)) %>%
    rename(param = ind, value = gamma) %>%
    mutate(param = paste0("G[", param, "]"))
  
  # 1.2 Posterior draws
  p1 <- sample %>%
    filter(str_sub(param, start = 1, end = 1) == "G") %>%
    
    # Plot
    ggplot(aes(x = value, y = param)) +
    # Theme and layout
    theme_bw() +
    labs(x = "", y = "", color = 'Percentile Interval') +
    theme(
      axis.text.y = element_text(size = 6.5),
      axis.text.x = element_text(vjust = -1.5),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "#ccccc0",
                                        size = 0.3,
                                        linetype = "dotted"),
      strip.background = element_rect(fill = "white", color = "white")
    ) +
    
    # 1.3 Posterior density
    stat_slab(
      slab_color = NA,
      fill = "#a19987",
      scale = 0.6,
      normalize = "groups",
      slab_alpha = 0.3
    ) +
    
    # Contour of slab
    stat_slab(
      slab_color = "#363533",
      fill = NA,
      slab_linewidth = 0.2,
      scale = 0.6,
      normalize = "groups",
      slab_alpha = 0.5
    ) +
    
    # Target values
    geom_point(data = target_df_1,
               fill = "#ab0f24",
               pch = 21,
               colour = "white",
               size = 1.8) +
    
    xlim(c(xmin, xmax))
  
  
  ## 2. Receiving
  # 2.1 Target values
  target_df_2 <- target_df %>%
    select(ind_a, rho_a) %>%
    rename(ind = ind_a, rho = rho_a) %>%
    bind_rows(target_df %>%
                select(ind_b, rho_b) %>%
                rename(ind = ind_b, rho = rho_b)) %>%
    distinct() %>%
    filter(ind < (n_var + 1)) %>%
    rename(param = ind, value = rho) %>%
    mutate(param = paste0("R[", param, "]"))
  
  # 2.2 Posterior draws
  p2 <- sample %>%
    filter(str_sub(param, start = 1, end = 1) == "R") %>%
    
    # 2.3 Plot
    ggplot(aes(x = value, y = param)) +
    # Theme and layout
    theme_bw() +
    labs(x = "", y = "", color = 'Percentile Interval') +
    theme(
      axis.text.y = element_text(size = 6.5),
      axis.text.x = element_text(vjust = -1.5),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "#ccccc0",
                                        size = 0.3,
                                        linetype = "dotted"),
      strip.background = element_rect(fill = "white", color = "white")
    ) +
    scale_y_discrete(position = "right") +
    
    # Posterior density
    stat_slab(
      slab_color = NA,
      fill = "#a19987",
      scale = 0.6,
      normalize = "groups",
      slab_alpha = 0.3
    ) +
    
    # Contour of slab
    stat_slab(
      slab_color = "#363533",
      fill = NA,
      slab_linewidth = 0.2,
      scale = 0.6,
      normalize = "groups",
      slab_alpha = 0.5
    ) +
    
    # Target values
    geom_point(data = target_df_2,
               fill = "#ab0f24",
               pch = 21,
               colour = "white",
               size = 1.8) +
    
    xlim(c(xmin, xmax))
  
  (p1 + p2) %>%
    return()
}

ivp.i %>% saveRDS("./functions/ivp.i.rds")
