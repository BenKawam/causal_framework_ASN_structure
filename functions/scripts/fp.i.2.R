fp.i.2 <- function(t, # Target values
                   s, # Posterior samples
                   split = TRUE, # Are SD, cor coef, and unbounded param on different panels
                   sd_xlim = c(-0.2, 1.8), # limits for SD param
                   oth_xlim = c(-2, 2), # Limits for unbounded param
                   heights = c(3.5, 3.5, 2.7), # Heightes of the three panels
                   prior = FALSE) # Are we plotting prior distribution?
                   {
  target_df <- t
  
  # Bind posterior draws from adjusted and unadjusted models
  samples_df <- bind_rows(s[[1]] %>% mutate(model = 1),
                          s[[2]] %>% mutate(model = 2))
  
  # One panel per stat model if we don't split by param type
  if (split == FALSE){
  plot <- fp.i(samples_df = samples_df,
       target_df = target_df) +
    facet_grid(~ factor(model, levels = c(1, 2))) +
    theme(strip.text = element_blank())
  }
  
  # Three different datasets if we do split by param type 
  if (split == TRUE){
    # Posterior samples for unobounded param 
    s_oth <- samples_df %>%
      filter(!param %in% c("s_G", "s_R", "s_T", "c_GR", "c_TT"))
    # Target values for unobounded param 
    t_oth <- target_df %>%
      filter(!param %in% c("s_G", "s_R", "s_T", "c_GR", "c_TT"))
    
    s_sd <- samples_df %>%
      filter(param %in% c("s_G", "s_R", "s_T"))
    t_sd <- target_df %>%
      filter(param %in% c("s_G", "s_R", "s_T"))
    
    s_cor <- samples_df %>%
      filter(param %in% c("c_GR", "c_TT"))
    t_cor <- target_df %>%
      filter(param %in% c("c_GR", "c_TT"))
    
    # Unbounded param
    p1 <- fp.i(samples_df = s_oth,
               target_df = t_oth,
               diff_t_col = TRUE) +
      facet_grid(~ factor(model, levels = c(1, 2))) +
      theme(strip.text = element_blank()) +
      xlim(oth_xlim)
    
    # SDs
    p2 <- fp.i(samples_df = s_sd,
               target_df = t_sd,
               diff_t_col = TRUE) +
      facet_grid(~ factor(model, levels = c(1, 2))) +
      theme(strip.text = element_blank()) +
      annotate(xmin = -0.2, xmax = 0, 
               ymin = -Inf, ymax = Inf, 
               geom = "rect", fill = "#f5f2ed") + 
      scale_x_continuous(limits = sd_xlim, expand = c(0, 0)) +
      geom_vline(xintercept = 0, color = "#b8b5ab", linewidth = 0.3, linetype = "dotted") +
      scale_y_discrete(limits = c("s_G", "s_R", "s_T") %>% rev())
    
    # Corr. coef
    p3 <- fp.i(samples_df = s_cor,
               target_df = t_cor,
               diff_t_col = TRUE) +
      facet_grid(~ factor(model, levels = c(1, 2))) +
      theme(strip.text = element_blank()) +
      annotate(xmin = -1.2, xmax = -1, 
               ymin = -Inf, ymax = Inf, 
               geom = "rect", fill = "#f5f2ed") + 
      annotate(xmin = 1, xmax = 1.2, 
               ymin = -Inf, ymax = Inf, 
               geom = "rect", fill = "#f5f2ed") +
      scale_x_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) +
      geom_vline(xintercept = -1, color = "#b8b5ab", linewidth = 0.3, linetype = "dotted") +
      geom_vline(xintercept = 1, color = "#b8b5ab", linewidth = 0.3, linetype = "dotted") +
      scale_y_discrete(limits = c("c_GR", "c_TT") %>% rev())
    
    # Composite fig
    plot <- (p1 / p2 / p3) + 
      plot_layout(heights = heights)
  }
    
    # Output of the function is the ggplot
    plot %>% return()
}

fp.i.2 %>% saveRDS("./functions/fp.i.2.rds")
