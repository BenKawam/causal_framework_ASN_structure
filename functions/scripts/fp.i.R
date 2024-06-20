fp.i <- function(samples_df, # Posterior samples
                 target_df, # Target values
                 prior = FALSE, # Are we plotting prior or post dist?
                 diff_t_col = FALSE) # Target values are red or red+grey?
                 {
  
  plot <- ggplot(samples_df, aes(x = value, y = param)) +
    
    # Theme and layout
    theme_bw() +
    labs(x = "", y = "", color = 'Percentile Interval') +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 6.5),
      axis.text.x = element_text(vjust = -1.5),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "#c2c2b0",
                                        size = 0.3,
                                        linetype = "dotted"),
      strip.background = element_rect(fill = "white", color = "white")
    ) +
    geom_vline(xintercept = 0, color = "#b8b5ab", linewidth = 0.3, linetype = "dotted")

    
    # Posterior density
  plot <- plot + stat_slab(
      aes(group = sim_n),
      slab_color = NA,
      fill = ifelse(prior == FALSE, "#a19987", "#e8ddd3"),
      scale = 0.5,
      normalize = "groups",
      slab_alpha = ifelse(prior == FALSE, 0.05, 1),
      density = "bounded",
      trim = FALSE
    ) +
    
    # Contour of slab
    stat_slab(
      aes(group = sim_n),
      slab_color = "#363533",
      fill = NA,
      slab_linewidth = ifelse(prior == FALSE, 0.1, 0.3),
      scale = 0.5,
      normalize = "groups",
      slab_alpha = ifelse(prior == FALSE, 0.15, 0.5),
      density = "bounded",
      trim = FALSE
    )
  
  if (diff_t_col == FALSE){
    plot <- plot +
      # Target values
      geom_point(data = target_df,
                 pch = 21,
                 colour = "white",
                 fill = "#ab0f24",
                 size = 1.8) +
      geom_text(data = target_df,
                aes(label = round(value, digits = 2)), 
                nudge_y = - 0.21, size = 2.6, col = "#ab0f24")
  }
  
  if (diff_t_col == TRUE){
    plot <- plot +
      # Target values
      geom_point(aes(fill = as.factor(model),
                     alpha = as.factor(model)),
                 pch = 21,
                 colour = "white",
                 data = target_df,
                 size = 1.8) +
      geom_text(aes(col = as.factor(model), 
                    alpha = as.factor(model),
                    label = round(value, digits = 2)),
                data = target_df,
                nudge_y = - 0.21, size = 2.6) +
      scale_color_manual(values = c("#ab0f24", "#7a6f71"))  +
      scale_fill_manual(values = c("#ab0f24", "#7a6f71"))  +
      scale_alpha_manual(values = c(1, 0.8))
  }
    
    
  
  return(plot)
}

fp.i %>% saveRDS("./functions/fp.i.rds")

