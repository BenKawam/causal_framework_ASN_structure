fp <- function(post_draws, # Posterior draws
               prior = FALSE, # Do I plot prior or posterior distribution?
               prior_data, # prior samples if prior == T
               lims = c(-0.12, 1.4), # Limits for parameters bounded at 0
               limo = c(-6, 4), # Limits for non-bounded parameters ("others")
               vec_param = matrix()) # Fixed effects to sample from
               {  
  
  # Reformatting
  post_draws <- post_draws %>%
    select(any_of(
      c(
        "D",
        "d[1]",
        "d[2]",
        "d[3]",
        "s_G",
        "s_R",
        "s_T",
        "c_GR",
        "c_TT",
        "b_T",
        "H[1]",
        "H[2]",
        "H_cont",
        "b_Re",
        "b_Ra",
        "b_Ra[1]",
        "b_Ra[2]",
        "b_Ra[3,1]",
        "b_Ra[1,2]",
        "b_Ra[2,2]",
        "b_Ra[3,2]",
        "b_Ra[1]",
        "b_Ra[2]"
      )
    )) %>%
    tibble() %>%
    gather(param, value, 1:ncol(.)) %>%
    # Are the param bounded at 0 (cat 1); at 0 and 1 (cat 2); not bounded (cat 3)
    mutate(cat = ifelse(param == "s_G" | param == "s_R" | param == "s_T", 1,
                 ifelse(param == "c_GR" | param == "c_TT", 2, 
                        3)))
  if(prior == TRUE){
    # Are the param bounded at 0 (cat 1); at 0 and 1 (cat 2); not bounded (cat 3)
    prior_data <- prior_data  %>%
      mutate(cat = ifelse(param == "s_G" | param == "s_R" | param == "s_T", 1,
                   ifelse(param == "c_GR" | param == "c_TT", 2, 
                   3)))
  }
  
  # Plot marginal posterior
  pp <- function(post_draws,
                 prior_data,
                 line0 = TRUE) # Line at x = 0
                 {
    p <- ggplot(post_draws, aes(x = value, y = param)) +
      
      # Theme and layout
      theme_bw() +
      labs(x = "", y = "", color = 'Percentile Interval') +
      theme(
        axis.text.y = element_text(size = 6.5),
        axis.text.x = element_text(vjust = -1.5),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#b8b5ab", linewidth = 0.3, linetype = "dotted"),
        strip.background = element_rect(fill = "white", color = "white")
      )
    
    if (line0 == TRUE) {
      p <- p +
        geom_vline(xintercept = 0, color = "#b8b5ab", linewidth = 0.3, linetype = "dotted")
    }
    
    if (prior == TRUE) {
      p <- p +
        stat_slab(
          data = prior_data,
          slab_color = NA,
          fill = "#dce4e8",
          scale = 0.5,
          normalize = "groups",
          slab_alpha = 0.5,
          density = "bounded",
          trim = FALSE
        )
    }
    
    # Posterior density
    p + stat_slab(
      slab_color = NA,
      fill = "#e0dfd1",
      scale = 0.5,
      normalize = "groups",
      slab_alpha = 0.7,
      density = "bounded",
      trim = FALSE
    ) +
      
      # Contour of slab
      stat_slab(
        slab_color = "#363533",
        fill = NA,
        slab_linewidth = 0.3,
        scale = 0.5,
        normalize = "groups",
        slab_alpha = 0.75,
        density = "bounded",
        trim = FALSE
      ) %>% return()
  }
  
  # Category 1
  p1 <- pp(post_draws %>% filter(cat == 1),
           prior_data %>% filter(cat == 1),
           line0 = FALSE) +
    annotate(xmin = lims[1], xmax = 0, 
             ymin = -Inf, ymax = Inf, 
             geom = "rect", fill = "#f5f2ed") + 
    scale_x_continuous(limits = lims, expand = c(0, 0)) +
    geom_vline(xintercept = 0, color = "#b8b5ab", linewidth = 0.3, linetype = "dotted") +
    scale_y_discrete(limits = c("s_G", "s_R", "s_T") %>% rev())
    
  # Category 2
  p2 <- pp(post_draws %>% filter(cat == 2),
           prior_data %>% filter(cat == 2)) +
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
  
  # Category 3
  p3 <- pp(post_draws %>% filter(cat == 3),
           prior_data %>% filter(cat == 3)) +
    xlim(limo[1], limo[2]) 
  
  
  if (is.vector(vec_param) == TRUE){
    p3 <- p3 +
      scale_y_discrete(limits = vec_param %>% rev())
  }
  
  # Composite figure layour
  layout <- "
  AB
  AB
  AB
  AC
  AC
  "
  
  (p3 + p1 + p2) +
    plot_layout(design = layout) %>% return()
}

fp %>% saveRDS("./functions/fp.rds")




