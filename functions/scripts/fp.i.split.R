fp.i.split <- function(samples,
                       target,
                       sd_xlim = c(-0.2, 1.9),
                       oth_xlim = c(-2, 2),
                       heights = c(1.7, 3.5, 2.5),
                       prior = FALSE,
                       vec_param = matrix()){
  s_oth <- samples %>%
    filter(!param %in% c("s_G", "s_R", "s_T", "c_GR", "c_TT"))
  t_oth <- target %>%
    filter(!param %in% c("s_G", "s_R", "s_T", "c_GR", "c_TT"))
  
  s_sd <- samples %>%
    filter(param %in% c("s_G", "s_R", "s_T"))
  t_sd <- target %>%
    filter(param %in% c("s_G", "s_R", "s_T"))
  
  s_cor <- samples %>%
    filter(param %in% c("c_GR", "c_TT"))
  t_cor <- target %>%
    filter(param %in% c("c_GR", "c_TT"))
  
  p1 <- fp.i(s_oth, t_oth, prior) +
    xlim(oth_xlim)
  
  if (is.na(vec_param) == FALSE){
    p1 <- p1 +
      scale_y_discrete(limits = vec_param %>% rev())
  }
  
  p2 <- fp.i(s_sd, t_sd, prior) +
    xlim(sd_xlim) +
    annotate(xmin = -0.2, xmax = 0, 
             ymin = -Inf, ymax = Inf, 
             geom = "rect", fill = "#f5f2ed") + 
    scale_x_continuous(limits = sd_xlim, expand = c(0, 0)) +
    geom_vline(xintercept = 0, color = "#b8b5ab", linewidth = 0.3, linetype = "dotted") +
    scale_y_discrete(limits = c("s_G", "s_R", "s_T") %>% rev())
  
  p3 <- fp.i(s_cor, t_cor, prior) +
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
  
  
  (p1 / p2 / p3) + 
    plot_layout(heights = heights) %>%
    return()
}

fp.i.split %>% saveRDS("./functions/fp.i.split.rds")

