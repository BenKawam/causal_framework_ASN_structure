sn <- function(data,
               type = "obs",
               output = "graph",
               col = "none",
               layout = "default",
               coord_df = "auto",
               col_edge = "#3A5B71",
               legend = FALSE,
               subset_group = FALSE,
               grp_nb = 1,
               node_size = 8,
               text_size = 3,
               limits = NA) {
  # Packages
  library(igraph)
  library(ggraph)

  if (subset_group == TRUE) {
    data$id <- data$id %>% filter(grp == grp_nb)
    data$df <- data$df %>% filter(grp == grp_nb)
  }
  
  nodes <- tibble(name = data$id$ID)
  
  if (col != "none") {nodes$X <- data$id$x}
  
  edges <- tibble(
    from = data$df$ind_a,
    to = data$df$ind_b,
    type = type,
    grooming = ifelse(type == "obs", data$df$y_ab, data$df$m_ab)
  ) %>%
    bind_rows(tibble(
      from = data$df$ind_b,
      to = data$df$ind_a,
      type = type,
      grooming = ifelse(type == "obs", data$df$y_ba, data$df$m_ba)
    )) %>%
    mutate(grooming = ifelse(grooming == 0, NA, grooming)) %>%
    select(- type)
  
  G <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
  
  # Plot
  plot <- G %>%
    ggraph(layout = coord_df) +
    geom_edge_fan(aes(alpha = after_stat(index), edge_width = grooming),
                  colour = col_edge) +
    scale_edge_width(range = c(1, 3),
                     limits = limits)
  
  if (col == "discrete") {
    plot <- plot +
      geom_node_point(
        aes(fill = as.factor(X)),
        shape = 21, color = "#f0f2f2", size = node_size) +
      scale_fill_manual(values = c("#6596a3", "#474a5b"))
  }
  
  if (col == "continuous") {
    plot <- plot +
      geom_node_point(
        aes(fill = X),
        shape = 21, color = "#f0f2f2", size = node_size) +
      scale_fill_gradient(low = "#879fad", high = "#1b3647")
  }
  
  if (col == "none") {
    plot <- plot +
      geom_node_point(fill = '#3D485B', shape = 21, color = "#f0f2f2", size = node_size)
  }
  
  plot <- plot +
    geom_node_text(
      aes(label = name),
      vjust = 0.5,
      hjust = 0.5,
      color = "white",
      size = text_size
    ) +
    scale_edge_alpha('Edge direction', guide = 'edge_direction') +
    theme_void() +
    theme(
      panel.border = element_blank(),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
  
  if (legend == FALSE) {plot <- plot + theme(legend.position = "none")}

  
  if(output == "graph"){return(plot)}
  if(output == "edges"){return(edges)}
}

sn %>% saveRDS("./functions/sn.rds")

