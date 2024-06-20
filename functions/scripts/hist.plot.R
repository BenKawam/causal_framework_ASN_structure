hist.plot <- function(vec, # Vector of numbers
                      binw = 1, # Bin width
                      fill = "#27495c",
                      col = "#f7f7f0",
                      linewidth = 0.1) {
  # Reformatting
  vec %>%
    tibble() %>%
    ggplot(aes(.))  +
    # Histogram
    geom_histogram(
      binwidth = binw,
      color = col,
      linewidth = linewidth,
      fill = fill
    ) +
    stat_bin(
      binwidth = binw,
      geom = "text",
      aes(label = after_stat(
        if_else (condition = count > 0 & count < 3,
                 as.character(count), "")
      )),
      vjust = -1,
      size = 2,
      col = "#4d4d4d"
    ) +
    # Layour
    theme_bw() +
    theme(
      axis.text.x = element_text(vjust = 0),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(
        color = "#ccccc0",
        linewidth = 0.3,
        linetype = "dotted"
      ),
      panel.border = element_blank(),
      strip.background = element_rect(fill = "white", color = "white"),
      plot.margin = margin(1, 0, 0, 0, "cm"),
    ) +
    labs(y = "") +
    scale_y_continuous(expand = c(0.1, 20)) +
    scale_x_continuous(expand = c(0.02, 0)) +
    labs(x = "")
}

hist.plot %>% saveRDS("./functions/hist.plot.rds")
