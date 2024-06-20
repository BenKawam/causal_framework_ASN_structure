dvp.i <- function(target_df, # Target values
                  sample, # Posterior samples
                  xmin = -2.5, # Lower limit x
                  xmax = 2.5, # Upper limit x
                  n_var = 20) # Number of varying eff. to sample from
                  {

target_df <- target_df %>%
  select(dyad, ind_a, ind_b, tau_ab) %>%
  rename(g = ind_a, r = ind_b, tau = tau_ab) %>%
  mutate(direction = 1) %>%
  bind_rows(target_df %>%
              select(ind_a, ind_b, tau_ba, dyad) %>%
              rename(g = ind_b, r = ind_a, tau = tau_ba) %>%
              mutate(direction = 2)) %>%
  filter(dyad < (n_var + 1)) %>%
  group_by(dyad, direction) %>%
  slice(1) %>%
  mutate(param = paste0("T[", g, ",", r, "]"),
         param = fct_reorder(param, as.numeric(dyad))) %>%
  rename(value = tau) %>%
  select(-c(g, r)) %>%
  ungroup()

p1 <- sample %>%
  mutate(direction = ifelse(str_sub(param, start = 1, end = 4) == "T_ab", 1, 2),
         dyad = parse_number(param)) %>%
  select(-param) %>%
  left_join(target_df %>% select(-value), by = c("direction", "dyad")) %>%
  filter(direction == 1) %>%
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
  geom_point(data = target_df %>% filter(direction == 1),
             fill = "#ab0f24",
             pch = 21,
             colour = "white",
             size = 1.8) +
  
  xlim(c(xmin, xmax)) +
  scale_y_discrete(limits = rev)


p2 <- sample %>%
  mutate(direction = ifelse(str_sub(param, start = 1, end = 4) == "T_ab", 1, 2),
         dyad = parse_number(param)) %>%
  select(-param) %>%
  left_join(target_df %>% select(-value), by = c("direction", "dyad")) %>%
  filter(direction == 2) %>%
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
  geom_point(data = target_df %>% filter(direction == 2),
             fill = "#ab0f24",
             pch = 21,
             colour = "white",
             size = 1.8) +
  
  xlim(c(xmin, xmax)) +
  scale_y_discrete(limits = rev, position = "right")

(p1 + p2) %>%
  return()
}

dvp.i %>% saveRDS("./functions/dvp.i.rds")
