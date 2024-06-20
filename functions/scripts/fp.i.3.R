fp.i.3 <- function(
    target,
    sample) {
  
  # Bind target values from three models
  target_df <- bind_rows(target[[1]] %>% mutate(model = "a1"),
                         target[[2]] %>% mutate(model = "a2"),
                         target[[3]] %>% mutate(model = "a3"))
  
  # Bind posterior draws from adjusted and unadjusted models
  samples_df <- bind_rows(sample[[1]] %>% mutate(model = "a1"),
                          sample[[2]] %>% mutate(model = "a2"),
                          sample[[3]] %>% mutate(model = "a3"))
  
  # Plot
  fp.i(samples_df = samples_df,
       target_df = target_df) +
    
    # One panel per estimator
    facet_grid(~ factor(model, levels = c('a1', 'a2', 'a3'))) +
    
    # Theme
    theme(strip.text = element_blank()) %>%
    
    # Output of the function is the ggplot
    return()
}

fp.i.3 %>% saveRDS("./functions/fp.i.3.rds")
