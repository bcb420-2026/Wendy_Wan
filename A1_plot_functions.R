# custom plotting functions for BCB420 Assignment 1

# define the boxplot function
plot_box <- function(mat, groups, main = "Distribution of Log2 Counts", ylab = "log2(CPM + 1)") {
  
  df <- as.data.frame(mat) %>%
    mutate(gene = rownames(mat)) %>% 
    pivot_longer(cols = -gene, names_to = "sample", values_to = "value") %>%
    # Using log2(value + 1) to handle zeros and normalize scale for visualization
    mutate(log_value = log2(value + 1))
  
  # map samples to their conditions
  meta_lookup <- data.frame(sample = colnames(mat), Condition = groups)
  df <- left_join(df, meta_lookup, by = "sample")
  
  # generate the ggplot object
  ggplot(df, aes(x = sample, y = log_value, fill = Condition)) +
    geom_boxplot(outlier.size = 0.2, alpha = 0.7) +
    # mock = skyblue, infected = salmon
    scale_fill_manual(values = c("Mock" = "grey50", "Infected" = "firebrick3")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = main, x = "NHBE Sample (Series 1)", y = ylab)
}

# define the density plotting function
plot_density <- function(mat, groups, main = "Expression Density Plot") {
  
  df <- as.data.frame(mat) |>
    mutate(gene = rownames(mat)) |>
    pivot_longer(cols = -gene, names_to = "sample", values_to = "value") |>
    mutate(log_value = log2(value + 1))
  
  # map samples to their conditions
  meta_lookup <- data.frame(sample = colnames(mat), Condition = groups)
  df <- left_join(df, meta_lookup, by = "sample")
  
  # generate the density plot
  ggplot(df, aes(x = log_value, color = sample)) +
    geom_density(linewidth = 0.8, linetype = "solid") +
    scale_color_brewer(palette = "Dark2") + 
    theme_bw() +
    labs(title = main, 
         x = "log2(CPM + 1)", 
         y = "Density", 
         color = "Sample ID") +
    theme(legend.position = "right", 
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))
}