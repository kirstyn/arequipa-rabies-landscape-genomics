plot_trait_transitions <- function(df, trait_name = "Trait", bf_cutoff = 3) {
  library(ggplot2)
  library(wesanderson)
  
  # Ensure all character columns
  df <- df %>%
    mutate(from = as.character(from), to = as.character(to))
  
  # Compute significant transitions
  df <- df %>%
    mutate(
      supported = bayes_factor >= bf_cutoff,
      transition = paste(from, "→", to)
    )
  
  # Create a colour palette for BF categories
  heat_cols <- wes_palette("Zissou1", n = 6, type = "continuous")
  df$bf_category <- factor(df$bf_category, levels = c("Negative","Weak","Substantial","Strong","Very strong","Decisive"))
  
  ggplot(df, aes(x = from, y = to, fill = bf_category)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(avg_jumps, 2), alpha = supported)) +
    scale_fill_manual(values = heat_cols) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.4), guide = "none") +
    labs(
      x = "From state",
      y = "To state",
      title = paste("Transitions for", trait_name),
      fill = "BF category"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



plot_trait_transitions_bubble <- function(df, trait_name = "Trait", bf_cutoff = 3) {
  library(ggplot2)
  library(wesanderson)
  
  df <- df %>%
    mutate(
      from = as.character(from),
      to = as.character(to),
      supported = bayes_factor >= bf_cutoff
    )
  
  ggplot(df, aes(x = from, y = to, size = avg_jumps, color = bf_category)) +
    geom_point(alpha = 0.8) +
    scale_color_manual(values = wes_palette("Zissou1", n = 6, type = "continuous")) +
    labs(
      x = "From",
      y = "To",
      color = "BF category",
      size = "Avg Markov jumps",
      title = paste("Transitions for", trait_name)
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
