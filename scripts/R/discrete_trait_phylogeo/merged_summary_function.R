create_merged_summary <- function(jumps, bf, rewards, n_trees = 9000) {
  
  library(dplyr)
  
  # -----------------------------
  # 1. Transition summaries
  # -----------------------------
  summary_transitions <- jumps %>%
    group_by(from, to) %>%
    summarise(
      n_transitions = n(),
      mean_time = mean(time, na.rm = TRUE),
      sd_time = sd(time, na.rm = TRUE),
      min_time = min(time, na.rm = TRUE),
      max_time = max(time, na.rm = TRUE),
      .groups = "drop"
    )
  
  # -----------------------------
  # 2. Merge with Bayes factors
  # -----------------------------
  merged_summary <- summary_transitions %>%
    left_join(
      bf,
      by = c("from" = "start_name", "to" = "end_name")
    ) %>%
    mutate(
      bf_category = case_when(
        bayes_factor < 1 ~ "Negative",
        bayes_factor < 3 ~ "Weak",
        bayes_factor < 10 ~ "Substantial",
        bayes_factor < 30 ~ "Strong",
        bayes_factor < 100 ~ "Very strong",
        TRUE ~ "Decisive"
      ),
      bf_category = factor(
        bf_category,
        levels = c("Negative", "Weak", "Substantial", "Strong", "Very strong", "Decisive")
      ),
      supported = bayes_factor >= 3
    )
  
  # -----------------------------
  # 3. Compute average Markov jumps per tree
  # -----------------------------
  merged_summary <- merged_summary %>%
    mutate(
      avg_jumps = n_transitions / n_trees
    )
  
  # -----------------------------
  # 4. Add reward (persistence)
  # -----------------------------
  rewards_clean <- rewards %>%
    group_by(name) %>%
    summarise(
      reward = mean(reward, na.rm = TRUE),
      percent_time = mean(percent_time, na.rm = TRUE),
      .groups = "drop"
    )
  
  merged_summary <- merged_summary %>%
    left_join(
      rewards_clean,
      by = c("from" = "name")
    ) %>%
    dplyr::rename(
      origin_reward = reward,
      reward_percent = percent_time
    )
  
  return(merged_summary)
}