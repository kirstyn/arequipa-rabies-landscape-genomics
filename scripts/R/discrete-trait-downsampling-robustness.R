
library(dplyr)
df <- read.csv("analysis/BEAST_runs/discrete-trait-runs/collated-results/supp_table_transitions.csv")


# Rules applied to assess robustness of results:

df <- df %>%
  mutate(
    Stable = case_when(
      
      # Robust: BF-supported in full AND at least one DS recovers support
      Full.BF > 10 &
        (DS1.BF > 10 | DS2.BF > 10) ~ "Robust",
      
      # Sensitive: BF-supported in full but not recovered in DS
      Full.BF > 10 ~ "Sensitive",
      
      # Not in analysis set (optional if you keep full table)
      TRUE ~ NA_character_
    )
  )

write.csv(df,"analysis/BEAST_runs/discrete-trait-runs/collated-results/supp_table_transitions-downsampled-robustness.csv")
