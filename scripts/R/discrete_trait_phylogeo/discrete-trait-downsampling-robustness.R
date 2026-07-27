# ==============================================================================
# Title: discrete-trait-downsampling-robustness.R
# Description: Defining whether results are sensitive or robust to downsampling (criteria applied to results)
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

df <- read.csv(here("analysis","BEAST_runs","discrete-trait-runs","collated-results","table_transitions.csv"))


# Rules applied to assess robustness of results:
df <- df %>%
  mutate(
    Stable = case_when(
      # Robust: BF-supported in full AND at least one DS recovers support
      Full.BF > 10 &
        (DS1.BF > 10 | DS2.BF > 10) ~ "Robust",
      # Sensitive: BF-supported in full but not recovered in DS
      Full.BF > 10 ~ "Sensitive",
      # Not in analysis set 
      TRUE ~ NA_character_
    )
  )

#write.csv(df,here("analysis","BEAST_runs","discrete-trait-runs","collated-results","table_transitions-downsampled-robustness.csv"))
