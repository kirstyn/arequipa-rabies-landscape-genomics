# ==============================================================================
# Title: create_trait_summary_tables.R
# Description: Gathers results from discrete trait analyses (BF, MJ, MR) and summarises in table
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

source(here("scripts/R/merged_summary_function.R"))


#############################################
#  Create summary tables for traits        #
#############################################

### river 
river_jumps <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "river-areas","river_collect_times.csv"))
river_bf <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "river-areas","river.Bayes.factor.test.result.csv"))
river_rewards <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "river-areas","river_rewards.csv"))

# apply summarise function
river_summary=create_merged_summary(river_jumps, river_bf, river_rewards,n_trees = 9000) %>%
dplyr::mutate(trait = "river")


### river-downsampled 
river_jumps <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "river-areas", "river-downsampled","n148_treemmer_pruned_1_river.collect_times.csv"))
river_bf <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "river-areas","river-downsampled", "Bayes.factor.test.result.csv") )
river_rewards <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "river-areas","river-downsampled", "n148_treemmer_pruned_1_river.rewards.csv"))

# summarise: 
river_summary=create_merged_summary(river_jumps, river_bf, river_rewards) %>%
  mutate(trait = "river")
#write.csv(river_summary,here("analysis","BEAST_runs","discrete-trait-runs","river-areas","river-downsampled","downsampled_river_merged_summary_with_avg_jumps.csv"), row.names = F)

#############################################

### area
area_jumps <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "urban-periurban","area_collect_times.csv"))

area_bf <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "urban-periurban","area_Bayes.factor.test.result.csv"))

area_rewards <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "urban-periurban","area_rewards.csv"))

# summarise
area_summary=create_merged_summary(area_jumps, area_bf, area_rewards) %>%
  mutate(trait = "area")
#write.csv(area_summary,here("analysis","BEAST_runs","discrete-trait-runs","urban-periurban","area_merged_summary_with_avg_jumps.csv"))

#############################################

### district
district_jumps <- read.csv(
  here("analysis", "BEAST_runs", "discrete-trait-runs", "district",
       "district_collect_times.csv"))
district_bf <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "district","district_Bayes.factor.test.result.csv"))

district_rewards <- read.csv(here("analysis", "BEAST_runs", "discrete-trait-runs", "district","district_rewards.csv"))

# summarise
district_summary=create_merged_summary(district_jumps, district_bf, district_rewards) %>%
  mutate(trait = "district")

# collect all summaries
all_summary <- bind_rows(
  river_summary,
  area_summary,
  district_summary
)

#############################################
# Look at residence Vs exchange
# Quadrant plots

res_exchange <- all_summary %>%
  group_by(trait,from,reward_percent) %>%
  summarise(
    total_outgoing = sum(avg_jumps, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::rename(name = from)%>%
  dplyr::rename(persistence = reward_percent)

incoming <- all_summary %>%
  group_by(to) %>%
  summarise(total_incoming = sum(avg_jumps, na.rm = TRUE), .groups = "drop")

res_exchange <- res_exchange %>%
  left_join(incoming, by = c("name" = "to"))


quadrants <- res_exchange %>%
  group_by(trait) %>%
  summarise(
    med_persistence = median(persistence, na.rm = TRUE),
    med_outgoing = median(total_outgoing, na.rm = TRUE),
    .groups = "drop"
  )

# Join medians to res_exchange for plotting facets
res_exchange2 <- res_exchange %>% left_join(quadrants, by = "trait")

# Plot
ggplot(res_exchange2, aes(x = persistence, y = total_outgoing)) +
  
  # Points
  geom_point(aes(color = name), size = 4) +
  
  # Labels
  geom_text(
    aes(label = name),
    vjust = -1,
    fontface = "bold",
    size = 3
  ) +
  
  # Quadrant lines
  geom_vline(aes(xintercept = med_persistence), data = quadrants, linetype = "dashed", color = "grey50") +
  geom_hline(aes(yintercept = med_outgoing), data = quadrants, linetype = "dashed", color = "grey50") +
  
  # Facets per trait
  facet_wrap(~trait) +
  
  # Labels
  labs(
    x = "Persistence (% time)",
    y = "Total outgoing Markov jumps",
  ) +
  
  theme_minimal(base_size = 11)
