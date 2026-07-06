library(dplyr)
library(ggplot2)
source("scripts/R/merged_summary_function.R")
## Create summary tables for traits

### river 
river_jumps <- read.csv("analysis/BEAST_runs/discrete-trait-runs/river-areas/river_collect_times.csv")
river_bf <- read.csv("analysis/BEAST_runs/discrete-trait-runs/river-areas/river.Bayes.factor.test.result.csv")
river_rewards <- read.csv("analysis/BEAST_runs/discrete-trait-runs/river-areas/river_rewards.csv")

river_summary=create_merged_summary(river_jumps, river_bf, river_rewards,n_trees = 9000) %>%
  mutate(trait = "river")


### river-downsampled 
river_jumps <- read.csv("analysis/BEAST_runs/discrete-trait-runs/river-areas/river-downsampled/n148_treemmer_pruned_1_river.collect_times.csv")
river_bf <- read.csv("analysis/BEAST_runs/discrete-trait-runs/river-areas/river-downsampled/Bayes.factor.test.result.csv")
river_rewards <- read.csv("analysis/BEAST_runs/discrete-trait-runs/river-areas/river-downsampled/n148_treemmer_pruned_1_river.rewards.csv")

river_summary=create_merged_summary(river_jumps, river_bf, river_rewards) %>%
  mutate(trait = "river")
#write.csv(river_summary,"analysis/BEAST_runs/discrete-trait-runs/river-areas/river-downsampled/downsampled_river_merged_summary_with_avg_jumps.csv", row.names = F)

### area
area_jumps <- read.csv("analysis/BEAST_runs/discrete-trait-runs/urban-periurban/area_collect_times.csv")
area_bf <- read.csv("analysis/BEAST_runs/discrete-trait-runs/urban-periurban/area_Bayes.factor.test.result.csv")
area_rewards <- read.csv("analysis/BEAST_runs/discrete-trait-runs/urban-periurban/area_rewards.csv")  

area_summary=create_merged_summary(area_jumps, area_bf, area_rewards) %>%
  mutate(trait = "area")
write.csv(area_summary,"analysis/BEAST_runs/discrete-trait-runs/urban-periurban/area_merged_summary_with_avg_jumps.csv")

### district
district_jumps <- read.csv("analysis/BEAST_runs/discrete-trait-runs/district/district_collect_times.csv")
district_bf <- read.csv("analysis/BEAST_runs/discrete-trait-runs/district/district_Bayes.factor.test.result.csv")
district_rewards <- read.csv("analysis/BEAST_runs/discrete-trait-runs/district/district_rewards.csv")  
district_summary=create_merged_summary(district_jumps, district_bf, district_rewards) %>%
  mutate(trait = "district")

all_summary <- bind_rows(
  river_summary,
  area_summary,
  district_summary
)


# Compute medians per trait for quadrant guidance
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
  geom_point(aes(color = state), size = 4) +
  
  # Labels
  geom_text(
    aes(label = state),
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
    x = "Persistence (% time in state)",
    y = "Total outgoing Markov jumps",
    title = "Residence vs Exchange across traits"
  ) +
  
  theme_minimal(base_size = 11)