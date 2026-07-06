# -----------------------------
# 0. Load packages
# -----------------------------
library(tidyverse)
library(ggtree)
library(treeio)
library(patchwork)
library(wesanderson)
library(ggraph)
library(igraph)
library(grid)
library(tidygraph)

# -----------------------------
# 1. Load data
# -----------------------------
area_jumps <- read.csv("analysis/BEAST_runs/discrete-trait-runs/urban-periurban/area_collect_times.csv")
area_bf <- read.csv("analysis/BEAST_runs/discrete-trait-runs/urban-periurban/area_Bayes.factor.test.result.csv")
area_tree <- read.beast("analysis/BEAST_runs/discrete-trait-runs/urban-periurban/n148-discrete-area.area.history.hipstr.tre")
area_rewards <- read.csv("analysis/BEAST_runs/discrete-trait-runs/urban-periurban/area_rewards.csv")

latest_year <- 2025.1808219178083
n_trees <- 9000

# -----------------------------
# 2. Clean data types
# -----------------------------
area_jumps <- area_jumps %>%
  mutate(
    from = as.character(from),
    to = as.character(to),
    state = as.character(state)
  )

area_bf <- area_bf %>%
  mutate(
    start_name = as.character(start_name),
    end_name = as.character(end_name)
  )

# -----------------------------
# 3. Define consistent colours
# -----------------------------
states <- sort(unique(c(area_jumps$from, area_jumps$to)))

state_cols <- setNames(
  wes_palette("GrandBudapest1", n = length(states)),
  states
)


# -----------------------------
# 4. Tree plot
# -----------------------------
area_tree@data$area.states <- as.character(area_tree@data$area.states)
area_tree@data$area.states <- factor(area_tree@data$area.states, levels = names(state_cols))

area_tree_plot <- ggtree(area_tree, mrsd = "2025-03-08", aes(color = area.states)) +
  theme_tree2() +
  scale_color_manual(values = state_cols, name = "area") +
  scale_x_continuous(name = "Year") +
  #ggtitle("Time-scaled tree")+ 
  theme(legend.position = "none")  +ggtitle("A") +
  theme(plot.title = element_text(face = "bold", hjust = 0))

# -----------------------------
# 5. Transition summaries
# -----------------------------
summary_transitions <- area_jumps %>%
  group_by(from, to) %>%
  summarise(
    n_transitions = n(),
    mean_time = mean(time),
    sd_time = sd(time),
    min_time = min(time),
    max_time = max(time),
    .groups = "drop"
  )

merged_summary <- summary_transitions %>%
  left_join(area_bf, by = c("from" = "start_name", "to" = "end_name")) %>%
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
# 11. Add average counts to merged_summary
# -----------------------------

# Compute mean MJ per transition from posterior
avg_mj <- mj_cumulative %>%
  group_by(transition) %>%
  summarise(avg_jumps = mean(avg_jumps), .groups = "drop")

# Add the transition column to merged_summary
merged_summary <- merged_summary %>%
  mutate(transition = paste(from, "→", to)) %>%
  left_join(avg_mj, by = "transition")

# 4. Add reward per origin state
merged_summary <- merged_summary %>%
  left_join(
    rewards %>% select(name, reward, percent_time),
    by = c("from" = "name")
  ) %>%
  rename(
    origin_reward = reward,
    reward_percent = percent_time
  )

# Check
head(merged_summary)
# write.csv(merged_summary,
#           file = "analysis/BEAST_runs/discrete-trait-runs/urban-periurban/area_merged_summary_with_avg_jumps.csv",
#           row.names = FALSE)

# -----------------------------
# 6. BF heatmap
# -----------------------------
area_bf_palette <- setNames(heat_cols, levels(merged_summary$bf_category))

area_bf_heatmap <- ggplot(merged_summary, aes(x = from, y = to, fill = bf_category)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = area_bf_palette, name = "BF support") +
  labs(
    x = "From state",
    y = "To state",
    title = "Support for discrete trait transitions"
  ) +
  theme_minimal()

# -----------------------------
# 7. MJ summaries (flow plot)
# -----------------------------
mj_counts <- area_jumps %>%
  group_by(state, from, to) %>%
  summarise(n_jumps = n(), .groups = "drop")

mj_summary <- mj_counts %>%
  group_by(from, to) %>%
  summarise(
    mean_jumps = mean(n_jumps),
    .groups = "drop"
  )

outgoing <- mj_summary %>%
  group_by(from) %>%
  summarise(mean_jumps = sum(mean_jumps), .groups = "drop") %>%
  mutate(type = "Outgoing", state = from)

incoming <- mj_summary %>%
  group_by(to) %>%
  summarise(mean_jumps = sum(mean_jumps), .groups = "drop") %>%
  mutate(type = "Incoming", state = to)

flow_summary <- bind_rows(outgoing, incoming)

flow_cols <- c(
  "Incoming" = "grey70",
  "Outgoing" = "grey30"
)

mj_flow_plot <- ggplot(flow_summary, aes(x = state, y = mean_jumps, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = flow_cols) +
  labs(
    x = "State",
    y = "Posterior mean Markov jumps",
    title = "Movement into and out of each state"
  ) +
  theme_minimal()

# -----------------------------
# 8. MJ cumulative (transition colour)
# -----------------------------

# Identify significant transitions
sig_transitions <- merged_summary %>%
  filter(supported) %>%
  mutate(transition = paste(from, "→", to)) %>%
  pull(transition)

# Prepare cumulative MJ data
mj_cumulative <- area_jumps %>%
  mutate(
    year = latest_year - time,
    transition = paste(from, "→", to),
    origin = from
  ) %>%
  filter(transition %in% sig_transitions) %>%
  group_by(transition) %>%
  arrange(year) %>%
  mutate(
    cum_jumps = row_number(),
    avg_jumps = cum_jumps / n_trees
  ) %>%
  ungroup() %>%
  filter(year >= 2007)

# Generate a distinct colour palette per transition
sig_transitions_uniq <- unique(mj_cumulative$transition)
transition_cols <- setNames(
  colorRampPalette(wes_palette("Moonrise3", n = 3))(length(sig_transitions_uniq)),
  sig_transitions_uniq
)

# Plot cumulative Markov jumps per transition
mj_cumulative_plot <- ggplot(
  mj_cumulative,
  aes(x = year, y = avg_jumps, color = transition, group = transition)
) +
  geom_line(linewidth = 1) +  # solid lines
  scale_color_manual(values = transition_cols, name = "Transition") +
  labs(
    x = "Year",
    y = "Average Markov jumps per tree",
    title = "Cumulative Markov jumps through time (significant transitions)"
  ) +
  theme_minimal() +
  theme(
    legend.key.width = unit(1.5, "cm")
  )
theme_minimal()

# -----------------------------
# 9. Network plot (final, nodes sized by reward)
# -----------------------------
source("scripts/R/network_graph_function.R")
net_data <- prepare_network_data(merged_summary, area_rewards)

network_plot <- plot_trait_network(
  edges = net_data$edges,
  nodes = net_data$nodes,
  state_cols = state_cols,
  panel_label = "B"
);network_plot
# -----------------------------
# 10. Final combined plot
# -----------------------------
#final_plot <- (tree_plot | network_plot) / mj_cumulative_plot
final_plot <- (area_tree_plot | network_plot) 

final_plot

