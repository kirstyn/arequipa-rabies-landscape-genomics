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
jumps <- read.csv("analysis/BEAST_runs/discrete-trait-runs/district/district_collect_times.csv")
bf <- read.csv("analysis/BEAST_runs/discrete-trait-runs/district/district_Bayes.factor.test.result.csv")
tree <- read.beast("analysis/BEAST_runs/discrete-trait-runs/river-areas/peru-n163-discrete-river-mj_v2.hipstr.tre")
latest_year <- 2025.1808219178083

# -----------------------------
# 2. Wes Anderson palettes
# -----------------------------
state_cols <- wes_palette("Darjeeling1", n = 3)           # river / state
heat_cols  <- wes_palette("Zissou1", n = 6, type = "continuous") # for BF heatmap
flow_cols  <- c("Incoming" = state_cols[1], "Outgoing" = state_cols[2])

# Assign consistent colors for transitions
unique_transitions <- unique(paste(jumps$from, "→", jumps$to))
trans_cols <- setNames(
  colorRampPalette(wes_palette("Zissou1", n = 6, type = "continuous"))(length(unique_transitions)),
  unique_transitions
)

# -----------------------------
# 3. Time-scaled tree
# -----------------------------
tree_plot <- ggtree(tree, mrsd = "2025-03-08", aes(color = group_river)) +
  theme_tree2() +
  scale_color_manual(values = state_cols, name = "River") +
  scale_x_continuous(name = "Year") +
  ggtitle("Time-scaled tree")

# -----------------------------
# 4. Summarise transitions and BF
# -----------------------------
summary_transitions <- jumps %>%
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
  left_join(bf, by = c("from" = "start_name", "to" = "end_name")) %>%
  select(from, to, n_transitions, mean_time, sd_time, min_time, max_time, bayes_factor, posterior_probability) %>%
  mutate(
    bf_category = case_when(
      bayes_factor < 1 ~ "Negative",
      bayes_factor < 3 ~ "Weak",
      bayes_factor < 10 ~ "Substantial",
      bayes_factor < 30 ~ "Strong",
      bayes_factor < 100 ~ "Very strong",
      TRUE ~ "Decisive"
    ),
    bf_category = factor(bf_category, levels = c("Negative", "Weak", "Substantial", "Strong", "Very strong", "Decisive")),
    supported = bayes_factor >= 30
  )

bf_palette <- setNames(heat_cols, levels(merged_summary$bf_category))

bf_heatmap <- ggplot(merged_summary, aes(x = from, y = to, fill = bf_category)) +
  geom_tile(color = "white") +
  # geom_text(aes(label = round(bayes_factor,1))) +
  scale_fill_manual(values = bf_palette, name = "BF support") +
  labs(x = "From state", y = "To state",
       title = "Support for discrete trait transitions") +
  theme_minimal()

# -----------------------------
# 5. Posterior MJ summaries
# -----------------------------
mj_counts <- jumps %>%
  group_by(state, from, to) %>%
  summarise(n_jumps = n(), .groups = "drop")

mj_summary <- mj_counts %>%
  group_by(from, to) %>%
  summarise(
    mean_jumps = mean(n_jumps),
    median_jumps = median(n_jumps),
    lower = quantile(n_jumps, 0.025),
    upper = quantile(n_jumps, 0.975),
    .groups = "drop"
  )

# Incoming / outgoing flows
outgoing <- mj_summary %>%
  group_by(from) %>%
  summarise(mean_jumps = sum(mean_jumps), .groups = "drop") %>%
  mutate(type = "Outgoing", state = from)

incoming <- mj_summary %>%
  group_by(to) %>%
  summarise(mean_jumps = sum(mean_jumps), .groups = "drop") %>%
  mutate(type = "Incoming", state = to)

flow_summary <- bind_rows(outgoing, incoming)

mj_flow_plot <- ggplot(flow_summary, aes(x = state, y = mean_jumps, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = flow_cols, name = "") +
  labs(x = "State", y = "Posterior mean Markov jumps",
       title = "Movement into and out of each state") +
  theme_minimal()

# -----------------------------
# 6. MJ cumulative per transition through time (significant only)
# -----------------------------
# Identify significant transitions first
sig_transitions <- merged_summary %>%
  filter(supported) %>%        # or whatever threshold you define
  mutate(transition = paste(from, "→", to)) %>%
  pull(transition)
n_trees <- 9000   # number of posterior trees after burn-in

mj_cumulative_sig <- jumps %>%
  mutate(
    year = latest_year - time,
    transition = paste(from, "→", to)
  ) %>%
  filter(transition %in% sig_transitions) %>%
  group_by(transition) %>%
  arrange(year) %>%
  mutate(
    cum_jumps = row_number(),
    avg_jumps = cum_jumps / n_trees
  ) %>%
  ungroup()

mj_cumulative_sig_filtered <- mj_cumulative_sig %>% 
  filter(year >= 2007)

mj_cumulative_plot_sig <- ggplot(
  mj_cumulative_sig_filtered,
  aes(x = year, y = avg_jumps, color = transition)
) +
  geom_line(size = 1) +
  scale_color_manual(
    values = trans_cols[names(trans_cols) %in% sig_transitions],
    name = "Transition"
  ) +
  labs(
    x = "Year",
    y = "Average Markov jumps per tree",
    title = "Average Cumulative MJ through time (significant transitions only)"
  ) +
  theme_minimal()

mj_cumulative_plot_sig
# -----------------------------
# 7. Combine figures with patchwork
# -----------------------------
# final_plot <- (tree_plot | bf_heatmap) / (mj_flow_plot | mj_cumulative_plot)
# final_plot

# Only significant transitions
# edges must contain all numeric attributes for width/labels
edges <- merged_summary %>%
  filter(supported) %>%
  mutate(
    from = as.character(from),
    to = as.character(to),
    from_color = from,             # color by origin
    bf_median = bayes_factor,      # just to be explicit
    label = round(bayes_factor,1)  # optional label
  )

# Create igraph object and include edge attributes
g <- graph_from_data_frame(edges, directed = TRUE)
E(g)$bayes_factor <- edges$bf_median
E(g)$label <- edges$label
E(g)$from_color <- edges$from_color

# Create igraph object
g <- graph_from_data_frame(edges, directed = TRUE)

# reorder so external (el pedregal on left)

# get layout coordinates
lay <- create_layout(g, layout = "circle")

# move a specific node (example: "external")
#lay$x[lay$name == "external"] <- -lay$x[lay$name == "external"]
#lay$y[lay$name == "external"] <- -lay$y[lay$name == "external"]

# Plot with continuous edge width (proportional to BF)
network_plot <- ggraph(lay) +
  geom_edge_arc(
    aes(edge_width = bayes_factor, edge_color = from_color),
    arrow = arrow(length = unit(4, "mm")),
    end_cap = circle(3, "mm")
  ) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), vjust = -1.2, fontface = "bold") +
  scale_edge_width(range = c(0.5, 3)) +
  scale_edge_color_manual(values = state_cols) +
  labs(edge_width = "Bayes factor") +
  theme_void()

final_plot <- (tree_plot | network_plot) /  mj_cumulative_plot_sig
final_plot
