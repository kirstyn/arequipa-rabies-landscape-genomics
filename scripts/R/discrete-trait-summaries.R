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
library(patchwork)

# -----------------------------
# 1. Load data
# -----------------------------
river_jumps <- read.csv("analysis/BEAST_runs/discrete-trait-runs/river-areas/river_collect_times.csv")
bf <- read.csv("analysis/BEAST_runs/discrete-trait-runs/river-areas/river.Bayes.factor.test.result.csv")
tree <- read.beast("analysis/BEAST_runs/discrete-trait-runs/river-areas/n148-discrete-river.hipst.tre")
rewards <- read.csv("analysis/BEAST_runs/discrete-trait-runs/river-areas/river_rewards.csv")

# Remove 'external' from all relevant datasets
river_jumps <- river_jumps %>% filter(from != "external", to != "external", state != "external")
rewards <- rewards %>% filter(name != "external")
bf <- bf %>% filter(start_name != "external", end_name != "external")
tree@data <- tree@data %>% filter(river != "external")

latest_year <- 2025.1808219178083
n_trees <- 9000

# -----------------------------
# 2. Clean data types
# -----------------------------
river_jumps <- river_jumps %>%
  mutate(
    from = as.character(from),
    to = as.character(to),
    state = as.character(state)
  )

bf <- bf %>%
  mutate(
    start_name = as.character(start_name),
    end_name = as.character(end_name)
  )

# -----------------------------
# 3. Define consistent colours
# -----------------------------
states <- sort(unique(c(river_jumps$from, river_jumps$to)))

state_cols <- setNames(
  wes_palette("FantasticFox1", n = length(states)),
  states
)


# -----------------------------
# 4. Tree plot
# -----------------------------
tree@data$river <- as.character(tree@data$river)
tree@data$river <- factor(tree@data$river, levels = names(state_cols))

tree_plot <- ggtree(tree, mrsd = "2025-03-08", aes(color = river)) +
  theme_tree2(base_size = 14) +  # base text size
  scale_color_manual(values = state_cols, name = "River") +
  scale_x_continuous() +         # x-axis
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 30)  # increase bottom margin
  ) +
  theme(legend.position = "none"); tree_plot

reward_plot <- rewards %>%
  filter(!is.na(name), !is.na(percent_time)) %>%
  mutate(
    name = factor(name, levels = c("south", "north")),
    label = dplyr::recode(name,
                          "north" = "North",
                          "south" = "South"
    )
  ) %>%
  ggplot(aes(x = label, y = percent_time, fill = name)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = state_cols, guide = "none") +
  labs(y = "% time", x = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.5) # add axes lines
  )  +theme_minimal(base_size = 14) 



tree_with_inset <- tree_plot +
  inset_element(
    reward_plot,
    left = 0.65,   # horizontal position
    bottom = 0.05,  # vertical position
    right = 0.98,
    top = 0.3
  )  +theme_minimal(base_size = 14) 

combined_plot <- tree_with_inset | river_map | bf_barplot

# plot.part1=combined_plot & theme(panel.border = element_rect(colour = "black", fill=NA))
# 
# plot.part1+
#   plot_annotation(tag_levels = list(c('A', '', 'B')))&
#   theme(plot.tag = element_text(size = 12))
# # Save plot
# ggsave(
#   filename = "results/figures/river-discrete.pdf",  # change to .pdf if desired
#   plot = plot.part1,
#   width = 12, height = 6, dpi = 300
# )
# -----------------------------
# 5. Transition summaries
# -----------------------------
summary_transitions <- river_jumps %>%
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

merged_summary <- merged_summary  %>% filter(from != "external", to != "external")
# Checked
head(merged_summary)
# write.csv(merged_summary, 
#           file = "analysis/BEAST_runs/discrete-trait-runs/river-areas/merged_summary_with_avg_jumps.csv", 
#           row.names = FALSE)

# -----------------------------
# 6. BF heatmap
# -----------------------------
#bf_palette <- setNames(heat_cols, levels(merged_summary$bf_category))
bf_heatmap <- ggplot(merged_summary, aes(x = from, y = to)) +
  geom_tile(aes(fill = bayes_factor), color = "white") +  # fill by support
  geom_text(aes(label = round(avg_jumps, 2)), size = 4) +  # numbers inside tiles
  scale_fill_gradient(
    low = "grey90",
    high = "grey10",
    name = "Support"
  ) +
  labs(
    x = "From state",
    y = "To state",
    title = "Mean Markov jumps between states"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

bf_heatmap

bf_barplot <- merged_summary %>%
  filter(!is.na(avg_jumps)) %>%
  mutate(transition = paste(from, "→", to)) %>%
  ggplot(aes(x = transition, y = avg_jumps, fill = from)) +
  geom_col() +
  scale_fill_manual(values = state_cols, name = "Origin") +
  labs(
    x = "Transition",
    y = "Mean Markov jumps"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

bf_barplot



# -----------------------------
# 7. MJ summaries (flow plot)
# -----------------------------
mj_counts <- river_jumps %>%
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
mj_cumulative <- river_jumps %>%
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
  scale_color_manual(values = river_cols, name = "Transition") +
  labs(
    x = "Year",
    y = "Average Markov jumps per tree",
    title = "Cumulative Markov jumps through time (significant transitions)"
  ) +
  theme_minimal() +
  theme(
    legend.key.width = unit(1.5, "cm")
  )+
  theme_minimal()

  # -----------------------------
  # 9. Network plot (final, nodes sized by reward)
  # -----------------------------
 merged_summary_2 <-  merged_summary %>%
  dplyr::filter(from != "external", to != "external")

 edges <- merged_summary_2 %>%
    filter(supported) %>%
    mutate(
      from = as.character(from),
      to = as.character(to),
      origin = from   # edge colour mapping
    )
  
  # Build graph
  g <- graph_from_data_frame(edges, directed = TRUE)
  
  # Set node order explicitly
  node_order <- c("south","north")
  
  g <- graph_from_data_frame(edges, directed = TRUE)
  
  # Reorder vertices
  g <- igraph::permute(
    g,
    match(node_order, igraph::V(g)$name)
  )
  
  # Layout
  lay <- create_layout(
    g,
    layout = "manual",
    x = c(-1, 0),   # south, north, external
    y = c(-1, 1)
  )
  lay$name <- c("south", "north")
  
  # Join rewards to layout nodes
  lay <- lay %>%
    left_join(rewards %>% rename(name = name), by = "name")  # 'reward' column added
  
  # Identify which origin states actually exist in edges
  edge_states <- unique(E(g)$origin)
  bf_vals <- sort(unique(edges$bayes_factor))
  
  network_plot <- ggraph(lay) +
    geom_node_point(aes(size = percent_time, color = name)) +
    geom_node_text(
      aes(label = dplyr::recode(name,
                         "external" = "El Pedregal",
                         "north" = "North",
                         "south" = "South")),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    geom_edge_arc(
      aes(
        edge_width = log10(bayes_factor),
        edge_color = origin,
        label = round(avg_jumps, 1)
      ),
      arrow = arrow(length = unit(4, "mm")),
      end_cap = circle(3, "mm"),
      label_size = 5,
      label_colour = "black",
      label_pos = 0.5,
      angle_calc = "none"
    ) +
    scale_edge_color_manual(values = state_cols[edge_states], guide = "none") +
    scale_color_manual(
      values = state_cols,
      labels = c(
        "external" = "El Pedregal",
        "north"    = "North",
        "south"    = "South"
      ),
      name = "State/Origin"
    ) +
    scale_edge_width(
      range = c(0.7, 2.5),
      breaks = log10(bf_vals),
      labels = scales::comma(round(bf_vals)),
      name = "Bayes factor"
    ) +
    scale_size_continuous(
      range = c(4, 12),
      breaks = c(0.2, 30, 60),
      labels = c("0.2%", "30%", "60%"),
      name = "Persistence\n(% time)"
    ) +
    theme_void(base_size = 10) +
    theme(
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.6, "cm"),
      plot.title = element_text(face = "bold", hjust = 0)
    ) +
    ggtitle("B")
  
  network_plot
# -----------------------------
# 10. Final combined plot
# -----------------------------
#final_plot <- (tree_plot | network_plot) / mj_cumulative_plot
  final_plot <- (tree_plot | network_plot) 

final_plot




# -----------------------------
# 5. Transition summaries
# -----------------------------
summary_transitions <- river_jumps %>%
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
#           file = "analysis/BEAST_runs/discrete-trait-runs/river-areas/merged_summary_with_avg_jumps.csv", 
#           row.names = FALSE)

# -----------------------------
# 6. BF heatmap
# -----------------------------
bf_palette <- setNames(heat_cols, levels(merged_summary$bf_category))

bf_heatmap <- ggplot(merged_summary, aes(x = from, y = to, fill = avg_jumps)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "grey90",
    high = "grey10",
    name = "Mean transitions"
  ) +
  labs(
    x = "From state",
    y = "To state",
    title = "Mean Markov jumps between states"
  ) +
  theme_minimal();bf_heatmap

bf_barplot <- merged_summary %>%
  filter(!is.na(avg_jumps)) %>%
  mutate(transition = paste(from, "→", to)) %>%
  ggplot(aes(x = transition, y = avg_jumps, fill = from)) +
  geom_col() +
  scale_fill_manual(values = state_cols, name = "Origin") +
  labs(
    x = "Transition",
    y = "Mean Markov jumps"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

bf_barplot

bf_barplot

# -----------------------------
# 7. MJ summaries (flow plot)
# -----------------------------
mj_counts <- river_jumps %>%
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
mj_cumulative <- river_jumps %>%
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
  )+
  theme_minimal()

  # -----------------------------
  # 9. Network plot (final, nodes sized by reward)
  # -----------------------------
  edges <- merged_summary %>%
    filter(supported) %>%
    mutate(
      from = as.character(from),
      to = as.character(to),
      origin = from   # edge colour mapping
    )
  
  # Build graph
  g <- graph_from_data_frame(edges, directed = TRUE)
  
  # Set node order explicitly
  node_order <- c("south","north")
  
  g <- graph_from_data_frame(edges, directed = TRUE)
  
  # Reorder vertices
  g <- igraph::permute(
    g,
    match(node_order, igraph::V(g)$name)
  )
  
  # Layout
  lay <- create_layout(
    g,
    layout = "manual",
    x = c(-1, 0, 1),   # south, north, external
    y = c(-1, 1, 0)
  )
  lay$name <- c("south", "north")
  
  # Join rewards to layout nodes
  lay <- lay %>%
    left_join(rewards %>% rename(name = name), by = "name")  # 'reward' column added
  
  # Identify which origin states actually exist in edges
  edge_states <- unique(E(g)$origin)
  bf_vals <- sort(unique(edges$bayes_factor))
  
  network_plot <- ggraph(lay) +
    geom_node_point(aes(size = percent_time, color = name)) +
    geom_node_text(
      aes(label = dplyr::recode(name,
                         "north" = "North",
                         "south" = "South")),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    geom_edge_arc(
      aes(
        edge_width = log10(bayes_factor),
        edge_color = origin,
        label = round(avg_jumps, 1)
      ),
      arrow = arrow(length = unit(4, "mm")),
      end_cap = circle(3, "mm"),
      label_size = 5,
      label_colour = "black",
      label_pos = 0.5,
      angle_calc = "none"
    ) +
    scale_edge_color_manual(values = state_cols[edge_states], guide = "none") +
    scale_color_manual(
      values = state_cols,
      labels = c(
        "north"    = "North",
        "south"    = "South"
      ),
      name = "State/Origin"
    ) +
    scale_edge_width(
      range = c(0.7, 2.5),
      breaks = log10(bf_vals),
      labels = scales::comma(round(bf_vals)),
      name = "Bayes factor"
    ) +
    scale_size_continuous(
      range = c(4, 12),
      breaks = c(0.2, 30, 60),
      labels = c("0.2%", "30%", "60%"),
      name = "Persistence\n(% time)"
    ) +
    theme_void(base_size = 10) +
    theme(
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.6, "cm"),
      plot.title = element_text(face = "bold", hjust = 0)
    ) +
    ggtitle("B")
  
  network_plot
# -----------------------------
# 10. Final combined plot
# -----------------------------
#final_plot <- (tree_plot | network_plot) / mj_cumulative_plot
  final_plot <- (tree_plot | network_plot) 

final_plot

