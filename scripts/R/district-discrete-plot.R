# -----------------------------
# 0. Load packages
# -----------------------------
library(tidyverse)
library(ggtree)
library(treeio)
library(wesanderson)
library(patchwork)
library(sf)
library(Polychrome)
library(sf)
library(ggraph)
library(igraph)
library(dplyr)
library(scales)
library(ggpubr)

source("scripts/R/merged_summary_function.R")
jumps=read.csv("analysis/BEAST_runs/discrete-trait-runs/district/district_collect_times.csv")
bf= read.csv("analysis/BEAST_runs/discrete-trait-runs/district/district_Bayes.factor.test.result.csv")
rewards=read.csv("analysis/BEAST_runs/discrete-trait-runs/district/district-Mrewards.csv")
district_summary=create_merged_summary(jumps, bf, rewards, n_trees = 9000) 
#write.csv(district_summary, "analysis/BEAST_runs/discrete-trait-runs/district/district_summary.csv", row.names=F)
# -----------------------------
# 1. Load data
# -----------------------------
#district_summary <- read.csv("analysis/BEAST_runs/discrete-trait-runs/district/")

tree <- read.beast("analysis/BEAST_runs/discrete-trait-runs/district/n148-discrete-district.extractedtree.nexus")

cases <- read.csv("raw_data/epi_metadata/team-defined-traits/gps_aqpseq_groups.csv")

arequipa_city <- st_read("processed_data/gis_data/arequipa-city.shp")
library(sf)

arequipa_city <- st_read("processed_data/gis_data/arequipa-city.shp")

# -----------------------------
# 2. Clean data
# -----------------------------
# district_summary <- district_summary %>%
#   filter(from != "elpedregal", to != "elpedregal")
# 
# tree@data <- tree@data %>%
#   filter(district != "elpedregal")


# -----------------------------
# 3. Define consistent colours
# -----------------------------
geo_levels <- unique(tree@data$district[!is.na(tree@data$district)])#high contrast, bit vulgar:
geo_cols <- setNames(
  colorspace::desaturate(glasbey(length(geo_levels)),amount=0.2),
  geo_levels
)
state_cols <- geo_cols

# -----------------------------
# 4. Convert cases to sf
# -----------------------------
cases_sf <- st_as_sf(
  cases,
  coords = c("lon", "lat"),
  crs = 4326
)

# -----------------------------
# 5. Tree plot
# -----------------------------
tree@data$district <- as.character(tree@data$district)
tree@data$district <- factor(tree@data$district, levels = names(state_cols))

tree_plot <- ggtree(tree, mrsd = "2025-03-08", aes(color = district)) +
  theme_tree2(base_size = 14) +
  scale_color_manual(values = state_cols, name = "district") +
  scale_x_continuous(
    limits = c(2013, 2025),
    breaks = seq(2013, 2025, by = 3)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 30),
    legend.position = "none"
  )

# -----------------------------
# 6. Reward plot (time in state)
# -----------------------------
reward_plot <- district_summary %>%
  group_by(from) %>%
  summarise(avg_reward = mean(reward_percent)) %>%
  ggplot(aes(x = from, y = avg_reward, fill = from)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = state_cols, guide = "none") +
  labs(y = "% time", x = NULL) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5)
  )

# -----------------------------
# 7. Combine tree + inset
# -----------------------------
tree_with_inset <- tree_plot +
  inset_element(
    reward_plot,
    left = 0.65,
    bottom = 0.05,
    right = 0.98,
    top = 0.3
  )

# -----------------------------
# 8. Map
# -----------------------------
# Define map limits from cases
bbox <- st_bbox(cases_sf)
# Add 5% buffer
x_buffer <- (bbox["xmax"] - bbox["xmin"]) * 0.05
y_buffer <- (bbox["ymax"] - bbox["ymin"]) * 0.05

xlim <- c(bbox["xmin"] - x_buffer, bbox["xmax"] + x_buffer)
ylim <- c(bbox["ymin"] - y_buffer, bbox["ymax"] + y_buffer)

district_map <- ggplot() +
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  geom_sf(
    data = cases_sf,
    aes(color = group_district),   
    size = 2.5
  ) +
  scale_color_manual(
   name = "",             # sets legend title
    values = state_cols,           # keeps your colors
    labels = c(
      "ccolorado" = "Cerro Colorado",
      "hunter"    = "Hunter",
      "socabaya"  = "Socabaya",
      "mmelgar"   = "Mariano Melgar",
      "miraflores"= "Miraflores",
      "yura"      = "Yura",
      "cayma"     = "Cayma",
      "jlbyr"     = "J.L.B.Y.R",
      "asa"       = "Alto Selva Alegre",
      "paucarpata"= "Paucarpata",
      "uchumayo"  = "Uchumayo",
      "characato" = "Characato"
    )
  )+
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void(base_size = 14) #+
  theme(legend.position = "none")

# -----------------------------
# 9. Final plot
# -----------------------------
combined_plot <- tree_with_inset | district_map

combined_plot

# -----------------------------
# 10. Save
# -----------------------------
ggsave(
  filename = "results/figures/district-discrete.pdf",
  plot = combined_plot,
  width = 12, height = 6, dpi = 300
)


# -----------------------------
# Network-style map of district centroids
# -----------------------------

#  centroids
district_centroids <- read.csv("analysis/BEAST_runs/discrete-trait-runs/district/district-locations.csv")


district_summary2 <- district_summary %>%
  filter(
    from != "elpedregal",
    to != "elpedregal",
    supported == TRUE, 
    bayes_factor>10
  )
  

# Use your merged_summary for edges
edges <- district_summary2 %>%
  filter(supported) %>%
  mutate(
    from = as.character(from),
    to   = as.character(to),
    origin = from
  )

# Build graph
g <- graph_from_data_frame(edges, directed = TRUE)

# Create manual layout using your district centroids
lay <- create_layout(
  g,
  layout = "manual",
  x = district_centroids$longitude[match(V(g)$name, district_centroids$location)],
  y = district_centroids$latitude[match(V(g)$name, district_centroids$location)]
)
lay$name <- V(g)$name

# Join rewards (or persistence) to layout nodes
lay <- lay %>%
  left_join(rewards %>% rename(location = name), by = c("name" = "location"))

# Identify edge origins for colour mapping
edge_states <- unique(E(g)$origin)
mj_vals <- sort(unique(edges$avg_jumps))

# Plot network
network_plot <- ggraph(lay) +
  geom_node_point(aes(size = percent_time, color = name)) +
  geom_node_text(
    aes(label = name),
    vjust = -0.8,
    size = 3.5,
    fontface = "bold"
  ) +
  geom_edge_arc(
    aes(
      edge_width = mj_vals,
      edge_color = origin,
      label = round(mj_vals, 1)
    ),
    arrow = arrow(length = unit(4, "mm")),
    end_cap = circle(3, "mm"),
    label_size = 5,
    label_colour = "black",
    label_pos = 0.5,
    angle_calc = "none"
  ) +
  scale_edge_color_manual(values = state_cols[edge_states], guide = "none") +
  scale_color_manual(values = state_cols, name = "District") +
  scale_edge_width(
    range = c(0.7, 2.5),
    breaks = round(mj_vals, 1),
    labels = scales::comma(round(mj_vals, 1)),
    name = "MJ"
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
  ggtitle("District-level Movement Network")

network_plot



# 1. Convert centroids to sf points
district_centroids_sf <- district_centroids %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(arequipa_city))
# Join persistence to centroids sf
district_centroids_sf <- district_centroids_sf %>%
  left_join(rewards %>% rename(location = name), by = c("location" = "location"))

# 2. Prepare edges with coordinates
edges_sf <- edges %>%
  left_join(district_centroids, by = c("from" = "location")) %>%
  rename(x_from = longitude, y_from = latitude) %>%
  left_join(district_centroids, by = c("to" = "location")) %>%
  rename(x_to = longitude, y_to = latitude)



# Decide bin breaks manually or via quantiles
# Here, we’ll create “nice” bins based on the range of avg_jumps

edges_sf <- edges_sf %>%
  mutate(
    mj_bin = cut(
      avg_jumps,
      breaks = c(0, 1, 3, 6, 20, Inf),
      include.lowest = TRUE,
      labels = c("0–1", "1–3", "3–6", "6–20", ">20")
    ),
    # Assign numeric widths
    line_width = case_when(
      mj_bin == "0–1"   ~ 0.5,
      mj_bin == "1–3"   ~ 1,
      mj_bin == "3–6"   ~ 1.5,
      mj_bin == "6–20"  ~ 2,
      mj_bin == ">20"   ~ 2.5
    ),
    # Create factor for legend
    line_width_factor = factor(mj_bin, levels = c("0–1", "1–3", "3–6", "6–20", ">20"))
  )

# Define the MJ bins and their corresponding numeric widths
mj_legend <- data.frame(
  mj_bin = factor(c("0–1", "1–3", "3–6", "6–20", ">20"),
                  levels = c("0–1", "1–3", "3–6", "6–20", ">20")),
  line_width = c(0.5, 1, 1.5, 2, 2.5)
)

map_network_plot <- ggplot() +
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  
   geom_sf(data = cases_sf, aes(color = group_district), size = 2.5, alpha=0.5) +
  
  geom_curve(
    data = edges_sf,
    aes(
      x = x_from, y = y_from,
      xend = x_to, yend = y_to,
      color = origin,
      size = line_width,         # actual widths
      linetype = bf_category
    ),
    curvature = 0.4,
    arrow = arrow(length = unit(4, "mm")),
    lineend = "round",
    show.legend = c(color = FALSE, size = TRUE, linetype = TRUE)  # <- exclude color
    
  ) +
  
  scale_color_manual(values = state_cols, name = "District") +
  
  scale_size_continuous(
    range = c(0.5, 2.5),
    breaks = mj_legend$line_width,            # numeric widths for legend
    labels = mj_legend$mj_bin,               # show bin labels
    name = "Jumps"
  ) +
  
  scale_linetype_manual(
    values = c("Decisive" = "solid", "Very strong" = "dashed", "Strong" = "dotted"),
    name = "BF Support"
  ) +
  
  theme_void() +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme(legend.position = "right")
map_network_plot <- map_network_plot +
  guides(
    color = guide_legend(ncol = 2,order = 1),
    size = guide_legend(ncol = 1,order = 2),
    linetype = guide_legend(ncol = 1,order = 3)
  ) +
  theme_void(base_size = 18) 
map_network_plot_no_legend <- map_network_plot+theme(
  legend.position = "none"
)
  
big.legend=get_legend(map_network_plot, position = NULL)

## Final plot

# Combine with relative width

combined_plot <- tree_with_inset | map_network_plot_no_legend
combined_plot
  

combined_plot
ggsave(
  filename = "results/figures/district-discrete-mappedJumps.pdf",
  plot = combined_plot,
  width = 12, height = 6, dpi = 600
)

ggsave(
  filename = "results/figures/district-discrete-mappedJumps-legend.pdf",
  plot = big.legend,
  width = 6, height = 6, dpi = 600
)
