# ==============================================================================
# Title: discrete-district-final-plot.R
# Description: Summarise and plot district discrete trait analyses from beast
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

source(here("scripts/R/merged_summary_function.R"))

jumps=read.csv(here("analysis","BEAST_runs","discrete-trait-runs","district","district_collect_times.csv"))
bf= read.csv(here("analysis","BEAST_runs","discrete-trait-runs","district","district_Bayes.factor.test.result.csv"))
rewards=read.csv(here("analysis","BEAST_runs","discrete-trait-runs","district","district-Mrewards.csv"))

# only need once
#district_summary=create_merged_summary(jumps, bf, rewards, n_trees = 9000) 
#write.csv(district_summary, "analysis/BEAST_runs/discrete-trait-runs/district/district_summary.csv", row.names=F)

# -----------------------------
# 1. Load data
# -----------------------------
district_summary <- read.csv(here("analysis","BEAST_runs","discrete-trait-runs","district","district_summary.csv"))

tree <- read.beast(here("analysis","BEAST_runs","discrete-trait-runs","district","n148-discrete-district.extractedtree.nexus"))

cases <- read.csv(here("raw_data","epi_metadata","team-defined-traits","gps_aqpseq_groups.csv"))

arequipa_city <- st_read("processed_data/gis_data/arequipa-city.shp")
arequipa_city <- arequipa_city %>%filter(NAME_2 == "Arequipa")


# -----------------------------
# 2. Clean data
# -----------------------------
# district_summary <- district_summary %>%
#   filter(from != "elpedregal", to != "elpedregal")
# 
# tree@data <- tree@data %>%
#   filter(district != "elpedregal")

# pretty names for districts
district_labels <- c(
  "ccolorado" = "Cerro Colorado",
  "CERRO COLORADO" = "Cerro Colorado",
  "asa" = "Alto Selva Alegre",
  "ALTO SELVA ALEGRE" = "Alto Selva Alegre",
  "mollebaya" = "Mollebaya",
  "MOLLEBAYA" = "Mollebaya",
  "pedregal" = "El Pedregal",
  "elpedregal" = "El Pedregal",
  "yura" = "Yura",
  "YURA" = "Yura",
  "puno" = "Puno",
  "cayma" = "Cayma",
  "CAYMA" = "Cayma",
  "hunter" = "Jacobo Hunter",
  "HUNTER" = "Jacobo Hunter",
  "mmelgar" = "Mariano Melgar",
  "MARIANO MELGAR" = "Mariano Melgar",
  "jlbyr" = "José Luis Bustamante y Rivero",
  "JLBYR" = "José Luis Bustamante y Rivero",
  "paucarpata" = "Paucarpata",
  "PAUCARPATA" = "Paucarpata",
  "characato" = "Characato",
  "CHARACATO" = "Characato",
  "miraflores" = "Miraflores",
  "MIRAFLORES" = "Miraflores",
  "socabaya" = "Socabaya",
  "SOCABAYA" = "Socabaya",
  "sachaca" = "Sachaca",
  "SACHACA" = "Sachaca",
  "uchumayo" = "Uchumayo",
  "UCHUMAYO" = "Uchumayo"
)
tree@data$district_pretty <- district_labels[tree@data$district]
cases$district_pretty <- district_labels[cases$district]


# -----------------------------

# -----------------------------
# 3. Define consistent colours
# -----------------------------

state_cols <- readRDS("scripts/R/geo_cols.rds")

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
tree@data$district_pretty <- as.character(tree@data$district_pretty)
tree@data$district_pretty <- factor(tree@data$district_pretty, levels = names(state_cols))

tree_plot <- ggtree(tree, mrsd = "2025-03-08", aes(color = district_pretty)) +
  theme_tree2(base_size = 14) +
  scale_color_manual(values = state_cols, name = "district") +
  scale_x_continuous(
    limits = c(2013, 2025),
    breaks = seq(2013, 2025, by = 3)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.title = element_text(size = 14),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 30),
    legend.position = "none"
  );tree_plot

# -----------------------------
# 6. Reward plot (time in state)
# -----------------------------
district_summary$from <- district_labels[district_summary$from]
district_summary$to <- district_labels[district_summary$to]

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
  ); reward_plot

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
    aes(color = district_pretty),   
    size = 2.5
  ) +
  scale_color_manual(
    name = "",             # sets legend title
    values = state_cols
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
# ggsave(
#   filename = "results/figures/district-discrete.pdf",
#   plot = combined_plot,
#   width = 12, height = 6, dpi = 300
# )


# -----------------------------
# Network-style map of district centroids
# -----------------------------

#  centroids
district_centroids <- read.csv(here(
  "analysis","BEAST_runs","discrete-trait-runs","district","district-locations.csv")
) %>%
  mutate(
    location_pretty = district_labels[location]
  )

# create centroids
district_centroids_cases <- cases_sf %>%
  group_by(district_pretty) %>%
  summarise(
    n_cases = n(),
    geometry = st_centroid(st_union(geometry)),
    .groups = "drop"
  )

# extract coords
coords <- st_coordinates(district_centroids_cases)

# shift Sachaca south (decrease latitude)
coords[district_centroids_cases$district_pretty == "Sachaca", 2] <-
  coords[district_centroids_cases$district_pretty == "Sachaca", 2] - 0.005

# rebuild geometry properly (CRS preserved)
district_centroids_cases$geometry <- st_sfc(
  lapply(seq_len(nrow(coords)), function(i) {
    st_point(coords[i, ])
  }),
  crs = st_crs(district_centroids_cases)
)

# re-assert sf class
district_centroids_cases <- st_as_sf(district_centroids_cases)


rewards <- rewards %>%
  mutate(
    location_pretty = district_labels[name]
  )

district_summary2 <- district_summary %>%
  filter(
    from != "El Pedregal",
    to != "El Pedregal",
    supported == TRUE, 
    bayes_factor>50
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

# Extract coordinates from sf object
coords <- district_centroids_cases %>%
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry()

# Create manual layout using case-weighted centroids
lay <- create_layout(
  g,
  layout = "manual",
  x = coords$x[match(V(g)$name, coords$district_pretty)],
  y = coords$y[match(V(g)$name, coords$district_pretty)]
)

lay$name <- V(g)$name


# Join rewards (or persistence) to layout nodes
lay <- lay %>%
  left_join(
    rewards,
    by = c("name" = "location_pretty")
  )

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
  theme_void() +
  theme(
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.6, "cm"),
    plot.title = element_text(face = "bold", hjust = 0)
  ) +
  ggtitle("District-level Movement Network")

network_plot


# Join persistence to centroids sf

district_centroids_cases <- district_centroids_cases %>%
  left_join(
    rewards %>% dplyr::select(location_pretty, percent_time),
    by = c("district_pretty" = "location_pretty")
  ) 

district_centroids_cases <- district_centroids_cases %>%
  mutate(
    longitude = st_coordinates(geometry)[,1],
    latitude  = st_coordinates(geometry)[,2]
  )
# 2. Prepare edges with coordinates
edges_sf <- edges %>%
  left_join(
    district_centroids_cases %>%
      dplyr::select(district_pretty, longitude, latitude),
    by = c("from" = "district_pretty")
  ) %>%
  dplyr::rename(x_from = longitude, y_from = latitude) %>%
  left_join(
    district_centroids_cases %>%
      dplyr::select(district_pretty, longitude, latitude),
    by = c("to" = "district_pretty")
  ) %>%
dplyr::rename(x_to = longitude, y_to = latitude)


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
  mj_bin = factor(c("0-1", "1-3", "3-6", "6-20", ">20"),
                  levels = c("0-1", "1-3", "3-6", "6-20", ">20")),
  line_width = c(0.5, 1, 1.5, 2, 2.5)
)

map_network_plot <- ggplot() +
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  
  #geom_sf(data = cases_sf, aes(color = group_district), size = 2.5, alpha=0.5) +
  # Centroid points sized by reward_percent
  geom_sf(
    data = district_centroids_cases,
    aes(size = percent_time,color = district_pretty),  
    alpha = 0.7
  ) +
  geom_curve(
    data = edges_sf,
    aes(
      x = x_from, y = y_from,
      xend = x_to, yend = y_to,
      linewidth = line_width,         
      linetype = bf_category
    ),
    curvature = 0.5,
    arrow = arrow(length = unit(5, "mm"), angle=30),
    lineend = "round",
    show.legend = c(color = FALSE, size = TRUE, linetype = TRUE)  # <- exclude color
    
  ) +
  
  # # Centroid points sized by reward_percent
  # geom_sf(
  #   data = district_centroids_cases,
  #   aes(size = percent_time,color = district_pretty),  
  #   alpha = 0.7
  # ) +
  scale_linewidth_continuous(
    range = c(0.5, 2),
    breaks = mj_legend$line_width,            # numeric widths for legend
    labels = mj_legend$mj_bin,               # show bin labels
    name = "Jumps"
  ) +
  scale_size_continuous(
    range = c(1, 20),               # size range for centroid points
    name = "Persistence (%)",
    breaks = c(1, 10, 50)
  ) +
  scale_linetype_manual(
    values = c("Decisive" = "solid", "Very strong" = "31", "Strong" = "13"),
    name = "Support"
  ) +
  scale_color_manual(
    name = "",             # sets legend title
    values = state_cols

  )+
  theme_void(base_size = 14)+
 coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme(legend.position = "right")
map_network_plot

map_network_plot <- map_network_plot +
  guides(
    color = guide_legend(ncol = 3,order = 1, override.aes = list(size = 4)),
    size = guide_legend(ncol= 1,order = 2,title.theme = element_text(margin = margin(b = -2)) ),
    linetype = guide_legend(ncol = 1,order = 3)
  ) +
  theme_void(base_size = 14) 
map_network_plot_no_legend <- map_network_plot+theme(
  legend.position = "none"
)

big.legend=get_legend(map_network_plot)
plot(big.legend)



reward_plot <- map_network_plot +
  guides(
    size = guide_legend(order = 1, ncol=1),
    linewidth = "none",
    linetype = "none",
    colour="none"
  ) +
  theme(
    
    legend.position = c(0.5, 0.1),
    legend.title = element_text(margin = margin(b = 0)),
    
    legend.key.width = unit(0.2, "cm"),   # shrink space between key and text
    
    legend.spacing.x = unit(0.1, "cm"),
    
    legend.text = element_text(margin = margin(l = -1))); reward_plot

other_plot <- map_network_plot +
  guides(
    size = "none",
    colour= guide_legend(ncol=2, order = 1,override.aes = list(size = 4)),
    linewidth = guide_legend(order = 2),
    linetype = guide_legend(order = 3)
  ) +
  theme(legend.position = "right")
noDistrict_plot <- map_network_plot +
  guides(
    size = "none",
    colour= "none",
    linewidth = guide_legend(order = 2),
    linetype = guide_legend(order = 3)
  ) +
  theme(legend.position = "right")

justDistrict_plot <- map_network_plot +
  guides(
    size = "none",
    colour= guide_legend(ncol=2, order = 1, override.aes = list(size = 4)),
    linewidth = "none",
    linetype = "none"
  ) +
  theme(legend.position = "right")

other_legend <- get_legend(other_plot)
plot(other_legend)

noDistrict_legend <- get_legend(noDistrict_plot)
plot(noDistrict_legend)

justDistrict_legend <- get_legend(justDistrict_plot )
plot(justDistrict_legend)

reward_legend <- get_legend(reward_plot)

combined_legend <- plot_grid(
  other_legend,   # Jumps + BF
  reward_legend,  # Reward
  ncol = 2,
  rel_widths = c(2, 1)  # adjust spacing
)

plot(combined_legend)

## Final plot

# Combine with relative width

combined_plot <- tree_plot | map_network_plot_no_legend
combined_plot
#combined_plot <- tree_plot | reward_plot
combined_plot


combined_plot
# ggsave(
#   filename = here("figures","district-discrete-mappedJumps-BFover50.pdf"),
#   plot = combined_plot,
#   width = 6,
#   height = 5,
#   dpi = 600
# )
# ggsave(
#   filename = here("figures","district-discrete-mappedJumps-BFover50-rewardlegend.pdf"),
#   plot = reward_legend,
#   width = 6, height = 5, dpi = 600
# )

ggsave(
  filename = here("figures","district-discrete-mappedJumps-BFover50-combinedlegend.pdf"),
  plot = combined_legend,
  width = 4, height = 4.5, dpi = 600
)

ggsave(
  filename = here("figures","district-discrete-mappedJumps-BFover50-fulllegend.pdf"),
  plot = big.legend,
  width = 6, height = 6, dpi = 600
)

