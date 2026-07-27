# ==============================================================================
# Title: river-discrete-plot.R
# Description: Summarise and plot river as discrete trait analyses from beast
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

# -----------------------------
# 1. Load data
# -----------------------------
river_summary <- read.csv(here("analysis","BEAST_runs","discrete-trait-runs","river-areas","merged_summary_with_avg_jumps.csv"))

# downsampled
#river_summary <- read.csv(here("analysis","BEAST_runs","discrete-trait-runs","river-areas","river-downsampled","downsampled_river_merged_summary_with_avg_jumps.csv"))

tree <- read.beast(here("analysis","BEAST_runs","discrete-trait-runs","river-areas","n148-discrete-river.hipst.tre"))
#tree <- read.beast(here("analysis","BEAST_runs","discrete-trait-runs","river-areas","river-downsampled","n148_treemmer_pruned_1_river.hipstr.tre"))

cases <- read.csv(here("raw_data","epi_metadata","team-defined-traits","gps_aqpseq_groups.csv"))

arequipa_city <- st_read(here("processed_data","gis_data","arequipa-city.shp"))

# -----------------------------
# 2. Clean data
# -----------------------------
river_summary <- river_summary %>%
  filter(from != "external", to != "external")

tree@data <- tree@data %>%
  filter(river != "external")

# -----------------------------
# 3. Define consistent colours
# -----------------------------
states <- sort(unique(c(river_summary$from, river_summary$to)))

state_cols <- setNames(
  wes_palette("FantasticFox1", length(states)),
  states
)


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
tree@data$river <- as.character(tree@data$river)
tree@data$river <- factor(tree@data$river, levels = names(state_cols))

tree_plot <- ggtree(tree, mrsd = "2025-03-08", aes(color = river)) +
  theme_tree2(base_size = 14) +
  scale_color_manual(values = state_cols, name = "River") +
  scale_x_continuous(
    limits = c(2012, 2025),
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
reward_plot <- river_summary %>%
  ggplot(aes(x = from, y = reward_percent, fill = from)) +
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
xlim <- c(bbox["xmin"], bbox["xmax"])
ylim <- c(bbox["ymin"], bbox["ymax"])

chili_river <- readRDS(here("processed_data","gis_data","chili_river.rds"))
river_map <- ggplot() +
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  geom_sf(data = chili_river,
          colour = "#4FC3F7",
          linewidth = 0.8) +
  geom_sf(
    data = cases_sf,
    aes(color = group_river),   
    size = 2.5
  ) +
  scale_color_manual(
    name = "River\npositionality",
    values = state_cols
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void(base_size = 14) +
  theme(
    legend.position = c(0.01, 0.02),  # adjust as needed
    legend.justification = c(0, 0),
    legend.background = element_rect(
      fill = "white",
      colour = "black",
      linewidth = 0.3
    ),
    legend.box.background = element_rect(
      fill = "white",
      colour = "black",
      linewidth = 0.3
    ),
    legend.key = element_rect(fill = "white", colour = NA),
    #legend.title = element_text(size = 14,face = "bold",margin = margin(b = 2) ),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
  ); river_map 
# -----------------------------
# 9. Final plot
# -----------------------------
combined_plot <- tree_with_inset | river_map

combined_plot

# -----------------------------
# 10. Save
# -----------------------------
# ggsave(
#   filename = here("figures","river-discrete.pdf"),
#   plot = combined_plot,
#   width = 8, height = 3.5, dpi = 600
# )
