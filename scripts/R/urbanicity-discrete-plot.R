# -----------------------------
# 0. Load packages
# -----------------------------
library(tidyverse)
library(ggtree)
library(treeio)
library(wesanderson)
library(ggraph)
library(igraph)
library(grid)
library(tidygraph)
library(patchwork)
library(sf)

# -----------------------------
# 1. Load data
# -----------------------------
area_summary <- read.csv("analysis/BEAST_runs/discrete-trait-runs/urban-periurban/area_merged_summary_with_avg_jumps.csv")
area_tree <- read.beast("analysis/BEAST_runs/discrete-trait-runs/urban-periurban/n148-discrete-area.area.history.hipstr.tre")
cases <- read.csv("raw_data/epi_metadata/team-defined-traits/gps_aqpseq_groups.csv")  
arequipa_city <- st_read("processed_data/gis_data/arequipa-city.shp")

# clean data
area_summary <- area_summary %>%
  filter(from != "external", to != "external") %>%
  mutate(avg_jumps = n_transitions / 9000)

area_tree@data <- area_tree@data %>%
  filter(area.states != "external")

# -----------------------------
# 3. Define consistent colours
# -----------------------------
states <- sort(unique(c(area_summary$from, area_summary$to)))

state_cols <- setNames(
  wes_palette("Royal1", 2),
  states
)

# Convert cases to spatial points
cases_sf <- st_as_sf(
  cases,
  coords = c("lon", "lat"),
  crs = 4326
)


# -----------------------------
# 4. Tree plot
# -----------------------------
area_tree@data$area.states <- as.character(area_tree@data$area.states )
area_tree@data$area.states  <- factor(area_tree@data$area.states , levels = names(state_cols))

tree_plot <- ggtree(area_tree, mrsd = "2025-03-08", aes(color = area.states)) +
  theme_tree2(base_size = 14) +  # base text size
  scale_color_manual(values = state_cols, name = "Urbanicity") +
  scale_x_continuous(
    limits = c(2013, 2025),      # set x-axis range
    breaks = seq(2013, 2025, by = 3)  # breaks every 3 years
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 30),  # increase margins
    legend.position = "none"
  )

tree_plot

reward_plot <- area_summary %>%
  ggplot(aes(x = from, y = reward_percent, fill = from)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = state_cols, guide = "none") +
  labs(y = "% time", x = NULL) +
  theme_minimal(base_size = 16) +  # set minimal theme first
  theme(
  #  axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_line(color = "black", linewidth = 0.5) # add axes lines
  )


tree_with_inset <- tree_plot +
  inset_element(
    reward_plot,
    left = 0.65,   # horizontal position
    bottom = 0.05,  # vertical position
    right = 0.98,
    top = 0.3
  )  


# define map limits

bbox <- st_bbox(cases_sf)
xlim <- c(bbox["xmin"], bbox["xmax"])
ylim <- c(bbox["ymin"], bbox["ymax"])


area_map2 <- ggplot() +
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  geom_sf(
    data = cases_sf,
    aes(color = group_area),
    size = 2.5
  ) +
  scale_color_manual(
    name = "Urbanicity",
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
  )

combined_plot <- tree_with_inset | area_map2
combined_plot

ggsave(
  filename = "results/figures/area-discrete.pdf",  # change to .pdf if desired
  plot = combined_plot,
  width = 8, height = 3.5, dpi = 600
)

