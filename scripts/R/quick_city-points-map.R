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
library(ggraph)
library(igraph)
library(dplyr)
library(scales)
library(ggpubr)
library(ggspatial)
library(osmdata)
library(sf)
#install.packages("ggimage")
library(ggimage)


# -----------------------------
# 1. Load data
# -----------------------------


seq_cases <- read.csv("raw_data/epi_metadata/team-defined-traits/gps_aqpseq_groups.csv")

arequipa_city <- st_read("processed_data/gis_data/arequipa-city.shp")  %>%
  filter(NAME_1 %in% c("Arequipa"))
bbox <- st_bbox(arequipa_city)



rivers <- st_read("raw_data/spatial/shapefiles/peru-260528-free.shp/gis_osm_waterways_free_1.shp")
rivers <- st_transform(rivers, 4326)
arequipa_rivers <- st_intersection(rivers, arequipa_city)

arequipa_rivers <- st_intersection(rivers, arequipa_city)%>%
  dplyr::filter(name == "Río Chili")
  
#saveRDS(chili_river, "processed_data/gis_data/chili_river.rds")


aqp_airport <- st_as_sf(
  data.frame(
    name = "Rodríguez Ballón International Airport",
    lon = -71.5675,
    lat = -16.3411
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

all_cases <- read.csv("raw_data/epi_metadata/aqp-allCases-gps-kml.csv")

# -----------------------------
# 4. Convert cases to sf
# -----------------------------
seq_cases_sf <- st_as_sf(
  seq_cases,
  coords = c("lon", "lat"),
  crs = 4326
)

all_cases_sf <- st_as_sf(
  all_cases,
  coords = c("longitude", "latitude"),
  crs = 4326
)


# -----------------------------
# 8. Map
# -----------------------------
# Define map limits from cases
bbox <- st_bbox(seq_cases_sf)
# Add 5% buffer
x_buffer <- (bbox["xmax"] - bbox["xmin"]) * 0.05
y_buffer <- (bbox["ymax"] - bbox["ymin"]) * 0.05

xlim <- c(bbox["xmin"] - x_buffer, bbox["xmax"] + x_buffer)
ylim <- c(bbox["ymin"] - y_buffer, bbox["ymax"] + y_buffer)

sf_use_s2(FALSE)
arequipa_city <- st_make_valid(arequipa_city)

arequipa_city <- st_transform(arequipa_city, 4326)
all_cases_sf <- st_transform(all_cases_sf, 4326)
seq_cases_sf <- st_transform(seq_cases_sf, 4326)
all_cases_sf$type <- "All cases"
seq_cases_sf$type <- "Sequenced"


district_map <- ggplot() +
  
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  
  
  geom_sf(
    data = all_cases_sf,
    aes(colour = type),
    size = 2,
    alpha = 0.6
  ) +
  
  geom_sf(
    data = seq_cases_sf,
    aes(colour = type),
    size = 2.5,
    alpha = 0.8
  ) +
  scale_colour_manual(
    values = c(
      "All cases" = "grey60",
      "Sequenced" = "#3131CC"
    ),
    name = NULL
  )+
  # geom_sf_text(
  #   data = label_points,
  #   aes(label = NAME_3),
  #   size = 3,
  #   colour = "darkred",
  #   fontface = "bold"
  # ) +
  theme_void(base_size = 14) +
  # SCALE BAR
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    text_cex = 1.2,
    line_width = 0.8
  ) +
  geom_sf(
    data = aqp_airport,
    shape = 8,
    size = 0.2,
    colour = "red",
    stroke = 1.2
  )+
  geom_sf(
    data = arequipa_rivers,
    colour =  "#4FC3F7",
    linewidth = 0.8,
    alpha = 0.8
  ) +
  # # NORTH ARROW (COMPASS)
  # annotation_north_arrow(
  #   location = "bl",
  #   which_north = "true",
  #   
  #   pad_x = unit(0.15, "cm"),
  #   pad_y = unit(1.2, "cm"),
  #   
  #   style = north_arrow_fancy_orienteering(
  #     fill = c("black", "white"),
  #     line_col = "black"
  #   )
  # )+
  
  coord_sf(
    xlim = xlim,
    ylim = ylim,
    expand = FALSE,
    label_axes = list(
      bottom = "E",
      left = "N"
    )
  ) +
  
  # CLEAN THEME (publication standard)
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    legend.position = c(0.2, 0.15),   # <- inset position (x, y)
    legend.background = element_rect(
      fill = "white",
      colour = "black",
      linewidth = 0.3
    ),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    
    plot.margin = margin(5, 5, 5, 5)
  )+ guides(colour = guide_legend(override.aes = list(size = 3, alpha = 0.8)))

district_map

ggsave(
  filename = "results/figures/fig1_all-and-seq-cases-arequipa_map_publication.pdf",
  plot = district_map,
  width = 120,
  height = 120,
  units = "mm",
  device = cairo_pdf
)

library(ggspatial)
library(sf)
library(ggplot2)

district_map <- ggplot() +
  
  # BASE MAP
  geom_sf(
    data = arequipa_city,
    fill = "grey95",
    color = "grey40",
    linewidth = 0.4
  ) +
  
  # CASES
  geom_sf(
    data = cases_sf,
    size = 2.5,
    alpha = 0.6,
    colour = "black"
  ) +
  
  # DISTRICT LABELS
  # geom_sf_text(
  #   data = label_points,
  #   aes(label = NAME_3),
  #   size = 3,
  #   colour = "darkred",
  #   fontface = "bold"
  # ) +
  
  # SCALE BAR
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    line_width = 0.4,
    text_cex = 0.8
  ) +
  
  # NORTH ARROW (clean + aligned)
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    
    pad_x = unit(0.2, "cm"),
    pad_y = unit(1.4, "cm"),
    
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    )
  ) +
  
  # COORDINATE FRAME
  coord_sf(
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  ) +
  
  # CLEAN THEME (publication standard)
  theme_bw(base_size = 12) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title = element_blank(),
    
    axis.text = element_text(size = 9, colour = "black"),
    
    axis.ticks = element_line(colour = "black"),
    
    plot.margin = margin(5, 5, 5, 5),
    
    legend.position = "none"
  );district_map 

ggsave(
  filename = "results/figures/district_map_publication.pdf",
  plot = district_map,
  width = 120,
  height = 120,
  units = "mm",
  device = cairo_pdf
)
