# Load libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(readr)
library(terra)
library(geodata)
library(dplyr)
library(osmdata)
library(ggspatial)

# Load dataset
cases <- read_csv("raw_data/epi_metadata/team-defined-traits/gps_aqpseq_groups.csv")

# Convert cases to spatial points
cases_sf <- st_as_sf(
  cases,
  coords = c("lon", "lat"),
  crs = 4326
)

# Download administrative boundaries
peru <- geodata::gadm("PER", level = 3, path = ".")

# Convert to sf
peru_sf <- st_as_sf(peru)

# filter Arequipa
arequipa_city <- peru_sf |>
  dplyr::filter(
    NAME_3 %in% c(
      "Arequipa",
      "Alto Selva Alegre",
      "Cayma",
      "Cerro Colorado",
      "Jacobo Hunter",
      "Jose Luis Bustamante y Rivero",
      "Mariano Melgar",
      "Miraflores",
      "Paucarpata",
      "Sachaca",
      "Socabaya",
      "Yanahuara",
      "Tiabaya"
    )
  )

# Filter all districts in Arequipa province
arequipa_city <- peru_sf |>
  dplyr::filter(NAME_3 %in% c(
    "Alto Selva Alegre",
    "Arequipa",
    "Cayma",
    "Cerro Colorado",
    "Characato",
    "Chiguata",
    "Jacobo Hunter",
    "Jose Luis Bustamante Y Rivero",
    "Mariano Melgar",
    "Miraflores",
    "Mollebaya",
    "Paucarpata",
    "Polobaya",
    "Quequeña",
    "Sabandia",
    "Sachaca",
    "Socabaya",
    "Tiabaya",
    "Uchumayo",
    "Yanahuara",
    "Yarabamba",
    "Yura"
  ))


# Define bounding box around cases
bbox <- st_bbox(cases_sf)


# Get river lines from OpenStreetMap
chili_river <- opq(bbox = bbox) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# Extract the river lines (osm_lines)
chili_river_sf <- chili_river$osm_lines

chili_river_sf <- chili_river_sf %>%
  dplyr::filter(name == "Río Chili")


# Produce map with Chili River with all cases by urban/periurban and north/south
ggplot() +
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  
  # Add river
  geom_sf(data = chili_river_sf, color = "#8ecae6", size = 1) +
  
  geom_sf(
    data = cases_sf,
    aes(color = group_river, shape = group_area),
    size = 2.5
  ) +
  
  scale_shape_manual(values = c("urban" = 16, "periurban" = 17)) +
  scale_color_manual(values = c("north" = "#1f78b4", "south" = "#e31a1c")) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_minimal() +
  labs(color = "River group", shape = "Area type", title = "Sequenced Cases in Arequipa City with Chili River")


#Exclude waterchannels
# Keep only features named "Río Chili" if available
chili_river_sf <- chili_river_sf %>%
  filter(name == "Río Chili")

# Merge all lines into one MULTILINESTRING
chili_river_main <- chili_river_sf %>%
  st_union() %>%
  st_cast("LINESTRING")  # optional: convert to simple line(s)

###Plot figure
ggplot() + # Districts
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  # Chili River
  geom_sf(data = chili_river_main, color = "#8ecae6", size = 5) +
  # Cases
  geom_sf(
    data = cases_sf,
    aes(color = group_river, shape = group_area),
    size = 2.5
  ) +
  # Shapes: urban vs periurban
  scale_shape_manual(values = c("urban" = 16, "periurban" = 17)) +
  # Colors: north vs south
  scale_color_manual(values = c("north" = "#1f78b4", "south" = "#e31a1c")) +
  # Zoom to city
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  # Scale bar
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1) +
  # Clean theme: remove axes
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  # Labels
  labs(
    color = "Position in relation to Chili River",
    shape = "Urbanicity",
    title = "Sequenced Rabies Cases in Arequipa City"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#### Plot by group_geo

ggplot() +
  # Districts
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  # Chili River
  geom_sf(data = chili_river_main, color = "#8ecae6", size = 2.0) +
  # Cases, colored by group_geo
  geom_sf(
    data = cases_sf,
    aes(color = group_geo, shape = group_area),
    size = 2.5
  ) +
  # Shapes: urban vs periurban
  scale_shape_manual(values = c("urban" = 16, "periurban" = 17)) +
  # Colors for group_geo (A to G)
  scale_color_manual(values = c(
    "A" = "#1f78b4",
    "B" = "#33a02c",
    "C" = "#e31a1c",
    "D" = "#ff7f00",
    "E" = "#6a3d9a",
    "F" = "#b15928",
    "G" = "#a6cee3"
  )) +
  # Zoom to city
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  # Scale bar
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1) +
  # Clean theme: remove axes, grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
  ) +
  # Labels
  labs(
    color = "District grouping",
    shape = "Urbanicity",
    title = "Sequenced Rabies Cases in Arequipa City"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


### Plot by group_area (urbanicity) 

ggplot() +
  # Districts
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  # Chili River
  geom_sf(data = chili_river_main, color = "#8ecae6", size = 2.0) +
  # Cases: urban vs periurban
  geom_sf(
    data = cases_sf,
    aes(color = group_area, shape = group_area),
    size = 2.5
  ) +
  # Shapes & colors
  scale_shape_manual(values = c("urban" = 16, "periurban" = 17)) +
  scale_color_manual(values = c("urban" = "#1f78b4", "periurban" = "#f1c40f")) +
  # Zoom
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  # Scale bar
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1) +
  # Theme
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  # Labels
  labs(
    color = "Urbanicity",
    shape = "Urbanicity",
    title = "Sequenced Rabies Cases in Arequipa City by Settlement Type"
  )
