# Install if not yet installed
# install.packages(c("ggplot2", "rnaturalearth", "sf", "ggspatial"))

library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggspatial)
library(dplyr)

# Read your metadata
data <- read.csv("processed_data/processed_metadata/081025_epi-seq_n167.csv")
data <-data %>%
filter(!is.na(lat) & !is.na(lon))
# Clean up column names if needed (optional)
# names(data)

# Make sure lat/lon columns exist
# e.g. data$lat and data$lon — rename if necessary
# data <- data %>% rename(lat = latitude, lon = longitude)

# Convert to spatial object
points_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)

# Load basemap (world or country)
world <- ne_countries(scale = "medium", returnclass = "sf")
aqp <- st_read("raw_data/spatial/shapefiles/AQP_AQP_province_districts.shp")

# Get bounding box based on your points to zoom in automatically
bbox <- st_bbox(points_sf)

# Plot
ggplot() +
  # Add world map for context
  geom_sf(data = world, fill = "antiquewhite", colour = "grey70", size = 0.2) +
  
  # Add Arequipa shapefile (district boundaries)
  geom_sf(data = aqp, fill = NA, colour = "grey40", linewidth = 0.5) +
  
  # Add your sampling points
  geom_sf(data = points_sf, col = "red3", size = 2, alpha = 0.8) +
  
  # Optional: add scale bar and north arrow
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  
  # Zoom to data extent
  coord_sf(
    xlim = c(bbox["xmin"] - 0.1, bbox["xmax"] + 0.1),
    ylim = c(bbox["ymin"] - 0.1, bbox["ymax"] + 0.1)
  ) +
  
  # Clean minimal theme
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  
  labs(title = "Sampling Locations by District")

library(terra)  # newer alternative to 'raster'

# --- You already have this ---
# points_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
# bbox <- st_bbox(points_sf)

# --- Convert bbox to numeric values ---
x_min <- bbox["xmin"]
x_max <- bbox["xmax"]
y_min <- bbox["ymin"]
y_max <- bbox["ymax"]

# --- Expand slightly to give a buffer around the points ---
buffer <- 0.2  # degrees
x_min <- x_min - buffer
x_max <- x_max + buffer
y_min <- y_min - buffer
y_max <- y_max + buffer

# --- Create raster based on bounding box ---
res <- 0.05  # grid cell resolution (in degrees, adjust as needed)

study_area <- rast(
  xmin = x_min, xmax = x_max,
  ymin = y_min, ymax = y_max,
  resolution = res,
  crs = "EPSG:4326"
)

# Fill all cells with 1
values(study_area) <- 1

# --- Save as ASCII raster ---
terra::writeRaster(study_area, "analysis/seraphim/n167_Study_area.asc", overwrite = TRUE)

# --- Optional: Visualise it to confirm ---
plot(study_area, main = "Study Area Raster")
points(points_sf, col = "red", pch = 16)
