# ========================================
# Full workflow: Assign samples to admin levels
# ========================================

# -------------------------
# Load packages
# -------------------------
library(sf)
library(dplyr)
library(ggplot2)

# -------------------------
# 0. Read metadata and filter
# -------------------------
data <- read.csv("processed_data/processed_metadata/081025_epi-seq_n167_stdDistrict.csv")

# Regions outside study area
outside_region <- c("CAMINACA", "ATUNCOLLA", "AZANGARO")

aqp_region <- data %>%
  filter(!district_std %in% outside_region) %>%
  filter(!is.na(lon), !is.na(lat))

# Convert to sf points
region_points_sf <- st_as_sf(
  aqp_region,
  coords = c("lon", "lat"),
  crs = 4326
)

# -------------------------
# 1. Read KMLs by level
# -------------------------

# ---- Clusters ----
cluster_files <- list.files(
  "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Delimitation_AQP/Delimitacion_cluster",
  pattern = "\\.kml$",
  full.names = TRUE
)

clusters <- do.call(rbind, lapply(cluster_files, st_read))
st_crs(clusters) <- 4326

# Split polygons vs points
cluster_polygons <- clusters[
  st_geometry_type(clusters) %in% c("POLYGON", "MULTIPOLYGON"), 
]

cluster_points <- clusters[
  st_geometry_type(clusters) == "POINT", 
]



# ---- Microreds ----
microred_files <- list.files(
  "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Delimitation_AQP/Delimitacion_microred",
  pattern = "\\.kml$",
  full.names = TRUE
)

microreds <- do.call(rbind, lapply(microred_files, st_read))
st_crs(microreds) <- 4326

# -------------------------
# 2. Optional: plot for sanity check
# -------------------------
ggplot() +
  geom_sf(data = microreds, fill = NA, colour = "red", linewidth = 0.8) +
  geom_sf(data = cluster_polygons, fill = NA, colour = "blue", linewidth = 0.3) +
  geom_sf(data = region_points_sf, colour = "black", size = 1) +
  theme_void()

# -------------------------
# 3. Assign samples to polygons
# -------------------------

# ---- Cluster ----
cases_cluster <- st_join(
  region_points_sf,
  cluster_polygons,
  join = st_within,
  left = TRUE
)
na_idx <- which(is.na(cases_cluster$Name))
nearest_idx <- st_nearest_feature(cases_cluster[na_idx, ], cluster_polygons)
cases_cluster$Name[na_idx] <- cluster_polygons$Name[nearest_idx]

# ---- Microred ----
cases_microred <- st_join(
  cases_cluster,
  microreds,
  join = st_within,
  left = TRUE,
  suffix = c("", "_microred")
)

# -------------------------
# 4. Create final assignment table
# -------------------------
# Replace 'cluster_name', 'district_name', 'microred_name' with actual column names from KMLs
assignments <- cases_microred %>%
  st_drop_geometry() %>%
  select(
    sample_id=ID,
    cluster   = Name,     # e.g. "Name" column in cluster polygons
    microred  = Name_microred     # e.g. "Name" column in microred polygons
  )

# -------------------------
# 5. Save output CSV
# -------------------------
write.csv(
  assignments,
  "processed_data/gis_data/sample_admin_assignments.csv",
  row.names = FALSE
)

# -------------------------
# 6. Optional: Check unassigned samples
# -------------------------
colSums(is.na(assignments))
assignments[is.na(assignments$cluster), ]


