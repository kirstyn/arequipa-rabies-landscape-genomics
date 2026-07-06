library(rmapshaper)
library(sf)
library(dplyr)
library(ggplot2)

library(sf)

aqp_district<- st_read("raw_data/spatial/shapefiles/AQP_AQP_province_districts.shp") %>%
  select(NAME_3, geometry)

# Reduce coordinate precision to help with memory
st_geometry(aqp_district) <- st_set_precision(st_geometry(aqp_district), 1e5)

dist_list <- split(aqp_district, aqp_district$NAME_3)

dist_simp_list <- lapply(dist_list, function(x) {
  ms_simplify(x, keep = 0.05, keep_shapes = TRUE)
})

aqp_district_simp <- do.call(rbind, dist_simp_list)

ggplot(aqp_district_simp) +
  geom_sf(aes(fill = NAME_3), color = "white", size = 0.2) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Arequipa districts (Simplified)", fill = "District")

saveRDS(aqp_district_simp, "processed_data/gis_data/aqp_district_simp.rds")

## extract centroids
centroids <- st_centroid(aqp_district_simp)

# Extract coordinates
coords <- st_coordinates(centroids)

# Combine attributes and coordinates
centroid_data <- aqp_district_simp %>%
  st_drop_geometry() %>%
  mutate(
    Longitude = coords[, "X"],
    Latitude = coords[, "Y"]
  )

# Select and rename relevant columns for CSV
centroid_csv <- centroid_data %>%
  select(Longitude, Latitude, Loc_ID = NAME_3)

# Save to CSV
write.csv(centroid_csv, "processed_data/gis_data/district_centroids.csv", row.names = FALSE)
