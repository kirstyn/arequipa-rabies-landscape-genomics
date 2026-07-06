# ======================================
# Create least-cost / resistance matrix
# ======================================

# Load required libraries
library(raster)
library(gdistance)
library(sp)
library(prettymapr)

# -----------------------------
# 1. Load raster(s) representing landscape resistance
#    - e.g., rivers, urban barriers, roads
#    - Higher values = more resistant (harder to cross)
# -----------------------------
# Example single raster: rivers as barrier
conductance_raster <- raster("analysis/seraphim/resistance_rasters/Elevation_SERAPHIM.asc")

# -----------------------------
# 2. Create transition object
# -----------------------------
tr <- transition(conductance_raster, transitionFunction = mean, directions = 8)

# Correct for geographical distances
tr_corr <- geoCorrection(tr, type = "c")

# -----------------------------
# 3. Define district centroids
#    - A SpatialPoints object with coordinates for each district
# -----------------------------
# Example: assume you have a dataframe of district centroids
# cols: district, lon, lat
district_centroids <- read.csv("analysis/BEAST_runs/discrete-trait-runs/n147-district-locations.csv")
coords <- district_centroids[, c("longitude","latitude")]
rownames(coords) <- district_centroids$location
sp_points <- SpatialPoints(coords, proj4string = CRS(projection(resistance_raster)))
#plot(sp_points)

# # Convert to sf to plot
# sf_points <- st_as_sf(sp_points)

# # If you want district labels, attach them
# sf_points$district <- rownames(coords)
# # Plot
# ggplot() +
#   annotation_map_tile(type = "osm", zoom = 12) +
#   geom_sf(data = sf_points, size = 3) +
#   theme_minimal() 
# -----------------------------
# 4. Compute least-cost distances between all pairs of districts
# -----------------------------
n <- length(sp_points)
lc_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(district_centroids$district, district_centroids$district))

for(i in 1:n){
  for(j in 1:n){
    if(i == j){
      lc_matrix[i,j] <- 0
    } else {
      lc_matrix[i,j] <- costDistance(tr_corr, sp_points[i], sp_points[j])
    }
  }
}

# -----------------------------
# 5. Result
#    - lc_matrix is now a predictor of movement cost
#    - Can feed directly into BEAST GLM
# -----------------------------
print(lc_matrix)
# 
# Make sure names are correct
rownames(lc_matrix) <- district_centroids$location
colnames(lc_matrix) <- district_centroids$location


# Convert to data frame (preserves row/col names)
lc_df <- as.data.frame(lc_matrix)


# Write to CSV
write.csv(
  lc_df,
  file = "analysis/BEAST_runs/predictors/waterchannels_least_cost.csv",
  row.names = TRUE,
  quote = FALSE
)
