# ==========================================
# BEAST GLM Predictor Matrices Workflow
# ==========================================

# -----------------------------
# Load libraries
# -----------------------------
library(dplyr)
library(sf)       # for GIS layers
library(geosphere) # for geographic distance
library(raster)   # for raster-based resistance surfaces

# -----------------------------
# 1. Load sample metadata
# -----------------------------
meta <- read.table("analysis/BEAST_runs/trait_files/n148-teamDefinedTraits-v2.txt", header=T) %>%
  mutate(
    district = as.factor(district),
    area = ifelse(area == "urban", 1, 0),      # 1 = periurban, 0 = urban
    river = ifelse(river == "north", 1, 0)      # 1 = north, 0 = south
  )

districts <- unique(meta$district)
n_districts <- length(districts)

# -----------------------------
# 2. Prepare empty predictor matrices
# -----------------------------
create_empty_matrix <- function(value = 0) {
  mat <- matrix(value, nrow = n_districts, ncol = n_districts)
  rownames(mat) <- districts
  colnames(mat) <- districts
  return(mat)
}

# -----------------------------
# 3. Periurban predictor
#    - 1 if source or destination is periurban
# -----------------------------
peri_mat <- create_empty_matrix()
for(i in districts) {
  for(j in districts) {
    peri_mat[i,j] <- ifelse(
      any(meta$district == i & meta$area == 1) | any(meta$area == j & meta$area == 1),
      1, 0
    )
  }
}

# -----------------------------
# 4. North/South river predictor
#    - 1 if districts are on opposite sides
# -----------------------------
river_mat <- create_empty_matrix()
for(i in districts) {
  for(j in districts) {
    side_i <- unique(meta$north_side[meta$district == i])
    side_j <- unique(meta$north_side[meta$district == j])
    river_mat[i,j] <- ifelse(side_i != side_j, 1, 0)
  }
}

# -----------------------------
# 5. Sample size predictors
#    - outgoing (source) sample size
#    - incoming (destination) sample size
# -----------------------------
sample_counts <- meta %>%
  group_by(district) %>%
  summarise(n_samples = n())

outgoing_mat <- create_empty_matrix()
incoming_mat <- create_empty_matrix()
for(i in districts) {
  for(j in districts) {
    outgoing_mat[i,j] <- sample_counts$n_samples[sample_counts$district == i]
    incoming_mat[i,j] <- sample_counts$n_samples[sample_counts$district == j]
  }
}

# -----------------------------
# 6. Geographic distance
#    - requires coordinates per district (centroids)
# -----------------------------
# Example: district_coords = data.frame(district, lat, lon)
district_coords <- meta %>%
  group_by(district) %>%
  summarise(lat = mean(lat), lon = mean(lon))

geo_dist_mat <- create_empty_matrix()
for(i in 1:n_districts) {
  for(j in 1:n_districts) {
    geo_dist_mat[i,j] <- distHaversine(
      c(district_coords$lon[i], district_coords$lat[i]),
      c(district_coords$lon[j], district_coords$lat[j])
    ) / 1000  # convert meters → km
  }
}

# Optional: standardize numeric predictors
geo_dist_mat <- scale(geo_dist_mat)
outgoing_mat <- scale(outgoing_mat)
incoming_mat <- scale(incoming_mat)

# -----------------------------
# 7. Resistance / least-cost matrix (optional)
#    - Using raster layers of landscape features
# -----------------------------
# Example: resistance_raster = raster("gis/river_resistance.tif")
# You can compute least-cost distance between centroids for each pair
# Requires gdistance package
# library(gdistance)
# conductance <- 1 / resistance_raster
# ... compute least-cost distances ...

# -----------------------------
# 8. Export matrices for BEAST GLM
# -----------------------------
write.csv(peri_mat, "predictors/periurban_matrix.csv", row.names = TRUE)
write.csv(river_mat, "predictors/river_matrix.csv", row.names = TRUE)
write.csv(geo_dist_mat, "predictors/geodist_matrix.csv", row.names = TRUE)
write.csv(outgoing_mat, "predictors/sample_outgoing_matrix.csv", row.names = TRUE)
write.csv(incoming_mat, "predictors/sample_incoming_matrix.csv", row.names = TRUE)

# Optional: resistance
# write.csv(resistance_matrix, "predictors/resistance_matrix.csv", row.names = TRUE)