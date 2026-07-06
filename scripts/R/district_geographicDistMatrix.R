library(geosphere)


# cols: district, lon, lat
district_centroids <- read.csv("analysis/BEAST_runs/discrete-trait-runs/n147-district-locations.csv")
coords <- district_centroids[, c("longitude","latitude")]
rownames(coords) <- district_centroids$location


# Compute pairwise great-circle distances (meters)
geo_matrix <- outer(1:nrow(coords), 1:nrow(coords), Vectorize(function(i, j) {
  if(i == j) return(0)
  distHaversine(coords[i, ], coords[j, ])
}))

# Assign row and column names
rownames(geo_matrix) <- district_centroids$location
colnames(geo_matrix) <- district_centroids$location

# Convert to data frame for CSV
geo_df <- as.data.frame(geo_matrix)

# Write CSV for BEAST GLM
write.csv(
  geo_df,
  file = "analysis/BEAST_runs/predictors/geographic_distance.csv",
  row.names = TRUE,
  quote = FALSE
)
