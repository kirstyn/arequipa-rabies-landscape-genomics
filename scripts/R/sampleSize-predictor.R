##
library(dplyr)
cases <- read.table("analysis/BEAST_runs/trait_files/n147-teamDefinedTraits-v2.txt", header=T)


# Count number of samples per district
sample_counts <- cases %>%
  group_by(district) %>%
  summarise(n_samples = n())

sample_counts


districts <- sample_counts$district
counts <- sample_counts$n_samples

# Source matrix: rows=source, cols=destination
sample_source_mat <- outer(counts, counts, function(x, y) x)

# Destination matrix: rows=source, cols=destination
sample_dest_mat <- outer(counts, counts, function(x, y) y)

rownames(sample_source_mat) <- districts
colnames(sample_source_mat) <- districts
rownames(sample_dest_mat) <- districts
colnames(sample_dest_mat) <- districts

# Example: assume ses_source_matrix is square with row/col names
diag(sample_source_mat) <- 0  # set all diagonals to zero

# Same for destination matrix
diag(sample_dest_mat) <- 0

# Export for BEAST
write.csv(sample_source_mat, "analysis/BEAST_runs/predictors/sample_source_matrix.csv", row.names = TRUE, quote = FALSE)
write.csv(sample_dest_mat, "analysis/BEAST_runs/predictors/sample_dest_matrix.csv", row.names = TRUE, quote = FALSE)



# 1 = periurban, 0 = urban
district_area <- case %>%
  mutate(area_num = ifelse(area=="periurban", 1, 0)) %>%
  group_by(district) %>%
  summarise(area_mean = mean(area_num))  # fraction periurban

# Optional: round or keep as fraction
area_vals <- round(district_area$area_mean)  # 1 if mostly periurban, 0 if mostly urban

# Make source and destination matrices
urban_source_mat <- outer(area_vals, area_vals, function(x,y) x)
urban_dest_mat <- outer(area_vals, area_vals, function(x,y) y)

rownames(urban_source_mat) <- districts
colnames(urban_source_mat) <- districts
rownames(urban_dest_mat) <- districts
colnames(urban_dest_mat) <- districts