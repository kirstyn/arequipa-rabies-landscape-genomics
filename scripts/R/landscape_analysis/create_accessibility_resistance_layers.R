# ==============================================================================
# Title: create_accessibility_resistance_layers.R
# Description: Create accessibility (travel time) resistance layers for landscape analyses. Data from Nelson, A. (2008)
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

# load raster
r <- rast(here("raw_data","spatial","shapefiles","access_50k/acc_50k.tif"))

# quick plot
plot(r)

base_raster <- rast(here("analysis","seraphim","aqpCity_200m_1kmbuffer.asc"))

# -------------------------
# 1. CROP to study area
# -------------------------
r_crop <- crop(r, base_raster)

# -------------------------
# 2. PROJECT
# -------------------------
if (!compareCRS(r_crop, base_raster)) {
  r_proj <- project(r_crop, base_raster)
} else {
  r_proj <- r_crop
}

# -------------------------
# 3. RESAMPLE to match grid
# -------------------------
r_aligned <- terra::resample(r_proj, base_raster, method = "bilinear")
# -------------------------
# 4. CHECK alignment
# -------------------------
ext(r_aligned)
res(r_aligned)
crs(r_aligned)

# -------------------------
# 5. PLOT
# -------------------------
plot(r_aligned, main = "Aligned acc_50k")
r_log <- log1p(r_aligned)   # log(1 + x), safe for zeros

plot(r_log, col = hcl.colors(100, "viridis"),
     main = "Log-transformed acc_50k")

# 1. NORMALISE (0–1)
# -------------------------
r_max <- global(r_aligned, "max", na.rm = TRUE)[1,1]

r_raw_norm <- r_aligned / r_max
r_log_norm <- r_log / global(r_log, "max", na.rm = TRUE)[1,1]

# -------------------------
# 2. SERAPHIM SCALING FUNCTION
# vt = 1 + k * (v / vmax)
# -------------------------
seraphim_k_scale <- function(r, k) {
  vmax <- global(r, "max", na.rm = TRUE)[1,1]
  r_norm <- r / vmax
  vt <- 1 + k * r_norm
  return(vt)
}

# -------------------------
# 3. APPLY K VALUES
# -------------------------
k_values <- c(10, 100, 1000)

output_dir <- here("analysis","seraphim","resistance_rasters","grid_1kmbuffer/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (k in k_values) {
  
  # RAW
  r_raw_k <- seraphim_k_scale(r_aligned, k)
  
  writeRaster(
    r_raw_k,
    filename = file.path(output_dir,
                         paste0("accessibility_raw_resistance_k", k, ".asc")),
    overwrite = TRUE,
    NAflag = -9999,
    filetype = "AAIGrid"
  )
  
  # LOG
  r_log_k <- seraphim_k_scale(r_log, k)
  
  writeRaster(
    r_log_k,
    filename = file.path(output_dir,
                         paste0("accessibility_log_resistance_k", k, ".asc")),
    overwrite = TRUE,
    NAflag = -9999,
    filetype = "AAIGrid"
  )
  
  # optional quick check
  plot(r_raw_k, main = paste("RAW resistance k =", k))
  plot(r_log_k, main = paste("LOG resistance k =", k))
}
