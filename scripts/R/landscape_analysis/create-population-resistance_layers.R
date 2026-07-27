# ==============================================================================
# Title: create-population-resistance_layers.R
# Description: Create landscape rasters of population density, change etc for seraphim analyses
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))
# -------------------------
# LOAD RASTERS
# -------------------------
r_2025 <- rast(here("raw_data","spatial","shapefiles","DX_data/per_pop_2025_CN_100m_R2025A_v1.tif"))
r_2015 <- rast(here("raw_data","spatial","shapefiles","DX_data","per_pop_2015_CN_100m_R2025A_v1.tif"))

base_raster <- rast(here("analysis","seraphim","aqpCity_200m_1kmbuffer.asc"))

# -------------------------
# ALIGN FUNCTION
# -------------------------
align_to_base <- function(r, base) {
  r <- terra::crop(r, base)
  if (!terra::same.crs(r, base)) {
    r <- terra::project(r, base)
  }
  r <- terra::resample(r, base, method = "bilinear")
  return(r)
}

# -------------------------
# ALIGN RASTERS
# -------------------------
r2025 <- align_to_base(r_2025, base_raster)
r2015 <- align_to_base(r_2015, base_raster)

# -------------------------
# CLEAN DATA
# -------------------------
r2025[r2025 == 0] <- NA
r2015[r2015 == 0] <- NA

# -------------------------
# LOG CHANGE (RELATIVE CHANGE BASE)
# -------------------------
r_change <- log1p(r2025) - log1p(r2015)

# -------------------------
# Z-SCORE STANDARDISATION
# -------------------------
r_mean <- global(r_change, "mean", na.rm = TRUE)[1,1]
r_sd   <- global(r_change, "sd", na.rm = TRUE)[1,1]

r_change_z <- (r_change - r_mean) / r_sd

# -------------------------
# HYPOTHESIS SURFACES
# -------------------------

# 1. MAGNITUDE (disturbance intensity)
mag_res <- abs(r_change_z)

# 2. GROWTH (population increase only)
growth_res <- r_change_z
growth_res[growth_res < 0] <- 0

# 3. DECLINE (population decrease only)
decline_res <- r_change_z
decline_res[decline_res > 0] <- 0
decline_res <- abs(decline_res)


# 4. ABSOLUTE CHANGE (raw demographic turnover)
abs_change <- r2025 - r2015
abs_change <- abs(abs_change)

# -------------------------
# 0–1 NORMALISATION FUNCTION
# -------------------------
norm01 <- function(x) {
  xmin <- global(x, "min", na.rm = TRUE)[1,1]
  xmax <- global(x, "max", na.rm = TRUE)[1,1]
  (x - xmin) / (xmax - xmin)
}

# -------------------------
# NORMALISE ALL SURFACES
# -------------------------
mag_res     <- norm01(mag_res)
growth_res  <- norm01(growth_res)
decline_res <- norm01(decline_res)
abs_change  <- norm01(abs_change)

# -------------------------
# SERAPHIM SCALING FUNCTION
# -------------------------
seraphim_k_scale <- function(r, k) {
  vmax <- global(r, "max", na.rm = TRUE)[1,1]
  r_norm <- r / vmax
  vt <- 1 + k * r_norm
  return(vt)
}

k_values <- c(10, 100, 1000)

# -------------------------
# OUTPUT DIRECTORY
# -------------------------
output_dir <- here("analysis","seraphim","resistance_rasters","grid_1kmbuffer/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------
# EXPORT SERAPHIM RASTERS
# -------------------------
for (k in k_values) {
  
  mag_k     <- seraphim_k_scale(mag_res, k)
  growth_k  <- seraphim_k_scale(growth_res, k)
  decline_k <- seraphim_k_scale(decline_res, k)
  abs_k     <- seraphim_k_scale(abs_change, k)
  
  writeRaster(mag_k,
              file.path(output_dir, paste0("pop_mag_resistance_k", k, ".asc")),
              overwrite = TRUE, NAflag = -9999)
  
  writeRaster(growth_k,
              file.path(output_dir, paste0("pop_growth_resistance_k", k, ".asc")),
              overwrite = TRUE, NAflag = -9999)
  
  writeRaster(decline_k,
              file.path(output_dir, paste0("pop_decline_resistance_k", k, ".asc")),
              overwrite = TRUE, NAflag = -9999)
  
  writeRaster(abs_k,
              file.path(output_dir, paste0("pop_abschange_resistance_k", k, ".asc")),
              overwrite = TRUE, NAflag = -9999)
}
