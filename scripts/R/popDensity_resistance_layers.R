library(terra)

# -------------------------
# LOAD RASTERS
# -------------------------
r_2025 <- rast("raw_data/spatial/shapefiles/DX_data/per_pop_2025_CN_100m_R2025A_v1.tif")
r_2015 <- rast("raw_data/spatial/shapefiles/DX_data/per_pop_2015_CN_100m_R2025A_v1.tif")

base_raster <- rast("analysis/seraphim/aqpCity_200m_1kmbuffer.asc")

# -------------------------
# ALIGN FUNCTION
# -------------------------
align_to_base <- function(r, base) {
  r_crop <- crop(r, base)
  
  if (!terra::same.crs(r_crop, base)) {
    r_proj <- project(r_crop, base)
  } else {
    r_proj <- r_crop
  }
  
  r_align <- terra::resample(r_proj, base, method = "bilinear")
  return(r_align)
}

# -------------------------
# ALIGN TO BASE GRID
# -------------------------
r2025 <- align_to_base(r_2025, base_raster)
r2015 <- align_to_base(r_2015, base_raster)

# -------------------------
# CLEAN DATA
# -------------------------
r2025[r2025 == 0] <- NA
r2015[r2015 == 0] <- NA

# -------------------------
# NORMALISE FUNCTION
# -------------------------
norm_fun <- function(r) {
  r / global(r, "max", na.rm = TRUE)[1,1]
}

# -------------------------
# POPULATION RESISTANCE (KEY CHOICE)
# high population = HIGH resistance
# -------------------------
r2025_norm <- norm_fun(r2025)
r2015_norm <- norm_fun(r2015)

r2025_res <- r2025_norm
r2015_res <- r2015_norm

# -------------------------
# -------------------------
# POPULATION CHANGE (RESISTANCE)
# higher increase → higher resistance
# -------------------------

r_change <- log1p(r2025) - log1p(r2015)

# normalise to 0–1
r_change_res <- (r_change - global(r_change, "min", na.rm=TRUE)[1,1]) /
  (global(r_change, "max", na.rm=TRUE)[1,1] -
     global(r_change, "min", na.rm=TRUE)[1,1])

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
output_dir <- "analysis/seraphim/resistance_rasters/grid_1kmbuffer/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------
# BUILD SERAPHIM LAYERS
# -------------------------
for (k in k_values) {
  
  # # STATIC POPULATION RESISTANCE
  # r2025_k <- seraphim_k_scale(r2025_res, k)
  # r2015_k <- seraphim_k_scale(r2015_res, k)
  # 
  # TEMPORAL CHANGE RESISTANCE
  rchange_k <- seraphim_k_scale(r_change_res, k)
  
  # -------------------------
  # SAVE OUTPUTS
  # -------------------------
  # writeRaster(r2025_k,
  #             file.path(output_dir, paste0("pop2025_resistance_k", k, ".asc")),
  #             overwrite = TRUE, NAflag = -9999)
  # 
  # writeRaster(r2015_k,
  #             file.path(output_dir, paste0("pop2015_resistance_k", k, ".asc")),
  #             overwrite = TRUE, NAflag = -9999)
  
  writeRaster(rchange_k,
              file.path(output_dir, paste0("pop_change_resistance_k", k, ".asc")),
              overwrite = TRUE, NAflag = -9999)
}


##absolute change 
r_change_abs <- r2025 - r2015

r_min <- global(r_change_abs, "min", na.rm = TRUE)[1,1]
r_max <- global(r_change_abs, "max", na.rm = TRUE)[1,1]

r_change_abs_norm <- (r_change_abs - r_min) / (r_max - r_min)

for (k in k_values) {
  
  # abso change
  rchange_k <- seraphim_k_scale(r_change_abs_norm , k)

  writeRaster(rchange_k,
              file.path(output_dir, paste0("pop_abschange_resistance_k", k, ".asc")),
              overwrite = TRUE, NAflag = -9999)
}


# combined
# -------------------------
# COMBINED CHANGE SURFACE
# -------------------------
r_combined <- (r2025 - r2015) * log1p(r2025)

# -------------------------
# NORMALISE
# -------------------------
r_vmax <- global(r_combined, "max", na.rm = TRUE)[1,1]
r_combined_scaled <- r_combined / r_vmax

# -------------------------
# SERAPHIM SCALING + EXPORT
# -------------------------
for (k in k_values) {
  
  rchange_k <- seraphim_k_scale(r_combined_norm, k)
  
  writeRaster(rchange_k,
              file.path(output_dir, paste0("pop_combinedChange_resistance_k", k, ".asc")),
              overwrite = TRUE, NAflag = -9999)
}

# -------------------------
# LOG-TRANSFORMED CHANGE SURFACE
# -------------------------
r_change <- log1p(r2025) - log1p(r2015)

# OPTIONAL: additional log stabilisation (only if still skewed)
r_change <- log1p(r_change)

# -------------------------
# NORMALISE 0–1
# -------------------------
r_min <- global(r_change, "min", na.rm = TRUE)[1,1]
r_max <- global(r_change, "max", na.rm = TRUE)[1,1]

r_change_res <- (r_change - r_min) / (r_max - r_min)


# relative pop change - alternative standardisation
# 1. signed log change
r_change <- log1p(r2025) - log1p(r2015)

# 2. z-score standardisation
r_change_z <- (r_change - global(r_change, "mean", na.rm=TRUE)[1,1]) /
  global(r_change, "sd", na.rm=TRUE)[1,1]

# 3. convert to positive resistance (preserving direction)
r_change_res <- exp(r_change_z)

for (k in k_values) {
  
  rchange_k <- seraphim_k_scale(r_change_res, k)
  
  writeRaster(
    rchange_k,
    file.path(output_dir, paste0("pop_relchange_zexp_resistance_k", k, ".asc")),
    overwrite = TRUE,
    NAflag = -9999
  )
}


# -------------------------
# Explicitly measure increasing pop versus decreasing as resistance surfaces
# -------------------------
growth_res <- r_change_z
growth_res[growth_res < 0] <- 0
growth_res <- growth_res + abs(min(growth_res, na.rm = TRUE))

decline_res <- r_change_z
decline_res[decline_res > 0] <- 0
decline_res <- abs(decline_res)
decline_res <- decline_res + abs(min(decline_res, na.rm = TRUE))


