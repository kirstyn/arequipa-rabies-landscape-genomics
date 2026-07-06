library(terra)

# -------------------------
# 0. LOAD DATA
# -------------------------
r <- rast("~/Downloads/19K_20240101-20241231.tif")
base_raster <- rast("analysis/seraphim/aqpCity_200m_1kmbuffer.asc")

# -------------------------
# 1. ALIGN LANDCOVER
# -------------------------

r_proj <- project(r, base_raster)
r_crop <- crop(r_proj, base_raster)
r_align <- resample(r_crop, base_raster, method = "near")
r_align <- round(r_align)

# -------------------------
# 2. CHECK LANDCOVER
# -------------------------
plot(r_align, col = rainbow(11), main = "Aligned landcover")

freq(r_align)

# -------------------------
# 3. DEFINE NULL LANDSCAPE
# -------------------------
# background = 1 everywhere
null_landscape <- rast(r_align)
values(null_landscape) <- 1

# -------------------------
# 4. CROPLAND MASK
# -------------------------
cropland_mask <- r_align == 5

plot(cropland_mask, main = "Cropland mask")

# -------------------------
# 5. K-SCALING FUNCTION (ADDITIVE MODEL)
# -------------------------
k_values <- c(10, 100, 1000)

output_dir <- "analysis/seraphim/resistance_rasters/grid_1kmbuffer/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (k in k_values) {
  
  # start with null landscape = 1
  r_k <- null_landscape
  
  # add k where cropland exists
  r_k[cropland_mask == 1] <- 1 + k
  
  plot(r_k, main = paste("Resistance (1 + k), k =", k))
  
  writeRaster(
    r_k,
    filename = file.path(output_dir,
                         paste0("cropland_resistance_k", k, ".asc")),
    overwrite = TRUE,
    NAflag = -9999,
    filetype = "AAIGrid"
  )
}
