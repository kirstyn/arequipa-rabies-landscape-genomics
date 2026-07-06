library(sf)        # For spatial data
library(dplyr)     # For data manipulation
library(ggplot2)   # For plotting
library(readr)     # For reading CSVs
library(terra)
library(tidyverse)
library(leaflet)
library(stringr)




# Load aqp landscape data
roads <- read_csv2("raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Access_roads_AQP/Roads/Carreteras_AQP_14ene2024.csv") %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

trainlines <- read_csv2("raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Access_roads_AQP/Train_line/Linea_tren_AQP_14ene2024.csv") %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = trainlines, color = "blue", size = 0.3) +
  theme_minimal() +
  labs(title = "Roads in Arequipa")


# Specify the KML file
kml_file <- "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Access_roads_AQP/Roads/Carreteras_AQP_14ene2024.kml"
st_layers(kml_file)
# Read KML using sf
roads_national <- st_read(kml_file, layer = "Nacional")
roads_regional <- st_read(kml_file, layer = "Regional")

# Optionally, add a column to identify the type
roads_national <- roads_national %>% mutate(type = "National")
roads_regional <- roads_regional %>% mutate(type = "Regional")

roads_all <- bind_rows(roads_national, roads_regional)

ggplot(roads_all) +
  geom_sf(aes(color = type), size = 0.5) +
  theme_minimal() +
  labs(title = "Roads in Arequipa", color = "Road Type")

# Load base raster
#base_raster <- rast("analysis/seraphim/aqpRegion_200m_Study_area.asc")
#base_raster <- rast("analysis/seraphim/aqpCity_200m_Study_area_expanded.asc")
base_raster <- rast("analysis/seraphim/aqpCity_200m_1kmbuffer.asc")
# =========================================================
# ROADS SURFACES (CONSISTENT: HIGH = ROAD ASSOCIATION)
# Binary + Distance-decay + k-scaling
# =========================================================

library(terra)
library(sf)

# -------------------------
# 1. sf -> SpatVector
# -------------------------
roads_vect <- vect(roads_all)

# -------------------------
# 2. rasterise roads
# (1 = road, NA = background)
# -------------------------
roads_bin <- rasterize(
  roads_vect,
  base_raster,
  field = 1,
  background = NA
)

# =========================================================
# -----------  A. BINARY SURFACE ---------------------------
# =========================================================

# Convert NA → 0 (non-road)
roads_binary <- classify(roads_bin, cbind(NA, 0))

# Normalise (already 0–1 but keep consistent)
roads_binary <- roads_binary / max(values(roads_binary), na.rm = TRUE)

# -------------------------
# Apply k-scaling
# -------------------------
k_vals <- c(10, 100, 1000)

for (k in k_vals) {
  
  roads_binary_k <- 1 + k * roads_binary
  
  writeRaster(
    roads_binary_k,
    filename = paste0(
      "analysis/seraphim/resistance_rasters/grid_1kmbuffer/roads_binary_k",
      k, ".asc"
    ),
    overwrite = TRUE,
    filetype = "AAIGrid"
  )
}

# =========================================================
# -----------  B. DISTANCE-DECAY SURFACE -------------------
# =========================================================

# -------------------------
# 3. distance TO roads
# -------------------------
road_dist <- distance(roads_bin)

# -------------------------
# 4. convert to "road influence"
# HIGH at roads, decays with distance
# -------------------------
scale_param <- 100  # controls decay (~100–300 good for urban)

roads_decay <- 1 / (1 + (road_dist / scale_param))

# -------------------------
# 5. normalise (0–1)
# -------------------------
roads_decay <- roads_decay / max(values(roads_decay), na.rm = TRUE)

# -------------------------
# 6. plot sanity check
# -------------------------
plot(roads_decay, main = "Road influence (distance-decay)")
plot(roads_vect, add = TRUE, col = "black", lwd = 0.5)

# -------------------------
# 7. apply k-scaling
# vt = 1 + k * v0
# -------------------------
for (k in k_vals) {
  
  roads_decay_k <- 1 + k * roads_decay
  
  plot(
    roads_decay_k,
    main = paste("Road decay surface (k =", k, ")")
  )
  plot(roads_vect, add = TRUE, col = "black", lwd = 0.5)
  
  writeRaster(
    roads_decay_k,
    filename = paste0(
      "analysis/seraphim/resistance_rasters/grid_1kmbuffer/roads_decay_k",
      k, ".asc"
    ),
    overwrite = TRUE,
    filetype = "AAIGrid"
  )
}

# Specify the KML file
kml_file <- "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Water_channels_AQP/Hidrografia_AQP_18ene2024.kml"
st_layers(kml_file)
# Read KML using sf
main_rivers <- st_read(kml_file, layer = "Rios_principales")
water_channels <- st_read(kml_file, layer = "Torrenteras")

# Optionally, add a column to identify the type
main_rivers <- main_rivers %>% mutate(type = "Main rivers")
water_channels <- water_channels %>% mutate(type = "Water channels")


# =========================================================
# WATER CHANNEL SURFACES (CONSISTENT WITH ROADS)
# =========================================================

library(terra)
library(sf)
library(dplyr)

# -------------------------
# 1. LOAD DATA
# -------------------------
kml_file <- "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Water_channels_AQP/Hidrografia_AQP_18ene2024.kml"

water_channels <- st_read(kml_file, layer = "Torrenteras")

# -------------------------
# 3. sf -> SpatVector
# -------------------------
channels_vect <- vect(water_channels)

# -------------------------
# 4. RASTERISE (1 = channel, NA = background)
# -------------------------
channels_bin <- rasterize(
  channels_vect,
  base_raster,
  field = 1,
  background = NA
)

# =========================================================
# ----------- A. BINARY SURFACE ----------------------------
# =========================================================

channels_binary <- classify(channels_bin, cbind(NA, 0))
channels_binary <- channels_binary / max(values(channels_binary), na.rm = TRUE)

# =========================================================
# ----------- B. DISTANCE-DECAY SURFACE --------------------
# =========================================================

# Distance to nearest channel
channel_dist <- distance(channels_bin)

# Convert to "channel influence" (HIGH near channels)
scale_param <- 100   # tune if needed (50–300 depending on scale)

channels_decay <- 1 / (1 + (channel_dist / scale_param))

# Normalise 0–1
channels_decay <- channels_decay / max(values(channels_decay), na.rm = TRUE)

# -------------------------
# sanity check plot
# -------------------------
plot(channels_decay, main = "Water channel influence (distance-decay)")
plot(channels_vect, add = TRUE, col = "blue", lwd = 0.5)

# =========================================================
# ----------- C. K-SCALING (SERAPHIM READY) ---------------
# vt = 1 + k * v0
# =========================================================

k_vals <- c(10, 100, 1000)

for (k in k_vals) {
  
  # ---- binary ----
  channels_binary_k <- 1 + k * channels_binary
  
  writeRaster(
    channels_binary_k,
    filename = paste0(
      "analysis/seraphim/resistance_rasters/grid_1kmbuffer/WaterChannels_binary_k",
      k, ".asc"
    ),
    overwrite = TRUE,
    filetype = "AAIGrid"
  )
  
  # ---- decay ----
  channels_decay_k <- 1 + k * channels_decay
  
  plot(
    channels_decay_k,
    main = paste("Water channel decay (k =", k, ")")
  )
  plot(channels_vect, add = TRUE, col = "blue", lwd = 0.5)
  
  writeRaster(
    channels_decay_k,
    filename = paste0(
      "analysis/seraphim/resistance_rasters/grid_1kmbuffer/WaterChannels_decay_k",
      k, ".asc"
    ),
    overwrite = TRUE,
    filetype = "AAIGrid"
  )
}

# =========================================================
# RIVERS: BINARY + DISTANCE DECAY SURFACES
# CONSISTENT WITH ROADS + WATER CHANNELS
# HIGH VALUES = STRONG RIVER INFLUENCE
# =========================================================

library(terra)
library(sf)

# -------------------------
# 1. RASTERISE RIVERS
# -------------------------
river_bin <- rasterize(
  vect(main_rivers),
  base_raster,
  field = 1,
  background = NA
)

# convert NA → 0 for binary surface
river_binary <- classify(river_bin, cbind(NA, 0))

# normalise (0–1)
river_binary <- river_binary / max(values(river_binary), na.rm = TRUE)

# =========================================================
# 2. DISTANCE-DECAY SURFACE
# =========================================================

river_dist <- distance(river_bin)

# controls decay strength (tune 100–500 for urban scale)
scale_param <- 100

river_decay <- 1 / (1 + (river_dist / scale_param))

# normalise (0–1)
river_decay <- river_decay / max(values(river_decay), na.rm = TRUE)

# -------------------------
# sanity check
# -------------------------
plot(river_decay, main = "River influence (distance decay)")
plot(vect(main_rivers), add = TRUE, col = "blue", lwd = 0.5)

# =========================================================
# 3. K-SCALING (vt = 1 + k * v0)
# =========================================================

k_values <- c(10, 100, 1000)

for (k in k_values) {
  
  # -------------------------
  # BINARY
  # -------------------------
  river_binary_k <- 1 + k * river_binary
  
  writeRaster(
    river_binary_k,
    filename = paste0(
      "analysis/seraphim/resistance_rasters/grid_1kmbuffer/river_binary_k",
      k, ".asc"
    ),
    overwrite = TRUE,
    filetype = "AAIGrid"
  )
  
  # -------------------------
  # DECAY
  # -------------------------
  river_decay_k <- 1 + k * river_decay
  
  plot(
    river_decay_k,
    main = paste("River decay surface (k =", k, ")")
  )
  plot(vect(main_rivers), add = TRUE, col = "blue", lwd = 0.5)
  
  writeRaster(
    river_decay_k,
    filename = paste0(
      "analysis/seraphim/resistance_rasters/grid_1kmbuffer/river_decay_k",
      k, ".asc"
    ),
    overwrite = TRUE,
    filetype = "AAIGrid"
  )
}


# Looking at clusters
files <- list.files("raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Clusters_AQP", pattern = "\\.kml$", full.names = TRUE)

polys <- lapply(files, st_read)
polys <- do.call(rbind, polys)

st_write(polys, "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/AQP_clusters.gpkg")
plot(st_geometry(polys))
     

### ADM boundaries:
# -------------------------
# 1. Read KMLs by level
# -------------------------

# Clusters
cluster_files <- list.files(
  "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Delimitation_AQP/Delimitacion_cluster",
  pattern = "\\.kml$",
  full.names = TRUE
)

clusters <- do.call(
  rbind,
  lapply(cluster_files, st_read)
)

# Districts
district_files <- list.files(
  "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Delimitation_AQP/Delimitacion_distrito",
  pattern = "\\.kml$",
  full.names = TRUE
)

districts <- do.call(
  rbind,
  lapply(district_files, st_read)
)

# Microreds
microred_files <- list.files(
  "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Delimitation_AQP/Delimitacion_microred",
  pattern = "\\.kml$",
  full.names = TRUE
)

microreds <- do.call(
  rbind,
  lapply(microred_files, st_read)
)

# -------------------------
# 2. Fix CRS if needed
# -------------------------
st_crs(clusters)  <- 4326
st_crs(districts) <- 4326
st_crs(microreds) <- 4326

# -------------------------
# 3. Plot together
# -------------------------
ggplot() +
 # geom_sf(data = microreds, fill = NA, colour = "red", linewidth = 0.8) +
  #geom_sf(data = districts, fill = NA, colour = "blue", linewidth = 0.6) +
  geom_sf(data = clusters,  fill = NA, colour = "black", linewidth = 0.3) +
  theme_void()

### Economic incomes:
# Microreds
microred_income_files <- list.files(
  "raw_data/spatial/shapefiles/SPATIAL_DATA_AQP/Economic_income_AQP/Economic_income_distrito/",
  pattern = "\\.kml$",
  full.names = TRUE
)

microreds_income <- do.call(
  rbind,
  lapply(microred_income_files , st_read)
)

ggplot() +
  geom_sf(data = microreds_income,  fill = NA, colour = "black", linewidth = 0.3) +
  theme_void()

# -------------------------
ggplot() +
  # geom_sf(data = microreds, fill = NA, colour = "red", linewidth = 0.8) +
  #geom_sf(data = districts, fill = NA, colour = "blue", linewidth = 0.6) +
  geom_sf(data = clusters,  fill = NA, colour = "black", linewidth = 0.3) +
  geom_sf(data = microreds_income,  fill = NA, colour = "black", linewidth = 0.3) +
  
  theme_void()


# =========================================================
# DEM (ELEVATION) - SERAPHIM CONSISTENT
# SINGLE SURFACE TYPE ONLY
# vt = 1 + k * v0
# =========================================================

library(terra)

# -------------------------
# 1. BASE GRID
# -------------------------
base_raster <- rast("analysis/seraphim/aqpCity_200m_1kmbuffer.asc")

# -------------------------
# 2. LOAD DEM
# -------------------------
dem_raw <- rast("~/Downloads/world-pop/per_elevation_merit103_100m_v1.tif")

# -------------------------
# 3. CROP + RESAMPLE
# -------------------------
dem_crop <- crop(dem_raw, base_raster)

dem <- terra::resample(
  dem_crop,
  base_raster,
  method = "bilinear"
)

# -------------------------
# 4. CLEAN VALUES
# -------------------------
dem[dem < 0] <- 0

# =========================================================
# 5. NORMALISE (0–1)
# =========================================================
dem_min <- global(dem, "min", na.rm = TRUE)[1,1]
dem_max <- global(dem, "max", na.rm = TRUE)[1,1]

dem_norm <- (dem - dem_min) / (dem_max - dem_min)

dem_norm[dem_norm < 0] <- 0
dem_norm[dem_norm > 1] <- 1

# =========================================================
# 6. K-SCALING ONLY (NO TRANSFORMS)
# =========================================================

k_values <- c(10, 100, 1000)

for (k in k_values) {
  
  dem_k <- 1 + k * dem_norm
  
  plot(
    dem_k,
    main = paste("Elevation (k =", k, ")")
  )
  
  writeRaster(
    dem_k,
    filename = paste0(
      "analysis/seraphim/resistance_rasters/grid_1kmbuffer/Elevation_k",
      k, ".asc"
    ),
    overwrite = TRUE,
    filetype = "AAIGrid"
  )
}



library(terra)
library(sf)
library(dplyr)

# -------------------------
# 1. LOAD + CLEAN SES DATA
# -------------------------
ses_all <- readRDS("raw_data/spatial/rds/AQPSESblocks.rds")
ses_all <- st_zm(ses_all)

ses_all <- ses_all %>%
  mutate(
    SES = as.character(SES),
    SES = ifelse(is.na(SES) | SES == "Undef", "F", SES)
  )

# -------------------------
# 2. CONVERT TO ORDINAL SCALE
# -------------------------
# A = most affluent
# F = most deprived

ses_all$SES_num <- as.numeric(factor(
  ses_all$SES,
  levels = c("A","B","C","D","E","F"),
  ordered = TRUE
))

# -------------------------
# 3. DEFINE AFFLUENCE AS RESISTANCE
# -------------------------
# A (affluent) = high resistance
# F (deprived) = low resistance

ses_all$SES_affluence <- max(ses_all$SES_num) + 1 - ses_all$SES_num

# -------------------------
#OR DEFINE DEPRIVATION AS RESISTANCE
# -------------------------
# A (affluent) = low resistance
# F (deprived) = high resistance

ses_all$SES_deprivation <- ses_all$SES_num

# -------------------------
# 4. ALIGN TO STUDY GRID
# -------------------------
  base_raster <- rast("analysis/seraphim/aqpCity_200m_1kmbuffer.asc")

ses_all <- st_transform(ses_all, crs(base_raster))
ses_vect <- vect(ses_all)

ses_raster <- rasterize(
  ses_vect,
  base_raster,
  field = "SES_affluence",
  background = NA
)

ses_raster <- rasterize(
  ses_vect,
  base_raster,
  field = "SES_deprivation",
  background = NA
)

plot(ses_raster, main = "SES affluence (raw resistance proxy)")

# -------------------------
# 5. NORMALISE (0–1 SCALE)
# -------------------------
# ensures comparability across all SERAPHIM layers

ses_min <- global(ses_raster, "min", na.rm = TRUE)[1,1]
ses_max <- global(ses_raster, "max", na.rm = TRUE)[1,1]

ses_norm <- (ses_raster - ses_min) / (ses_max - ses_min)

# handle outside-area cells
ses_norm[is.na(ses_norm)] <- 0

# -------------------------
# 6. SERAPHIM K-SCALING FUNCTION
# -------------------------
# vt = 1 + k * (v0 / vmax)

seraphim_k_scale <- function(r, k) {
  
  vmax <- global(r, "max", na.rm = TRUE)[1,1]
  r_norm <- r / vmax
  
  vt <- 1 + k * r_norm
  
  return(vt)
}

# -------------------------
# 7. APPLY K VALUES
# -------------------------
k_values <- c(10, 100, 1000)

output_dir <- "analysis/seraphim/resistance_rasters/grid_1kmbuffer/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (k in k_values) {
  
  ses_k <- seraphim_k_scale(ses_norm, k)
  
  plot(
    ses_k,
    main = paste("SES affluence resistance (k =", k, ")")
  )
  
  writeRaster(
    ses_k,
    filename = file.path(output_dir, paste0("SES_gradient_highRdeprivation_k", k, ".asc")),
    overwrite = TRUE,
    NAflag = -9999,
    filetype = "AAIGrid"
  )
}

# =========================================================
# SES — AFFLUENCE RESISTANCE (BACKGROUND = NA)
# =========================================================

# -------------------------
# 1. LOAD + CLEAN SES DATA
# -------------------------
ses_all <- readRDS("raw_data/spatial/rds/AQPSESblocks.rds")
ses_all <- st_zm(ses_all)

ses_all <- ses_all %>%
  mutate(
    SES = as.character(SES),
    SES = ifelse(is.na(SES) | SES == "Undef", "F", SES)
  )

# -------------------------
# 2. ORDER SES (A = most affluent → F = most deprived)
# -------------------------
ses_all$SES_num <- as.numeric(factor(
  ses_all$SES,
  levels = c("A","B","C","D","E","F"),
  ordered = TRUE
))

# -------------------------
# 3. AFFLUENCE = RESISTANCE
# -------------------------
# A = high resistance, F = low resistance
ses_all$SES_affluence <- max(ses_all$SES_num) + 1 - ses_all$SES_num
ses_all$SES_deprivation <- ses_all$SES_num
# -------------------------
# 4. ALIGN TO GRID
# -------------------------
base_raster <- rast("analysis/seraphim/aqpCity_200m_1kmbuffer.asc")

ses_all <- st_transform(ses_all, crs(base_raster))
ses_vect <- vect(ses_all)

# -------------------------
# 5. RASTERISE (IMPORTANT: keep NA outside polygons)
# -------------------------
ses_raster <- rasterize(
  ses_vect,
  base_raster,
  field = "SES_affluence",
  background = NA   # <<< KEY: true background stays NA
)
ses_raster <- rasterize(
  ses_vect,
  base_raster,
  field = "SES_deprivation",
  background = NA   # <<< KEY: true background stays NA
)

plot(ses_raster, main = "SES affluence (raw, NA background)")

# -------------------------
# 6. SERAPHIM SCALING FUNCTION
# vt = 1 + k * (v0 / vmax)
# ONLY uses non-NA cells
# -------------------------
seraphim_k_scale <- function(r, k) {
  
  vmax <- global(r, "max", na.rm = TRUE)[1,1]
  
  r_norm <- r / vmax
  
  vt <- 1 + k * r_norm
  
  return(vt)
}

# -------------------------
# 7. APPLY K SCALING (KEEP NA OUTSIDE INTACT)
# -------------------------
k_values <- c(10, 100, 1000)

output_dir <- "analysis/seraphim/resistance_rasters/grid_1kmbuffer/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (k in k_values) {
  
  ses_k <- seraphim_k_scale(ses_raster, k)
  
  # re-apply NA mask so background stays background
  ses_k[is.na(ses_raster)] <- NA
  
  plot(
    ses_k,
    main = paste("SES affluence resistance (k =", k, ")")
  )
  
  writeRaster(
    ses_k,
    filename = file.path(output_dir, paste0("SES_gradient_highRdeprivation_bkgNA_k", k, ".asc")),
    overwrite = TRUE,
    NAflag = -9999,
    filetype = "AAIGrid"
  )
}

# =========================================================
# SES RESISTANCE SURFACE WITH EXPLICIT BACKGROUND TREATMENT
# =========================================================
# -------------------------
# 1. LOAD + CLEAN SES DATA
# -------------------------
ses_all <- readRDS("raw_data/spatial/rds/AQPSESblocks.rds")
ses_all <- st_zm(ses_all)

ses_all <- ses_all %>%
  mutate(
    SES = as.character(SES),
    SES = ifelse(is.na(SES) | SES == "Undef", "F", SES)
  )

# -------------------------
# 2. ORDER SES (A = most affluent → F = most deprived)
# -------------------------
ses_all$SES_num <- as.numeric(factor(
  ses_all$SES,
  levels = c("A","B","C","D","E","F"),
  ordered = TRUE
))

# -------------------------
# 3. AFFLUENCE = RESISTANCE
# -------------------------
# A = high resistance, F = low resistance
ses_all$SES_affluence <- max(ses_all$SES_num) + 1 - ses_all$SES_num

# -------------------------
# 4. ALIGN TO STUDY GRID
# -------------------------
base_raster <- rast("analysis/seraphim/aqpCity_200m_1kmbuffer.asc")

ses_all <- st_transform(ses_all, crs(base_raster))
ses_vect <- vect(ses_all)

# -------------------------
# 5. RASTERISE (KEEP TRUE BACKGROUND AS NA)
# -------------------------
ses_raster <- rasterize(
  ses_vect,
  base_raster,
  field = "SES_affluence",
  background = NA
)

plot(ses_raster, main = "SES affluence (raw, NA background)")

# -------------------------
# 6. DEFINE BACKGROUND PENALTY
# -------------------------
# Treat outside SES coverage as WORSE than F (highest resistance)

max_ses <- global(ses_raster, "max", na.rm = TRUE)[1,1]
background_penalty <- max_ses + 1

ses_raster[is.na(ses_raster)] <- background_penalty

# -------------------------
# 7. SERAPHIM SCALING FUNCTION
# vt = 1 + k * (v / vmax)
# -------------------------
seraphim_k_scale <- function(r, k) {
  
  vmax <- global(r, "max", na.rm = TRUE)[1,1]
  
  r_norm <- r / vmax
  
  vt <- 1 + k * r_norm
  
  return(vt)
}

# -------------------------
# 8. APPLY K SCALING
# -------------------------
k_values <- c(10, 100, 1000)

output_dir <- "analysis/seraphim/resistance_rasters/grid_1kmbuffer/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (k in k_values) {
  
  ses_k <- seraphim_k_scale(ses_raster, k)
  
  plot(
    ses_k,
    main = paste("SES resistance (k =", k, ")")
  )
  
  writeRaster(
    ses_k,
    filename = file.path(output_dir,
                         paste0("SES_affluence_resistance_bkgPenalty_k", k, ".asc")),
    overwrite = TRUE,
    NAflag = -9999,
    filetype = "AAIGrid"
  )
}
# =========================================================
# INHABITED AREAS — BINARY RESISTANCE (1 vs 1 + k)
# =========================================================

library(terra)
library(sf)
library(dplyr)

# -------------------------
# 1. CLEAN INPUT DATA
# -------------------------
ses_all <- ses_all %>%
  mutate(
    SES = as.character(SES),
    SES = ifelse(is.na(SES) | SES == "Undef", "F", SES)
  )

# -------------------------
# 2. ALIGN TO GRID
# -------------------------
ses_all <- st_transform(ses_all, crs(base_raster))
ses_vect <- vect(ses_all)

# -------------------------
# 3. CREATE INHABITED MASK
# -------------------------
inhabited <- rasterize(
  ses_vect,
  base_raster,
  field = 1,
  background = NA
)

plot(inhabited, main = "Inhabited mask")

# -------------------------
# 4. BINARY BASE SURFACE
# -------------------------
# 1 = everywhere baseline movement cost
binary_base <- base_raster
binary_base[] <- 1

# -------------------------
# 5. APPLY INHABITED PENALTY
# -------------------------
# inhabited cells get (1 + k)
apply_k <- function(mask, k) {
  
  r <- binary_base
  
  r[!is.na(mask)] <- 1 + k   # inhabited
  r[is.na(mask)]  <- 1       # background stays baseline
  
  return(r)
}

# -------------------------
# 6. RUN K SCENARIOS
# -------------------------
k_values <- c(10, 100, 1000)

output_dir <- "analysis/seraphim/resistance_rasters/grid_1kmbuffer/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (k in k_values) {
  
  r_k <- apply_k(inhabited, k)
  
  plot(
    r_k,
    main = paste("Inhabited resistance (1 + k), k =", k)
  )
  
  writeRaster(
    r_k,
    filename = file.path(output_dir, paste0("SES_inhabited_binary_k", k, ".asc")),
    overwrite = TRUE,
    NAflag = -9999,
    filetype = "AAIGrid"
  )
}

# =========================================================
# SES — AFFLUENT vs DEPRIVED (FIXED + NA BACKGROUND)
# =========================================================


# -------------------------
# 1. LOAD + CLEAN DATA
# -------------------------
ses_all <- readRDS("raw_data/spatial/rds/AQPSESblocks.rds")
ses_all <- st_zm(ses_all)

ses_all <- ses_all %>%
  mutate(
    SES = as.character(SES),
    SES = ifelse(is.na(SES) | SES == "Undef", "F", SES)
  )

# -------------------------
# 2. DEFINE CLASS
# -------------------------
# A–C = affluent (2 = high resistance)
# D–F = deprived (1 = low resistance)

ses_all$SES_class <- ifelse(
  ses_all$SES %in% c("A", "B", "C"),
  2,
  1
)
ses_all$SES_class <- ifelse(
  ses_all$SES %in% c("A", "B", "C"),
  1,
  2
)
# -------------------------
# 3. ALIGN TO GRID
# -------------------------
base_raster <- rast("analysis/seraphim/aqpCity_200m_1kmbuffer.asc")

ses_all <- st_transform(ses_all, crs(base_raster))

# IMPORTANT: convert AFTER creating SES_class
ses_vect <- vect(ses_all)

# -------------------------
# 4. RASTERISE (KEEP TRUE BACKGROUND AS NA)
# -------------------------
ses_class_rast <- rasterize(
  ses_vect,
  base_raster,
  field = "SES_class",
  background = NA
)

plot(ses_class_rast, main = "SES class (1 = deprived, 2 = affluent)")

# -------------------------
# 5. OPTIONAL: RESISTANCE CONVERSION (BASE SURFACE)
# -------------------------
# make explicit resistance interpretation
# deprived = 1, affluent = 2 already encoded

ses_resistance <- ses_class_rast

# -------------------------
# 6. SERAPHIM K-SCALING FUNCTION
# -------------------------
seraphim_k_scale <- function(r, k) {
  vmax <- global(r, "max", na.rm = TRUE)[1,1]
  r_norm <- r / vmax
  1 + k * r_norm
}

# -------------------------
# 7. APPLY K VALUES
# -------------------------
k_values <- c(10, 100, 1000)

output_dir <- "analysis/seraphim/resistance_rasters/grid_1kmbuffer/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (k in k_values) {
  
  r_k <- seraphim_k_scale(ses_resistance, k)
  
  # keep true background as NA
  r_k[is.na(ses_class_rast)] <- NA
  
  plot(
    r_k,
    main = paste("SES affluence resistance (A–C vs D–F), k =", k)
  )
  
  writeRaster(
    r_k,
    filename = file.path(output_dir,
                         paste0("SES_deprivedVaffluent_bkgNA_k", k, ".asc")),
    overwrite = TRUE,
    NAflag = -9999,
    filetype = "AAIGrid"
  )
}
