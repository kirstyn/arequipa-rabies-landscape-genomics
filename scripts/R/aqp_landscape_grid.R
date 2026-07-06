library(sf)
library(dplyr)
library(ggplot2)
library(terra)

# ----------------------------
# DATA
# ----------------------------
aqp_districts <- readRDS("processed_data/gis_data/aqp_district_latest.rds")

data <- read.csv("processed_data/processed_metadata/081025_epi-seq_n167_stdDistrict.csv")

outside_city <- c("CAMINACA", "ATUNCOLLA", "AZANGARO", "MAJES")
outside_region <- c("CAMINACA", "ATUNCOLLA", "AZANGARO")

aqp_region <- data %>% filter(!district_std %in% outside_region)
aqp_city   <- data %>% filter(!district_std %in% outside_city)

# ----------------------------
# POINTS TO SF
# ----------------------------
region_points_sf <- st_as_sf(aqp_region, coords = c("lon", "lat"), crs = 4326)
city_points_sf   <- st_as_sf(aqp_city, coords = c("lon", "lat"), crs = 4326)

# ----------------------------
# PROJECT TO UTM (metres)
# ----------------------------
region_points_utm <- st_transform(region_points_sf, 32719)
city_points_utm   <- st_transform(city_points_sf, 32719)

# ----------------------------
# BOUNDING BOX + BUFFER
# ----------------------------
buffer <- 1000

region_bbox <- st_bbox(region_points_utm)
city_bbox   <- st_bbox(city_points_utm)

region_bbox_exp <- region_bbox
region_bbox_exp[c("xmin")] <- region_bbox["xmin"] - buffer
region_bbox_exp[c("xmax")] <- region_bbox["xmax"] + buffer
region_bbox_exp[c("ymin")] <- region_bbox["ymin"] - buffer
region_bbox_exp[c("ymax")] <- region_bbox["ymax"] + buffer

city_bbox_exp <- city_bbox
city_bbox_exp[c("xmin")] <- city_bbox["xmin"] - buffer
city_bbox_exp[c("xmax")] <- city_bbox["xmax"] + buffer
city_bbox_exp[c("ymin")] <- city_bbox["ymin"] - buffer
city_bbox_exp[c("ymax")] <- city_bbox["ymax"] + buffer

# ----------------------------
# BACK TO SF FOR PLOTTING BBOX
# ----------------------------
region_bbox_poly <- st_as_sfc(region_bbox_exp) |> st_set_crs(32719) |> st_transform(4326)
city_bbox_poly   <- st_as_sfc(city_bbox_exp)   |> st_set_crs(32719) |> st_transform(4326)

region_final_bbox <- st_bbox(region_bbox_poly)
city_final_bbox   <- st_bbox(city_bbox_poly)

# ----------------------------
# PLOTS (points only)
# ----------------------------
ggplot() +
  geom_sf(data = aqp_districts, fill = "grey90", colour = "grey40") +
  geom_sf(data = region_points_sf, colour = "red", size = 2, alpha = 0.7) +
  coord_sf(
    xlim = c(region_final_bbox["xmin"], region_final_bbox["xmax"]),
    ylim = c(region_final_bbox["ymin"], region_final_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_minimal()

ggplot() +
  geom_sf(data = aqp_districts, fill = "grey90", colour = "grey40") +
  geom_sf(data = city_points_sf, colour = "red", size = 2, alpha = 0.7) +
  coord_sf(
    xlim = c(city_final_bbox["xmin"], city_final_bbox["xmax"]),
    ylim = c(city_final_bbox["ymin"], city_final_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_minimal()

# ----------------------------
# CREATE RASTER GRID FUNCTION
# ----------------------------
make_grid <- function(bbox_expanded, res = 200, crs = "EPSG:32719") {
  
  e <- terra::ext(
    bbox_expanded["xmin"],
    bbox_expanded["xmax"],
    bbox_expanded["ymin"],
    bbox_expanded["ymax"]
  )
  
  r <- terra::rast(ext = e, resolution = res, crs = crs)
  
  # ✅ Correct way to fill with 1
  r <- terra::init(r, 1)
  
  return(r)
}

# ----------------------------
# CREATE GRIDS
# ----------------------------
region_r_utm <- make_grid(region_bbox_exp, 200)
city_r_utm   <- make_grid(city_bbox_exp, 200)

# project to WGS84 only for export if needed
region_r_wgs84 <- project(region_r_utm, "EPSG:4326")
city_r_wgs84   <- project(city_r_utm, "EPSG:4326")
# VERY IMPORTANT: ensure values persist after projection
city_r_wgs84[is.na(city_r_wgs84)] <- 1
# VERY IMPORTANT: ensure values persist after projection
region_r_wgs84[is.na(region_r_wgs84)] <- 1

# ----------------------------
# PLOT GRIDS + POINTS (CORRECT CRS)
# ----------------------------

par(mfrow = c(1, 2))

# ---- REGION ----
plot(region_r_utm, main = "Region grid (UTM)")
points(
  st_coordinates(region_points_utm),
  col = "red",
  pch = 16,
  cex = 0.6
)

# ---- CITY ----
plot(city_r_utm, main = "City grid (UTM)")
points(
  st_coordinates(city_points_utm),
  col = "blue",
  pch = 16,
  cex = 0.6
)

par(mfrow = c(1, 1))
# ----------------------------
# EXPORT
# ----------------------------
writeRaster(region_r_utm,
            "processed_data/gis_data/arequipaRegion_200m_grid.tif",
            overwrite = TRUE)

writeRaster(region_r_wgs84,
            "analysis/seraphim/aqpRegion_200m_Study_area.asc",
            overwrite = TRUE)

writeRaster(city_r_utm,
            "processed_data/gis_data/arequipaCity_200m_grid_1kmbuffer.tif",
            overwrite = TRUE)

writeRaster(city_r_wgs84,
            "analysis/seraphim/aqpCity_200m_1kmbuffer.asc",
            overwrite = TRUE)

# ----------------------------
# BASIC STATS
# ----------------------------
res(region_r_utm)

n_cells <- ncell(region_r_utm)
cell_area_m2 <- prod(res(region_r_utm))
total_area_km2 <- (n_cells * cell_area_m2) / 1e6

total_area_km2
