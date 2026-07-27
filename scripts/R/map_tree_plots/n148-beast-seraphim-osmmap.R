# ==============================================================================
# Title: n148-beast-seraphim-osmmap.R
# Description: Skygrid plot for Arequipa city analysis (ggplot version)
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))
# =========================
# LOAD DATA
# =========================
mcc_tre <- readAnnotatedNexus(
  here("analysis", "BEAST_runs", "continuous-trait-runs", "n148", "peru-148-rrw.hipstr.tre")
)

mostRecentSamplingDatum <- 2025.18

mcc_tab <- read.csv(
  here("analysis", "BEAST_runs", "continuous-trait-runs", "n148", "peru-148-rrw.extracted.csv")
)

# =========================
# LOCAL TIME RANGE
# =========================
plot_min <- min(mcc_tab$startYear, mcc_tab$endYear, na.rm = TRUE)
plot_max <- max(mcc_tab$startYear, mcc_tab$endYear, na.rm = TRUE)

# =========================
# HPD POLYGONS
# =========================

localTreesDirectory <- here("analysis","BEAST_runs","continuous-trait-runs","n148", "extracted_trees")

polygons <- suppressWarnings(
  spreadGraphic2(
    localTreesDirectory,
    nberOfExtractionFiles = 100,
    prob = 0.80,
    startDatum = 2002,
    precision = 1
  )
)

# =========================
# COLOUR SCALE (LOCAL)
# =========================

# In line with global country palette (phylogeny-country-cols.rds)
colour_scale <- colorRampPalette(
  c("#3131CC", "#4F9EEB", "#469395",
    "#B4CA85", "#F9D461", "#C947E5")
)(100)

# =========================
# TIME → COLOUR FUNCTION (LOCAL)
# =========================
time_to_col <- function(year, minYear, maxYear, palette) {
  year <- pmax(minYear, pmin(maxYear, year))
  t <- (year - minYear) / (maxYear - minYear)
  idx <- floor(t * (length(palette) - 1)) + 1
  palette[idx]
}

# =========================
# MCC POINT COLOURS
# =========================
endYears_colours <- time_to_col(
  mcc_tab$endYear,
  plot_min,
  plot_max,
  colour_scale
)

# FIXED ROOT COLOUR
root_col <- time_to_col(
  mcc_tab$startYear[1],
  plot_min,
  plot_max,
  colour_scale
)

# =========================
# HPD POLYGON COLOURS (FIXED)
# =========================
polygons_colours <- rep(NA, length(polygons))

for (i in seq_along(polygons)) {
  date <- as.numeric(names(polygons[[i]]))
  polygons_colours[i] <- paste0(
    time_to_col(date, plot_min, plot_max, colour_scale),
    "40"
  )
}

# =========================
# SPATIAL DATA
# =========================

# This still has some non-aqp date, filter out
arequipa_city <- st_read(here("processed_data","gis_data","arequipa-city.shp"))  %>% filter(NAME_1 %in% c("Arequipa"))

chili_river <- readRDS(here("processed_data","gis_data","chili_river.rds"))

# raster used to define landscape used to define map extent
template_raster <- rast(here("analysis","seraphim","aqpCity_200m_1kmbuffer.asc"))
ext <- ext(template_raster)

## highlight important hubs
highlight_districts <- c("Cerro Colorado")
districts_hi <- arequipa_city %>%
  filter(NAME_3 %in% highlight_districts)


# =========================
# BASEMAP
# =========================
osm_tiles <- get_tiles(
  x = vect(ext, crs = "EPSG:4326"),
  provider = "OpenStreetMap",
  zoom = 14
)

# =========================
# OUTPUT
# =========================
# png("results/figures/n148-rrw-beast-map-osm.png",
#     width = 180, height = 140, units = "mm", res = 600)
# 
# cairo_pdf("results/figures/n148-rrw-beast-map-osm.pdf",
#           width = 180/25.4,
#           height = 140/25.4)
 
par(mar = c(0,0,0,0),
    oma = c(1.2,3.5,1,0),
    mgp = c(0,0.4,0),
    lwd = 0.2,
    bty = "o")

# =========================
# BASEMAP
# =========================
plotRGB(osm_tiles, axes = FALSE)

# =========================
# BORDERS
# =========================
plot(st_geometry(arequipa_city),
     border = "grey30",
     col = NA,
     add = TRUE, lwd = 0.5,lty=2)

plot(st_geometry(districts_hi),
     border = "grey40",
     lwd = 1.2,
     lty=2,
     col = adjustcolor("lightgrey", alpha.f = 0.3),
     add = TRUE)
centroids <- st_centroid(districts_hi)
coords <- st_coordinates(centroids)
text(coords[,1], coords[,2],
     labels = districts_hi$NAME_3,
     cex = 0.8,
     font = 2,
     col = "grey40")
# =========================
# RIVERS
# =========================
plot(st_geometry(chili_river),
     col = "#4FC3F7",
     lwd = 1.2,
     add = TRUE)

# =========================
# MCC ARROWS
# =========================
for (i in 1:nrow(mcc_tab)) {
  curvedarrow(
    cbind(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"]),
    cbind(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"]),
    arr.length = 0,
    arr.width = 0,
    lwd = 0.8,
    lcol = "black",
    arr.col = NA,
    arr.pos = FALSE,
    curve = 0.1,
    endhead = FALSE
  )
}

# =========================
# POINTS
# =========================
for (i in nrow(mcc_tab):1) {
  
  if (i == 1) {
    
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"],
           pch = 16, col = root_col, cex = 1)
    
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"],
           pch = 1, col = "gray10", cex = 1)
  }
  
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"],
         pch = 16, col = endYears_colours[i], cex = 1)
  
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"],
         pch = 1, col = "gray10", cex = 1)
}

# =========================
# LEGEND (LOCAL SCALE)
# =========================
rast <- raster(matrix(nrow = 1, ncol = 2))
rast[1] <- plot_min
rast[2] <- plot_max
#plotRGB(osm_tiles, axes = FALSE)
plot(rast,
     legend.only = TRUE,
     col = colour_scale,
     smallplot = c(0.25, 0.70, 0.06, 0.08),
     horizontal = TRUE,
     axis.args = list(
       at = seq(floor(plot_min), ceiling(plot_max), 1),
       labels = seq(floor(plot_min), ceiling(plot_max), 1),
       tck = -0.015,
       cex.axis = 1,
       hadj = 0.5,mgp = c(2, 0.2, 0)
     ))

# =========================
# SCALE BAR
# =========================
map.scale(
  x = xmin(ext) + 0.02,
  y = ymin(ext) + 0.03,
  relwidth = 0.1,
  metric = TRUE,
  ratio = FALSE,
  cex = 1,
  lwd = 15
)

# dev.off()

# record plot stores this for use later
plot_arequipa_map <- recordPlot()
