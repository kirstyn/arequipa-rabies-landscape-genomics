# =========================
# LIBRARIES
# =========================
library(ape)
library(diagram)
library(fields)
library(gdistance)
library(sf)
library(terra)
library(phytools)
library(raster)
library(vegan)
library(RColorBrewer)
library(seraphim)
library(prettymapr)
library(maptiles)
library(osmdata)
library(dplyr)

# =========================
# LOAD DATA
# =========================
mcc_tre <- readAnnotatedNexus(
  "analysis/BEAST_runs/continuous-trait-runs/n148/peru-148-rrw.hipstr.tre"
)

mostRecentSamplingDatum <- 2025.18

mcc_tab <- read.csv(
  "analysis/BEAST_runs/continuous-trait-runs/n148/peru-148-rrw.extracted.csv"
)

# =========================
# OPTIONAL SAFETY FILTER (recommended)
# =========================
# mcc_tab <- mcc_tab %>%
#   filter(endYear >= 2002 & endYear <= mostRecentSamplingDatum)

# =========================
# HPD POLYGONS
# =========================
localTreesDirectory <- "/Users/kirstyn.brunker/GitHub/RABV_Arequipa_2025/analysis/BEAST_runs/continuous-trait-runs/n148/extracted_trees"

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
# GLOBAL TIME RANGE (LOCKED)
# =========================
minYear <- 2002
maxYear <- mostRecentSamplingDatum

# =========================
# COLOUR SCALE (MASTER)
# =========================
colour_scale <- colorRampPalette(
  c("#3131CC", "#4F9EEB", "#469395",
    "#B4CA85", "#F9D461", "#EE3838")
)(100)

# =========================
# SAFE TIME → COLOUR FUNCTION
# =========================
time_to_col <- function(year, minYear, maxYear, palette) {
  
  # clamp values (critical fix)
  year <- pmax(minYear, pmin(maxYear, year))
  
  t <- (year - minYear) / (maxYear - minYear)
  
  idx <- floor(t * (length(palette) - 1)) + 1
  
  idx <- pmax(1, pmin(length(palette), idx))
  
  palette[idx]
}

# =========================
# MCC POINT COLOURS
# =========================
endYears_colours <- time_to_col(
  mcc_tab$endYear,
  minYear,
  maxYear,
  colour_scale
)

# =========================
# HPD POLYGON COLOURS
# =========================
polygons_colours <- rep(NA, length(polygons))

for (i in seq_along(polygons)) {
  
  date <- as.numeric(names(polygons[[i]]))
  
  polygons_colours[i] <- paste0(
    time_to_col(date, minYear, maxYear, colour_scale),
    "40"  # transparency
  )
}

# =========================
# SPATIAL DATA
# =========================
borders <- st_read(
  "raw_data/spatial/shapefiles/Peru Shapefiles/DISTRITOS_inei_geogpsperu_suyopomalia/DISTRITOS_inei_geogpsperu_suyopomalia.shp"
)

borders_region <- borders %>%
  filter(NOMBDEP %in% c("AREQUIPA", "PUNO")) %>%
  group_by(NOMBDEP) %>%
  summarise(geometry = st_union(geometry))

template_raster <- rast("analysis/seraphim/aqpCity_200m_1kmbuffer.asc")
ext <- ext(template_raster)

# =========================
# BASEMAP
# =========================
osm_light <- get_tiles(
  x = vect(ext, crs = "EPSG:4326"),
  provider = "Esri.WorldGrayCanvas",
  zoom = 12
)

# =========================
# OUTPUT
# =========================
png("results/figures/n148-rrw-beast-map.png",
    width = 180, height = 140, units = "mm", res = 600)

cairo_pdf("results/figures/n148-rrw-beast-map.pdf",
          width = 180/25.4,
          height = 140/25.4)

par(mar = c(0,0,0,0),
    oma = c(1.2,3.5,1,0),
    mgp = c(0,0.4,0),
    lwd = 0.2,
    bty = "o")

# =========================
# BASEMAP
# =========================
plotRGB(osm_light, axes = FALSE)

# =========================
# BORDERS
# =========================
plot(st_geometry(borders_region),
     border = "gray30", lwd = 0.6, add = TRUE)

# =========================
# HPD POLYGONS
# =========================
for (i in seq_along(polygons)) {
  plot(polygons[[i]],
       col = polygons_colours[i],
       border = NA,
       add = TRUE)
}

# =========================
# MCC ARROWS
# =========================
for (i in 1:nrow(mcc_tab)) {
  curvedarrow(
    cbind(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"]),
    cbind(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"]),
    arr.length = 0,
    arr.width = 0,
    lwd = 0.2,
    lcol = "gray10",
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
    
    root_col <- time_to_col(
      
      mcc_tab[i,"startYear"],
      
      minYear,
      
      maxYear,
      
      colour_scale
      
    )
    
    
    
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"],
           
           pch = 16, col = root_col, cex = 0.8)
    
    
    
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"],
           
           pch = 1, col = "gray10", cex = 0.8)
    
  }
  
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"],
         pch = 16, col = endYears_colours[i], cex = 0.8)
  
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"],
         pch = 1, col = "gray10", cex = 0.8)
}

# =========================
# AXES
# =========================
# axis(1,
#      at = pretty(c(ext[1], ext[2]), n = 1),
#      pos = ext[3],
#      col.axis = "gray30", cex.axis=0.7, lwd=0, lwd.tick=0.2,
#      padj=-0.8, tck=-0.01)
# 
# axis(2,
#      at = pretty(c(ext[3], ext[4]), n = 1),
#      pos = ext[1],
#      col.axis = "gray30",cex.axis=0.7, lwd=0, lwd.tick=0.2,
#      padj=-0.8, tck=-0.01)

# =========================
# LEGEND (MATCHES EXACT SCALE)
# =========================
rast <- raster(matrix(nrow = 1, ncol = 2))
rast[1] <- minYear
rast[2] <- maxYear

# plot(rast,
#      legend.only = TRUE,
#      col = colour_scale,
#      smallplot = c(0.25, 0.70, 0.06, 0.08),
#      horizontal = TRUE,
#      axis.args = list(
#        at = seq(minYear, maxYear, 2),
#        labels = seq(minYear, maxYear, 2),
#        tck = -0.03,
#        cex.axis = 0.9
#      ))

# =========================
# SCALE BAR
# =========================
map.scale(
  x = xmin(ext) + 0.05,
  y = ymin(ext) + 0.02,
  relwidth = 0.2,
  metric = TRUE,
  ratio = FALSE,
  cex = 1,
  lwd = 4
)

dev.off()

