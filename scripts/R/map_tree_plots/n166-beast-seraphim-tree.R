# ==============================================================================
# Title: n166-beast-seraphim-tree.R
# Description: Map beast tree from rrw for full peru data, n166 (excludes Bolivia intro)
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

# =========================
# LOAD DATA
# =========================
mcc_tre <- readAnnotatedNexus(
  here("analysis","BEAST_runs","continuous-trait-runs","n166","n166-rrw-ucld-constant.hipstr.tre")
)

mostRecentSamplingDatum <- 2025.18

mcc_tab <- read.csv(
  here("analysis","BEAST_runs","continuous-trait-runs","n166","n166-rrw-ucld-constant.hipstr.csv")
)

# =========================
# TIME RANGE
# =========================
minYear <- floor(min(c(mcc_tab$startYear, mcc_tab$endYear)))
maxYear <- ceiling(max(c(mcc_tab$startYear, mcc_tab$endYear)))


# =========================
# COLOUR SCALE (BLUE → YELLOW, LAC CONSISTENT)
# =========================
colour_scale <- colorRampPalette(
  c("#3131CC", "#4F9EEB", "#469395",
    "#B4CA85", "#F9D461", "#EE3838")
)(100)

# =========================
# UNIVERSAL TIME TO COLOUR FUNCTION
# =========================
time_to_col <- function(year, minYear, maxYear, palette) {
  t <- (year - minYear) / (maxYear - minYear)
  t <- pmax(0, pmin(1, t))  # clamp
  
  idx <- floor(t * (length(palette) - 1)) + 1
  palette[idx]
}

# =========================
# COLOUR MAPPING
# =========================
endYears_colours <- time_to_col(
  mcc_tab$endYear,
  minYear,
  maxYear,
  colour_scale
)

# =========================
# HPD POLYGONS
# =========================
localTreesDirectory <- here("analysis","BEAST_runs","continuous-trait-runs","n166","extracted-trees")

polygons <- suppressWarnings(
  spreadGraphic2(localTreesDirectory,
                 nberOfExtractionFiles = 100,
                 prob = 0.80,
                 startDatum = minYear,
                 precision = 1)
)

# number of polygons
n_poly <- length(polygons)

# reconstruct time sequence
poly_times <- seq(
  from = startDatum,
  by   = precision,
  length.out = n_poly
)

# map colours
polygons_colours <- sapply(poly_times, function(t) {
  paste0(time_to_col(t, minYear, maxYear, colour_scale), "40")
})

# ===================
# SPATIAL DATA
# =========================
borders <- st_read(
  here("raw_data","spatial","shapefiles","Peru_shapefiles","DISTRITOS_inei_geogpsperu_suyopomalia","DISTRITOS_inei_geogpsperu_suyopomalia.shp")
)

borders_region <- borders %>%
  filter(NOMBDEP %in% c("AREQUIPA", "PUNO")) %>%
  group_by(NOMBDEP) %>%
  summarise(geometry = st_union(geometry))

mcc_ext <- ext(
  range(c(mcc_tab$startLon, mcc_tab$endLon), na.rm = TRUE),
  range(c(mcc_tab$startLat, mcc_tab$endLat), na.rm = TRUE)
)

# buffer
buf_x <- 0.2 * (mcc_ext[2] - mcc_ext[1])
buf_y <- 0.2 * (mcc_ext[4] - mcc_ext[3])

mcc_ext <- ext(
  mcc_ext[1] - buf_x,
  mcc_ext[2] + buf_x,
  mcc_ext[3] - buf_y,
  mcc_ext[4] + buf_y
)

# =========================
# BASEMAP
# =========================
osm_light <- get_tiles(
  x = vect(mcc_ext, crs = "EPSG:4326"),
  provider = "Esri.WorldGrayCanvas",
  zoom = 12
)

# =========================
# OUTPUT MAP
# =========================
png(here("figures","n166-beast-map.png"),
    width = 180, height = 140, units = "mm", res = 600)

cairo_pdf(here("figures","n166-rrw-beast-map.pdf"),
          width = 180/25.4,
          height = 140/25.4)

par(mar=c(0,0,0,0))

plotRGB(osm_light, axes = FALSE)

plot(st_geometry(borders_region),
     border = "gray30", lwd = 0.6, add = TRUE)

# HPD
for (i in seq_along(polygons)) {
  plot(polygons[[i]],
       col = polygons_colours[i],
       border = NA,
       add = TRUE)
}

# arrows
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

# points
for (i in nrow(mcc_tab):1) {
  
  if (i == 1) {
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"],
           pch = 16, col = colour_scale[1], cex = 0.8)
  }
  
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"],
         pch = 16, col = endYears_colours[i], cex = 0.8)
}

# =========================
# LEGEND 
# =========================
rast <- raster(matrix(c(minYear, maxYear), nrow=1))

# plot(rast,
#      legend.only = TRUE,
#      add = TRUE,
#      col = colour_scale,
#      smallplot = c(0.25, 0.7, 0.05, 0.08),
#      horizontal = TRUE,
#      axis.args = list(
#        at = seq(minYear, maxYear, 2),
#        labels = seq(minYear, maxYear, 2),
#        cex.axis = 0.8,
#        tck = -0.02
#      ))

# =========================
# SCALE BAR
# =========================
map.scale(
  x = xmin(ext) + 0.9,
  y = ymin(ext) + 0.0,
  relwidth = 0.2,
  metric = TRUE,
  ratio = FALSE,
  cex = 1,
  lwd = 4
)

dev.off()


grad_rast <- raster(matrix(
  seq(minYear, maxYear, length.out = 100),
  nrow = 1
))


cairo_pdf(here("figures","timescale_horizontal.pdf"),
          width = 180/25.4, height = 25/25.4)

par(mar = c(2, 2, 1, 2))  # slightly larger right margin

plot(grad_rast,
     col = colour_scale,
     legend.only = TRUE,
     horizontal = TRUE,
     smallplot = c(0.05, 0.95, 0.4, 0.6),  # ← key fix
     axis.args = list(
       at = seq(minYear, maxYear, 2),
       labels = seq(minYear, maxYear, 2),
       cex.axis = 0.8,
       tck = -0.2
     ))

dev.off()


