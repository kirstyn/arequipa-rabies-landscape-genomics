# -----------------------------
# 1. Libraries
# -----------------------------
library(sf)
library(terra)
library(diagram)

# -----------------------------
# 2. Load spatial data
# -----------------------------
base_raster <- rast("analysis/seraphim/aqpCity_Study_area.asc")
big_raster  <- rast("analysis/seraphim/aqpRegion_Study_area.asc")
big_raster  <- rast("analysis/seraphim/n167_Study_area.asc") 
borders <- readRDS("processed_data/gis_data/aqp_district_latest.rds")
borders <- st_read("raw_data/spatial/shapefiles/Peru Shapefiles/DISTRITOS_inei_geogpsperu_suyopomalia/DISTRITOS_inei_geogpsperu_suyopomalia.shp")
borders_simplified <- st_simplify(borders, dTolerance = 0.01)

# -----------------------------
# 3. Load phylogeographic data
# -----------------------------
# mcc_tab <- read.csv(
#   "analysis/BEAST_runs/lognormalRRW/aqpOnly_lognormalRRW.hipstr.csv",
#   stringsAsFactors = FALSE
# )
mcc_tab <- read.csv(
  "analysis/BEAST_runs/continuous-trait-runs/n148/peru-148-rrw.extracted.csv",
  stringsAsFactors = FALSE
)
mcc_tab <- mcc_tab[mcc_tab$endYear >= 2012, ]
polygon_colour <- "#C0C0C080"  # HPD polygon colour

# -----------------------------
# 4. Time colour scale (2011+ only)
# -----------------------------
time_min <- 2011
time_max <- max(c(mcc_tab$startYear, mcc_tab$endYear), na.rm = TRUE)

time_cols <- colorRampPalette(c("#2c7bb6", "#abd9e9", "#ffffbf",
                                "#fdae61", "#d7191c"))(100)

time_to_col <- function(year){
  y <- pmax(year, time_min)
  idx <- round((y - time_min) / (time_max - time_min) * 99) + 1
  time_cols[pmin(pmax(idx, 1), 100)]
}

# -----------------------------
# 5. Compute distances
# -----------------------------
start_sf <- st_as_sf(mcc_tab, coords = c("startLon","startLat"), crs = 4326)
end_sf   <- st_as_sf(mcc_tab, coords = c("endLon","endLat"), crs = 4326)
target_crs <- st_crs(base_raster)
start_sf <- st_transform(start_sf, target_crs)
end_sf   <- st_transform(end_sf, target_crs)
mcc_tab$dist_km <- as.numeric(st_distance(start_sf, end_sf, by_element = TRUE))/1000

# -----------------------------
# 6. Identify long-distance jumps
# -----------------------------
# this ignores internal branch transitions and sample 1203037_2012_2012 jump (puno associated outside of Aqp focus)
big_jump_threshold <- quantile(mcc_tab$dist_km, 0.99, na.rm = TRUE)

mcc_tab$is_big_jump <- (
  mcc_tab$dist_km >= big_jump_threshold &
    !is.na(mcc_tab$tipLabel) &
    mcc_tab$tipLabel != "1203037_2012_2012-05-08"
)

#mcc_tab$is_big_jump =FALSE
# -----------------------------
# 7. Define time windows (last window extends to time_max)
# -----------------------------
window_width <- 2

window_starts <- seq(2012, 2021, by = window_width)
window_ends   <- window_starts + window_width

# extend final window to max time
window_starts <- c(window_starts, 2023)
window_ends   <- c(window_ends, time_max)

n_windows <- length(window_starts)


# -----------------------------
# 8. Polygon times
# -----------------------------
startDatum <- 1995
precision <- 1
polygon_times <- sapply(seq_along(polygons), function(i){
  yrs <- as.numeric(names(polygons[[i]]))
  if(all(is.na(yrs))) startDatum + (i-1)*precision else yrs[1]
})

# -----------------------------
# 9. Branch plotting function
# -----------------------------
plot_branches <- function(df, lwd, alpha=0.12,
                          curve_base=0.05, curve_mid=0.20, curve_far=0.1,
                          ws=NULL, we=NULL){
  
  if(nrow(df) == 0) return(NULL)
  
  start_xy <- cbind(df$startLon, df$startLat)
  end_xy   <- cbind(df$endLon, df$endLat)
  valid <- complete.cases(start_xy, end_xy, df$startYear, df$endYear)
  start_xy <- start_xy[valid,,drop=FALSE]
  end_xy   <- end_xy[valid,,drop=FALSE]
  df <- df[valid,]
  if(nrow(df) == 0) return(NULL)
  
  d <- sqrt((end_xy[,1]-start_xy[,1])^2 + (end_xy[,2]-start_xy[,2])^2)
  q <- quantile(d, probs=c(0.25,0.85), na.rm=TRUE)
  curve_vals <- numeric(length(d))
  curve_vals[d <= q[1]] <- curve_base
  curve_vals[d > q[1] & d <= q[2]] <- curve_mid
  curve_vals[d > q[2]] <- curve_far
  curve_vals <- -curve_vals
  
  branch_col <- adjustcolor("#001f4d", alpha.f=alpha)
  
  for(i in seq_len(nrow(start_xy))){
    curvedarrow(start_xy[i,], end_xy[i,],
                arr.length = 0,   # disable arrowhead
                arr.width  = 0,   # disable arrowhead
                arr.col    = NA,  # disable arrowhead color
                lwd = lwd,
                curve = curve_vals[i],
                lcol = branch_col,
                arr.type="none")
  }
  
  # nodes at branch ends in window
  if(!is.null(ws) & !is.null(we)){
    eps <- 1e-6
    active_idx <- df$endYear + eps >= ws & df$endYear - eps < we
    if(any(active_idx)){
      # node_cols <- time_to_col(df$endYear[active_idx])
      # points(end_xy[active_idx,1], end_xy[active_idx,2], pch=21, cex=1,
      #        bg=node_cols, col="#001f4d", lwd=0.6)
      points(end_xy[active_idx,1], end_xy[active_idx,2], pch=21, cex=1,
             bg="#001f4d", col="#001f4d", lwd=0.6)
    }
  }
}

# -----------------------------
# 10. Save PDF (4x2 layout, arrows in last window)
# -----------------------------
# pdf("analysis/BEAST_runs/lognormalRRW/aqp_phylogeography_4x2_finalwindow_points.pdf",
#     width=14, height=8.5)
pdf("analysis/BEAST_runs/continuous-trait-runs/n166/n166-rrw-ucld-constant.temporal-plots-2012_test.pdf",
    width=14, height=8.5)

n_cols <- 4
n_rows <- 2
par(mfrow=c(n_rows, n_cols), mar=c(0.5,0.5,1.5,0.5), oma=c(4,4,2,2))


for(w in seq_len(n_windows)){
  
  ws <- window_starts[w]
  we <- window_ends[w]
  
  # background map
  arrival_branches <- mcc_tab[
    mcc_tab$endYear >= ws & mcc_tab$endYear < we,
  ]
  
  big_jump_in_window <- any(arrival_branches$is_big_jump)
  
  if(big_jump_in_window){
    plot(big_raster, col="white", box=FALSE, axes=FALSE, colNA="grey90", legend=FALSE)
  } else {
    plot(base_raster, col="white", box=FALSE, axes=FALSE, colNA="grey90", legend=FALSE)
  }
  
  plot(st_geometry(borders), add=TRUE, border="gray20", lwd=0.2)
  
  # polygons
  poly_idx <- which(polygon_times >= ws & polygon_times < we)
  if(length(poly_idx) > 0){
    for(i in poly_idx){
      plot(polygons[[i]], add=TRUE, col=polygon_colour, border=NA)
    }
  }
  
  # ARRIVAL-BASED BRANCHES
  arrival_branches <- mcc_tab[
    mcc_tab$endYear >= ws & mcc_tab$endYear < we,
  ]
  
  plot_branches(arrival_branches, lwd=0.9, alpha=1, ws=ws, we=we)
  
  title(main=paste0(ws,"–",we), cex.main=1.8)
  
  # arrows in last panel
  if(w == n_windows){
    
    usr <- par("usr")
    lx <- (usr[1] + usr[2]) / 2
    ly <- usr[3] + 0.05 * (usr[4] - usr[3])
    dx <- 0.04 * (usr[2] - usr[1])
    
    s <- 3
    
    curvedarrow(c(lx - dx, ly), c(lx + dx, ly),
                arr.length = 0.1 * s,
                arr.width  = 0.08 * s,
                curve      = -0.15,
                lwd        = 1.2 * s,
                lcol       = "grey30",
                arr.col    = "grey30")
    
    text(lx + dx + 0.02 * (usr[2] - usr[1]),
         ly,
         "West → East",
         adj = 0,
         cex = 1.2 * s,
         col = "grey20")
    
    curvedarrow(c(lx + dx, ly - 0.04 * (usr[4] - usr[3])),
                c(lx - dx, ly - 0.04 * (usr[4] - usr[3])),
                arr.length = 0.1 * s,
                arr.width  = 0.08 * s,
                curve      = -0.15,
                lwd        = 1.2 * s,
                lcol       = "grey30",
                arr.col    = "grey30")
    
    text(lx + dx + 0.02 * (usr[2] - usr[1]),
         ly - 0.04 * (usr[4] - usr[3]),
         "East → West",
         adj = 0,
         cex = 1.2 * s,
         col = "grey20")
  }
}

dev.off()


# without larger extent

pdf("analysis/BEAST_runs/continuous-trait-runs/n166/n166-rrw-ucld-constant.temporal-plots-2012_test_localextent2.pdf",
    width=14, height=8.5)

n_cols <- 4
n_rows <- 2
par(mfrow=c(n_rows, n_cols), mar=c(0.5,0.5,1.5,0.5), oma=c(4,4,2,2))

for(w in seq_len(n_windows)){
  
  ws <- window_starts[w]
  we <- window_ends[w]
  
  # background map (always base_raster)
  plot(base_raster, col="white", box=FALSE, axes=FALSE, colNA="grey90", legend=FALSE)
  plotRGB(osm_light, axes = FALSE)
  plot(st_geometry(borders), add=TRUE, border="gray20", lwd=0.2)
  
  # polygons
  poly_idx <- which(polygon_times >= ws & polygon_times < we)
  if(length(poly_idx) > 0){
    for(i in poly_idx){
      plot(polygons[[i]], add=TRUE, col=polygon_colour, border=NA)
    }
  }
  
  # ARRIVAL-BASED BRANCHES
  arrival_branches <- mcc_tab[
    mcc_tab$endYear >= ws & mcc_tab$endYear < we,
  ]
  
  plot_branches(arrival_branches, lwd=0.9, alpha=1, ws=ws, we=we)
  
  title(main=paste0(ws,"–",we), cex.main=1.8)
  
  # arrows in last panel
  if(w == n_windows){
    
    usr <- par("usr")
    lx <- (usr[1] + usr[2]) / 2
    ly <- usr[3] + 0.05 * (usr[4] - usr[3])
    dx <- 0.04 * (usr[2] - usr[1])
    
    s <- 3
    
    curvedarrow(c(lx - dx, ly), c(lx + dx, ly),
                arr.length = 0.1 * s,
                arr.width  = 0.08 * s,
                curve      = -0.15,
                lwd        = 1.2 * s,
                lcol       = "grey30",
                arr.col    = "grey30")
    
    text(lx + dx + 0.02 * (usr[2] - usr[1]),
         ly,
         "West → East",
         adj = 0,
         cex = 1.2 * s,
         col = "grey20")
    
    curvedarrow(c(lx + dx, ly - 0.04 * (usr[4] - usr[3])),
                c(lx - dx, ly - 0.04 * (usr[4] - usr[3])),
                arr.length = 0.1 * s,
                arr.width  = 0.08 * s,
                curve      = -0.15,
                lwd        = 1.2 * s,
                lcol       = "grey30",
                arr.col    = "grey30")
    
    text(lx + dx + 0.02 * (usr[2] - usr[1]),
         ly - 0.04 * (usr[4] - usr[3]),
         "East → West",
         adj = 0,
         cex = 1.2 * s,
         col = "grey20")
  }
}

dev.off()
