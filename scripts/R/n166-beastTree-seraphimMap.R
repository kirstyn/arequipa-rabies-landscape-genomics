#install.packages("devtools");install.packages("diagram")
library(devtools)
#install_github("sdellicour/seraphim/unix_OS") # (for a Unix OS)
library(ape)
library(diagram)
library(fields)
library(gdistance)
#library(rgeos)
library(sf)
library(terra)
library(HDInterval)
library(ks)
library(phytools)
library(raster)
library(vegan)
library(RColorBrewer)
library(seraphim)
library(prettymapr)
library(maptiles)
library(terra)


# Extract spatio-temporal info from MCC tree
#mcc_tre = readAnnotatedNexus("analysis/BEAST_runs/Logs/mainIntro_skygrid_rrw/aqp_mainIntro_skygrid_rrw.hipstr.tre")
mcc_tre = readAnnotatedNexus("analysis/BEAST_runs/continuous-trait-runs/n166/n166-rrw-ucld-constant.hipstr.tre")
mostRecentSamplingDatum = 2025.180

#mcc_tab = mccTreeExtractions(mcc_tre, mostRecentSamplingDatum)
#write.csv(mcc_tab, "analysis/BEAST_runs/continuous-trait-runs/n147/arequipa_only_mainIntro_n147_rrw.hipstr.csv", row.names=F, quote=F)
mcc_tab = read.csv("analysis/BEAST_runs/continuous-trait-runs/n166/n166-rrw-ucld-constant.hipstr.csv", head=T)

# Extract spatio-temporal info from posterior trees
localTreesDirectory = "/Users/kirstyn.brunker/GitHub/RABV_Arequipa_2025/analysis/BEAST_runs/continuous-trait-runs/n166/extracted-trees/"
allTrees = scan(file="analysis/BEAST_runs/continuous-trait-runs/n166/n166-rrw-ucld-constant.(time)-subsampled.trees", what="", sep="\n", quiet=T)
burnIn=0 # trees sample of 900 already has burnin discarded
randomSampling= FALSE
nberOfTreesToSample= 100
coordinateAttributeName = "location"

# treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling,nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)


# Estimate HPD region for each time slice
polygons = suppressWarnings(spreadGraphic1(mcc_tre, mcc_tab))
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.80
startDatum = 2002 #lowest limit of 95%HPD interval of root
precision = 12/12 # quarterly

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory,nberOfExtractionFiles, prob, startDatum, precision))


minYear <- 2002
maxYear <- 2025.18

#year_breaks <- seq(minYear, maxYear, length.out = length(colour_scale))
# Setting colour scales
# colour_scale <- colorRampPalette(
#   c("#3131CC", "#4F9EEB", "#F9D461")
# )(100)
breaks <- seq(minYear, maxYear, length.out = 101)
colour_scale <- colorRampPalette(
  c("#3131CC", "#4F9EEB", "#469395", "#B4CA85", "#F9D461", "#EE3838")
)(100)
minYear = 2002; maxYear = mostRecentSamplingDatum
endYears_indices = (((mcc_tab[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
endYears_colours = colour_scale[endYears_indices]
polygons_colours = rep(NA, length(polygons))
for (i in 1:length(polygons)) {
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"40")
}                            


# Co-plotting HPD regions and MCC tree

# Read shapefile using sf (replaces rgdal)
borders <- st_read("raw_data/spatial/shapefiles/Peru Shapefiles/DISTRITOS_inei_geogpsperu_suyopomalia/DISTRITOS_inei_geogpsperu_suyopomalia.shp")

borders_region <- borders %>%
  filter(NOMBDEP %in% c("AREQUIPA", "PUNO")) %>%
  group_by(NOMBDEP) %>%
  summarise(geometry = st_union(geometry))
plot(st_geometry(borders_region), border = "gray30", lwd = 0.6)

mcc_ext <- ext(
  range(c(mcc_tab$startLon, mcc_tab$endLon), na.rm = TRUE),
  range(c(mcc_tab$startLat, mcc_tab$endLat), na.rm = TRUE)
)

# add buffer (e.g. 10% of range)
buf_x <- 0.20 * (mcc_ext[2] - mcc_ext[1])
buf_y <- 0.20 * (mcc_ext[4] - mcc_ext[3])

mcc_ext <- ext(
  mcc_ext[1] - buf_x,
  mcc_ext[2] + buf_x,
  mcc_ext[3] - buf_y,
  mcc_ext[4] + buf_y
)


# get OSM tiles
# osm <- get_tiles(
#   x = vect(mcc_ext, crs = "EPSG:4326"),
#   provider = "OpenStreetMap",
#   zoom = 12
# )
osm_light <- get_tiles(
  x = vect(mcc_ext, crs = "EPSG:4326"),
  provider = "Esri.WorldGrayCanvas", 
  zoom = 12
)
# osm_db<- get_tiles(
#   x = vect(mcc_ext, crs = "EPSG:4326"),
#   provider = "CartoDB.Positron", 
#   zoom = 12
# )
# osm_topo<- get_tiles(
#   x = vect(mcc_ext, crs = "EPSG:4326"),
#   provider = "Esri.WorldTopoMap", 
#   zoom = 15
# )

crs_proj <- "EPSG:32719"  # Arequipa region

osm_proj <- project(osm_topo, crs_proj)
template_raster_proj <- project(template_raster, crs_proj)
# Query rivers



png("results/figures/n166-rrw-beast-map-db.png", width = 180, height = 140, units = "mm", res = 600)
cairo_pdf("results/figures/n166-rrw-beast-map-db.pdf", width = 180/25.4, height = 140/25.4)
par(mar=c(0,0,0,0), oma=c(1.2,3.5,1,0), mgp=c(0,0.4,0), lwd=0.2, bty="o")
#plotRGB(osm, axes = FALSE)
plotRGB(osm_light, axes = FALSE)
plot(st_geometry(borders_region), border = "gray30", lwd = 0.6, add=T)
usr <- par("usr")
label_x <- c(
  usr[1] + 0.15 * (usr[2] - usr[1]),  # AREQUIPA
  usr[2] - 0.2 * (usr[2] - usr[1]+3)   # PUNO
)

label_y <- c(
  usr[3] + 0.5 * (usr[4] - usr[3]),
  usr[3] + 0.8 * (usr[4] - usr[3])
)

text(label_x, label_y,
     labels = c("AREQUIPA", "PUNO"),
     cex = 0.9,
     col = "gray20",
     font = 2)

for (i in 1:length(polygons)) {
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
#plot(st_geometry(borders_simplified), add = TRUE, lwd = 0.1, border = "gray10")
for (i in 1:dim(mcc_tab)[1]) {
  curvedarrow(cbind(mcc_tab[i,"startLon"],mcc_tab[i,"startLat"]),
              cbind(mcc_tab[i,"endLon"],mcc_tab[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=0.2, lty=1, lcol="gray10", arr.col=NA,
              arr.pos=F, curve=0.1, dr=NA, endhead=F)
}
for (i in dim(mcc_tab)[1]:1) {
  if (i == 1) {
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=16,
           col=colour_scale[1], cex=0.8)
    points(mcc_tab[i,"startLon"], mcc_tab[i,"startLat"], pch=1,
           col="gray10", cex=0.8)
  }
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=16,
         col=endYears_colours[i], cex=0.8)
  points(mcc_tab[i,"endLon"], mcc_tab[i,"endLat"], pch=1,
         col="gray10", cex=0.8)
}
# rect(xmin(template_raster), ymin(template_raster), xmax(template_raster),
#       ymax(template_raster), xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(mcc_ext)), floor(xmax(mcc_ext))),
pos=ymin(mcc_ext), mgp=c(0,0.4,0), cex.axis=0.7, lwd=0, lwd.tick=0.2,
padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(mcc_ext)), floor(ymax(mcc_ext))),
pos=xmin(mcc_ext), mgp=c(0,0.6,0), cex.axis=0.7, lwd=0, lwd.tick=0.2,
padj=1, tck=-0.01, col.axis="gray30")
rast = raster(matrix(nrow=1, ncol=2))
rast[1] = min(mcc_tab[,"startYear"])
rast[2] = max(mcc_tab[,"endYear"])

map.scale(x = xmax(mcc_ext) -1.5,
          y = ymin(mcc_ext) +0.2,
          relwidth = 0.2,
          metric = TRUE,
          ratio = FALSE,
          cex = 1, lwd = 6, col="black")
dev.off()



cairo_pdf("results/figures/n166-rrw-beast-map-timescale_legend.pdf", width = 180/25.4,height = 40/25.4)

par(mar = c(2, 2, 2, 2))

plot(rast,
     legend.only = TRUE,
     col = colour_scale,
     legend.width = 0.4,
     legend.shrink = 0.6,
     smallplot = c(0.0, 1.0, 0.45, 0.55),  # FULL 180 mm width
     legend.args = list(text = "", cex = 1, line = 0.3, col = "gray30"),
     horizontal = TRUE,
     axis.args = list(
       cex.axis = 0.9,
       lwd = 0,
       lwd.tick = 0.2,
       tck = -0.2,
       col.axis = "black",
       at = seq(2002, 2025, 2)
     )
)

dev.off()
plot(rast, legend.only=T, col=colour_scale, legend.width=0.5,
     legend.shrink=0.5, smallplot=c(0.25,0.70,0.06,0.08), legend.args=list(text="",cex=1, line=0.3, col="gray30"), horizontal=T, axis.args=list(cex.axis=1,lwd=0, lwd.tick=0.5, tck=-0.5, col.axis="black", line=0, mgp=c(0,-0.02,0), at = seq(2002, 2025, 2)))

