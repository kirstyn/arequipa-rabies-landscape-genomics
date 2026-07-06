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


# Extract spatio-temporal info from MCC tree
#mcc_tre = readAnnotatedNexus("analysis/BEAST_runs/Logs/mainIntro_skygrid_rrw/aqp_mainIntro_skygrid_rrw.hipstr.tre")
mcc_tre = readAnnotatedNexus("analysis/BEAST_runs/continuous-trait-runs/n166/n166-rrw-ucld-constant.hipstr.tre")
mcc_tre = readAnnotatedNexus("analysis/BEAST_runs/continuous-trait-runs/n147/arequipa_only_mainIntro_n147_rrw.hipstr.tre")
mcc_tre = readAnnotatedNexus("analysis/BEAST_runs/continuous-trait-runs/n148/peru-148-rrw.hipstr.tre")
mostRecentSamplingDatum = 2025.180

#mcc_tab = mccTreeExtractions(mcc_tre, mostRecentSamplingDatum)
#write.csv(mcc_tab, "analysis/BEAST_runs/continuous-trait-runs/n147/arequipa_only_mainIntro_n147_rrw.hipstr.csv", row.names=F, quote=F)
mcc_tab = read.csv("analysis/BEAST_runs/continuous-trait-runs/n148/peru-148-rrw.extracted.csv", head=T)

# Extract spatio-temporal info from posterior trees
localTreesDirectory = "/Users/kirstyn.brunker/GitHub/RABV_Arequipa_2025/analysis/BEAST_runs/continuous-trait-runs/n148/extracted_trees/"
allTrees = scan(file="analysis/BEAST_runs/continuous-trait-runs/n148/peru-148-rrw-subsample.trees", what="", sep="\n", quiet=T)
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
endYears_colours <- map_year_to_col(
  mcc_tab$endYear,
  minYear,
  maxYear,
  colour_scale
)
# Setting colour scales
colour_scale = colorRampPalette(brewer.pal(11,"RdYlGn"))(141)[21:121]
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
#borders <- st_read("processed_data/gis_data/aqp_district_latest.rds")
borders <- readRDS("processed_data/gis_data/aqp_district_latest.rds")
borders <- st_read("raw_data/spatial/shapefiles/Peru Shapefiles/DISTRITOS_inei_geogpsperu_suyopomalia/DISTRITOS_inei_geogpsperu_suyopomalia.shp")


# Read raster using terra
template_raster <- rast("analysis/seraphim/n167_Study_area.asc")
template_raster <- rast("analysis/seraphim/aqpCity_Study_area.asc")


# Crop shapefile to raster extent
#borders <- crop(borders, extent(template_raster))
borders_cropped <- st_intersection(
  borders,
  st_as_sfc(st_bbox(template_raster))
)

# Simplify geometry (sf equivalent of gSimplify)
borders_simplified <- st_simplify(borders_cropped, dTolerance = 0.01)              


# bbox from your raster
ext <- ext(template_raster)


# Define bounding box (use your template extent)

bb <- st_bbox(c(
  xmin = xmin(template_raster),
  ymin = ymin(template_raster),
  xmax = xmax(template_raster),
  ymax = ymax(template_raster)
), crs = st_crs(4326))


# get OSM tiles
osm <- get_tiles(
  x = vect(ext, crs = "EPSG:4326"),
  provider = "OpenStreetMap",
  zoom = 15
)
osm_light <- get_tiles(
  x = vect(ext, crs = "EPSG:4326"),
  provider = "Esri.WorldGrayCanvas", 
  zoom = 13
)
osm_db<- get_tiles(
  x = vect(ext, crs = "EPSG:4326"),
  provider = "CartoDB.Positron", 
  zoom = 15
)
osm_topo<- get_tiles(
  x = vect(ext, crs = "EPSG:4326"),
  provider = "Esri.WorldTopoMap", 
  zoom = 15
)

crs_proj <- "EPSG:32719"  # Arequipa region

osm_proj <- project(osm_topo, crs_proj)
template_raster_proj <- project(template_raster, crs_proj)
# Query rivers

rivers <- opq(bb) |>
  add_osm_feature(key = "waterway", value = "river") |>
  osmdata_sf()
chili <- rivers$osm_lines |>
  
  dplyr::filter(grepl("Chili", name, ignore.case = TRUE))

png("results/figures/n148-rrw-beast-map-osm.png", width = 180, height = 140, units = "mm", res = 600)
cairo_pdf("results/figures/n148-rrw-beast-map-osm.pdf", width = 180/25.4, height = 140/25.4)
par(mar=c(0,0,0,0), oma=c(1.2,3.5,1,0), mgp=c(0,0.4,0), lwd=0.2, bty="o")
#plot(template_raster, col="white", box=F, axes=F, colNA="grey90", legend=F)
#plotRGB(osm, axes = FALSE)
plotRGB(osm_light, axes = FALSE)
#plotRGB(osm_topo, axes = FALSE)
plot(st_geometry(rivers$osm_lines),
     add = TRUE,
     col = adjustcolor("#2b8cbe", alpha.f = 0.7),
     lwd = 0.6)
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
axis(1, c(ceiling(xmin(ext)), floor(xmax(ext))),
       pos=ymin(ext), mgp=c(0,0.4,0), cex.axis=0.7, lwd=0, lwd.tick=0.2,
       padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(ext)), floor(ymax(ext))),
       pos=xmin(ext), mgp=c(0,0.6,0), cex.axis=0.7, lwd=0, lwd.tick=0.2,
       padj=1, tck=-0.01, col.axis="gray30")
rast = raster(matrix(nrow=1, ncol=2))
rast[1] = min(mcc_tab[,"startYear"])
rast[2] = max(mcc_tab[,"endYear"])
 plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5,
       legend.shrink=0.5, smallplot=c(0.25,0.70,0.06,0.08), legend.args=list(text="",cex=1, line=0.3, col="gray30"), horizontal=T, axis.args=list(cex.axis=1,lwd=0, lwd.tick=0.5, tck=-0.5, col.axis="black", line=0, mgp=c(0,-0.02,0), at = seq(2002, 2025, 2)))
  map.scale(x = xmin(template_raster) + 0.0,
           y = ymin(template_raster) + 0.01,
           relwidth = 0.2,
           metric = TRUE,
           ratio = FALSE,
           cex = 1, lwd = 5, col="black")
 dev.off()
 

 