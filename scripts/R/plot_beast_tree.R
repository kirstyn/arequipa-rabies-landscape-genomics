# Load libraries
library(ggtree)
library(treeio)
library(ggplot2)
library(dplyr)
library(viridis) # for perceptually uniform colour palette
library(pals)
# Load the BEAST MCC tree (with branch rates annotated)
tree_file <- "analysis/BEAST_runs/Logs/17/PER_RABV_2024_SS_17.prelim.hipstr.tre"
mcc_tree <- read.beast(tree_file)
annot <- read.table("processed_data/processed_metadata/171025_epi-seq_n167_beast_annot.txt", sep="\t", header=T)

# Inspect available annotations
mcc_tree@data %>% head()

# Determine number of unique districts for palette
n_districts <- length(unique(annot$district))

# Use a nice categorical palette
district_cols<- kelly(n_districts+3)[-c(1,2,9)]
names(district_cols) <- unique(annot$district)
colScale_district <- scale_color_manual(name ="District",values = district_cols, na.translate=F)
fillScale_district<- scale_fill_manual(name ="District",values = district_cols, na.translate=F)


### Tree without spatial model
## attach the sampling information data set 
## and add symbols colored by location
p <- ggtree(mcc_tree, mrsd = "2025-03-08") %<+% annot +
  geom_tree(size = 0.6, color = "black") +
  geom_nodepoint(aes(subset = as.numeric(posterior) > 0.9 & x < 2015),
                 shape = 23, color = "grey30", fill = "lightgrey", size = 2, alpha = 0.8)+
  geom_tippoint(aes(colour = district), size = 3) +
colScale_district+
  theme_tree2() +
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "white", colour = NA),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(5, 5, 5, 5)
  )
p

ggtree(mcc_tree, mrsd = "2025-03-08") %<+% annot +
  geom_tree(aes(color = rate), size = 0.6) +
  scale_color_gradientn(
    colours = c("#313695", "#4575b4", "#ffffbf", "#d73027", "#a50026"),
    name = "Diffusion rate (km/yr)",
    trans = "log10"
  ) +
  geom_nodepoint(aes(subset = as.numeric(posterior) > 0.9 & x < 2015),
                 shape = 23, color = "grey30", fill = "lightgrey", size = 2, alpha = 0.8)+
  theme_tree2() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )

library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggspatial)
library(dplyr)
aqp <- st_read("raw_data/spatial/shapefiles/AQP_AQP_province_districts.shp")

aqp <- aqp %>%
  mutate(
    # Make district formatting consistent with trait file
    NAME_3 = NAME_3 %>%
      gsub("[^a-zA-Z0-9]", "", .) %>%   # remove non-alphanumeric
      tolower() %>%                     # lowercase
      gsub("cerrocolorado", "ccolorado", .) %>%
      gsub("altoselvaalegre", "asa", .) %>%
      sub("marianomelgar", "mmelgar", .) %>%
      gsub("caminaca|atuncolla|azangaro", "", .)
  )


# Plot
ggplot() +
  # Add Arequipa shapefile (district boundaries)
  geom_sf(data = aqp, aes(fill =NAME_3), colour = "grey40", linewidth = 0.5) +
  fillScale_district+
  theme_void() +        # removes all axes, gridlines, background
  theme(legend.position = "none")  # removes the legend

# Assuming your rate is stored in 'rate' in the node/edge data
# If it’s in a different tag, replace 'rate' with the correct column

# Plot the tree
g <- p +
            #aes(color=rate))+
            geom_tree(size = 0.5) +
  geom_tippoint(aes(colour = district), size = 2) +
 # scale_color_viridis(option="C", name="Diffusion rate\n(km/yr)") + 
  theme_tree2() +  # adds x-axis with timescale
 # labs(title="Phylogenetic tree with branch diffusion rates") +
  theme(legend.position = "right")+
  geom_nodepoint(aes(subset = as.numeric(label) > 0.8),
                 color = "red", size = 2)+
  geom_tippoint((aes(color=district)))
# Display the tree
g

## Arequipa zoom 
# --- Download GADM Peru ADM3 (districts) ---
# Use the same GADM 4.1 shapefile
peru_adm3 <- st_read(shp_files[grepl("_3.shp$", shp_files)])

# Clean district names
peru_adm3 <- peru_adm3 %>%
  mutate(district_clean = tolower(gsub("[^a-zA-Z0-9]", "", NAME_3)))  # ADM3 = districts

# --- Filter to Arequipa province ---
arequipa_districts <- peru_adm3 %>%
  filter(NAME_2 == "Arequipa")   # NAME_2 = province

# --- Bounding box for Arequipa city ---
bbox <- st_bbox(arequipa_districts)
buffer <- 0.02  # smaller buffer for city-level zoom
xlim <- c(bbox["xmin"] - buffer, bbox["xmax"] + buffer)
ylim <- c(bbox["ymin"] - buffer, bbox["ymax"] + buffer)

# --- Plot ---
aqp_map <- ggplot() +
  # District polygons (ADM3)
  geom_sf(data = arequipa_districts, fill = NA, color = "grey60", linewidth = 0.5) +
  
  # Sampling points coloured by district
  geom_sf(data = points_sf, aes(color = district), size = 2.5, alpha = 0.8) +
  scale_color_manual(values = geo_cols, name = "District") +
  
  # Optional decorations
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.8) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  
  # Zoom in
  coord_sf(xlim = xlim, ylim = ylim) +
  
  # Clean theme
  theme_void() +
  theme(
    legend.position = "none"
  )
