# -----------------------------
# 1. Load packages
# -----------------------------
library(ggtree)
library(treeio)
library(tidyverse)
library(lubridate)
library(pals)
library(scico)
library(ggrepel)
library(Polychrome)
library(osmdata)

# ------# ------# -----------------------------
# 2. Load tree
# -----------------------------
#tree_file <- "analysis/BEAST_runs/no-trait/n167/strict-constant/n167-strict-constant.hipstr.tre"
tree_file <- "analysis/BEAST_runs/no-trait/n166/n166-ucld-constant.hipstr.tre"
tree <- read.beast(tree_file)

latest_year <- "2025-03-08"
latest_year_decimal <- decimal_date(as.Date(latest_year))

# -----------------------------
# 3. Load and process metadata
# -----------------------------
meta_file <- "processed_data/processed_metadata/081025_epi-seq_n167_beast_annot2.txt"
meta <- read.table(meta_file, sep="\t", header=TRUE, stringsAsFactors = FALSE)

meta <- meta %>%
  mutate(
    # Extract date from taxon
    raw_date = str_extract(taxon, "\\d{1,2}-[A-Za-z]{3}-\\d{2}"),
    date = dmy(raw_date),
    date_iso = format(date, "%Y-%m-%d"),
    # Rebuild taxon to match tree tip labels
    taxon_fixed = str_replace(taxon, "\\d{1,2}-[A-Za-z]{3}-\\d{2}", date_iso),
    clean_label = sub("_(\\d{4}-\\d{2}-\\d{2})$", "", taxon_fixed)
  ) %>%
  dplyr::select(taxon_fixed, everything())

# -----------------------------
# 5. Create ggtree object and attach metadata
# -----------------------------
p <- ggtree(tree, mrsd = latest_year, col = "black") %<+% meta 
# plot tree with red node labels
p +
  geom_text2(aes(label = node), color = "red", size = 3, hjust = -0.3, vjust = -0.3)

# -----------------------------
# 4. Node heights (convert to dates)
# -----------------------------
# Node heights from BEAST tree
node_heights <- p@data$height_mean

# Use the index as the node number
node_df <- data.frame(
  node = seq_along(node_heights),  # 1, 2, 3, … length(node_heights)
  height = node_heights
)

# Convert to decimal years relative to latest_year
latest_year_decimal <- decimal_date(as.Date(latest_year))
node_df <- node_df %>%
  mutate(
    node_age_decimal = latest_year_decimal - height,
    node_age_date = decimal2Date(node_age_decimal)
  )


# pretty names for districts
district_labels <- c(
  ccolorado   = "Cerro Colorado",
  asa         = "Alto Selva Alegre",
  mollebaya   = "Mollebaya",
  yura        = "Yura",
  cayma       = "Cayma",
  hunter      = "Jacobo Hunter",
  mmelgar     = "Mariano Melgar",
  jlbyr       = "José Luis Bustamante y Rivero",
  paucarpata  = "Paucarpata",
  characato   = "Characato",
  miraflores  = "Miraflores",
  socabaya    = "Socabaya",
  sachaca     = "Sachaca",
  uchumayo    = "Uchumayo",
  pedregal    = "El Pedregal",
  puno        = "Puno/Madre de Dios"
)

p@data$district_pretty   <- district_labels[p@data$district]
meta$district_pretty     <- district_labels[meta$district]

# -----------------------------
# 6. Define colors for geography
# -----------------------------
geo_levels <- unique(p@data$district_pretty[!is.na(p@data$district_pretty)])#high contrast, bit vulgar:
geo_cols <- setNames(
  colorspace::desaturate(glasbey(length(district_labels)), amount = 0.2),
  geo_levels
)
# pal16 <- glasbey(length(geo_levels))
# pal16.mute <- colorspace::desaturate(pal16, amount=0.2)
# geo_cols=pal16.mute
# palette_16 <- colorspace::qualitative_hcl(length(geo_levels), palette = "Dark 3")
# 
# palette_16 <- Polychrome::palette36.colors(length(geo_levels))
# #palette_16 <-colorspace::desaturate(palette_16, amount = 0.2)
# geo_cols <- setNames(
#   palette_16,
#   geo_levels
# )
# library(colorspace)
# 
# #ok contrast, muted:
# pal16 <- qualitative_hcl(length(geo_levels), palette = "Dark 3")
# geo_cols <- setNames(
#   pal16,
#   geo_levels
# )
# library(scico)
# pal16 <- scico(length(geo_levels), palette = "tokyo")
# geo_cols <- setNames(
#   pal16,
#   geo_levels
# )
# Save
#saveRDS(geo_cols, file = "scripts/R/geo_cols.rds")

all_districts <- unique(na.omit(p$data$district_pretty))
district_order <- c(
  
  setdiff(all_districts, c("El Pedregal", "Puno/Madre de Dios")),
  
  "El Pedregal",
  
  "Puno/Madre de Dios"
  
)
p@data$district_pretty <- factor(p@data$district_pretty, levels = district_order)
meta$district_pretty   <- factor(meta$district_pretty, levels = district_order)
# -----------------------------
# Define shapes for districts/geography
# -----------------------------
# Define shapes: pedregal and puno distinct, others default to circle (16)
geo_shapes <- c("El Pedregal" = 17,  # triangle
                "Puno/Madre de Dios" = 15)      # square

# All other districts will use default shape 16 (circle)
geo_shapes <- c(
  geo_shapes,
  setNames(rep(16, length(setdiff(district_order, names(geo_shapes)))),
           setdiff(district_order, names(geo_shapes)))
)

# Check
geo_shapes

# -----------------------------
# 7. Plot tree
# -----------------------------
# Nodes to highlight / label (e.g., MRCA of Arequipa & El Pedregal cluster)
#nodes_to_label <- c(187, 175, 178)
nodes_to_label <- c(185, 173, 176)



# see nodel labels 
# p+ geom_text2(
#   aes(label = node,subset = isTip == FALSE),
#   hjust =1.3, vjust = 0, size = 2.5, color = "darkred")
p_expand <- p %>%
  scaleClade(167, scale = 4, vertical = TRUE) %>%
  scaleClade(168, scale = 4, vertical = TRUE) %>%
  scaleClade(173, scale = 3, vertical = TRUE)%>%
  scaleClade(323, scale = 3, vertical = TRUE)

# Extract layout positions for node labels
layout <- p_expand$data

node_label_df <- layout %>%
  filter(node %in% nodes_to_label) %>%
  left_join(node_df %>% 
              dplyr::select(node, node_age_date), by = "node")

# Determine min and max year from tree
x_min <- floor(min(p$data$x))  # earliest decimal year
x_max <- floor(max(p$data$x))+1 # latest decimal year

# Final plot
p2 <- p_expand +
  
  geom_nodepoint(
    aes(subset = !is.na(as.numeric(posterior)) & as.numeric(posterior) > 0.9),
    fill = "lightgrey",
    colour = "black",
    shape = 23,
    alpha = 0.7,
    size = 1.2
  ) +
  
  # geom_tippoint(
  #   aes(fill = district_pretty),
  #   shape = 21,
  #   colour = "black",
  #   stroke = 0.35,
  #   size = 3,
  #   alpha = 0.8
  # ) +
  # 
  geom_tippoint(
    aes(
      shape = district_pretty,
      colour = district_pretty
    ),
    stroke = 0.5,
    size = 3,
    alpha = 0.8
  ) +
  scale_shape_manual(values = geo_shapes, name = "District") +
  scale_color_manual(values = geo_cols, name = "District") +
  # geom_tiplab(
  #   aes(label = clean_label),
  #   size = 1.2,
  #   offset = 0.4
  # ) +
  
  geom_label_repel(
    data = node_label_df,
    aes(x = x, y = y, label = format(node_age_date, "%Y-%m-%d")),
    nudge_x = -1.4,
    nudge_y = 0.5,
    direction = "y",
    force = 4,
    segment.color = "grey40",
    segment.size = 0.8,
    size =4,
    #fontface = "bold",
    box.padding = 0.25,
    label.padding = unit(0.15, "lines"),
    min.segment.length = 0.3,
    fill = alpha("white", 0.75)
  ) +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(x_min, x_max, by = 4),
    minor_breaks = seq(x_min+2, x_max+1, by = 2),
    limits = c(x_min, x_max)
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  
  # theme(
  #   axis.text.x = element_text(angle = 45, hjust = 0.5, size =14),
  #   axis.title.x = element_text(size = 14,face = "bold"),
  #   #legend.position = "left",
  #     legend.position = c(0.02, 0.55),
  #     legend.justification = c(0, 0.5),
  #   legend.background = element_rect(fill = "transparent", colour = NA),
  #   legend.title = element_text(face = "bold", size = 14),
  #   legend.text = element_text(size = 12),
  #   legend.key.size = unit(0.5, "cm"),
  #   plot.margin = margin(10, 10, 10, 10),
  #   axis.line.x = element_line(colour = "black", linewidth = 0.4),
  #   axis.ticks.x = element_line(colour = "black"),
  #   axis.ticks.length = unit(0.1, "cm")
  # )+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title.position = "top",
    
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    
    plot.margin = margin(10, 10, 10, 10),
    axis.line.x = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.length = unit(0.1, "cm")
  )+
  coord_cartesian(clip = "off");p2
# -----------------------------
# 8. Show plot
# -----------------------------
p2 <- p2 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA)
  )

  # p2 +
  # geom_hilight(node = 185, fill = "#3131CC", alpha = 0.2) +
  # geom_hilight(node = 168, fill = "#1B511B", alpha = 0.2) +
  # geom_hilight(node = 173, fill = "#030327", alpha = 0.2)

# -----------------------------

# -----------------------------
# Save publication-ready plot: full width, half-height A4
# -----------------------------
ggsave(
  filename = "results/figures/tree_half_A4_fullwidth.pdf",  # PDF for vector quality
  plot = p2,
  width = 21,      # full A4 width in cm
  height = 20,  # half A4 height in cm
  units = "cm",
  dpi = 600,
  bg = "transparent"
)

# Optional: PNG raster version
ggsave(
  filename = "results/figures/tree_half_A4_fullwidth.png",
  plot = p2,
  width = 21,
  height = 20,
  units = "cm",
  dpi = 600,
  bg = "transparent"
)


# install if needed
# install.packages("terra")
# install.packages("sf")
# install.packages("ggplot2")

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
options(ggspatial.cache = TRUE)  
# Download GADM Peru ADM2 (districts)
gadm_url <- "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_PER_shp.zip"
temp <- tempfile(fileext = ".zip")
download.file(gadm_url, temp)

# Read all layers
unzip(temp, exdir = tempdir())
shp_files <- list.files(tempdir(), pattern = "\\.shp$", full.names = TRUE)

# GADM contains PER with multiple levels — pick ADM2 for districts
peru_adm3 <- st_read(shp_files[grepl("_3.shp$", shp_files)])

# Filter all districts in Arequipa province
arequipa_city <- peru_adm3 |>
  dplyr::filter(NAME_1 == "Arequipa")
arequipa_city <- arequipa_city |>
  dplyr::filter(NAME_3 %in% c(
    "Alto Selva Alegre",
    "Arequipa",
    "Cayma",
    "Cerro Colorado",
    "Characato",
    "Chiguata",
    "Jacobo Hunter",
    "Jose Luis Bustamante Y Rivero",
    "Mariano Melgar",
    "Miraflores",
    "Mollebaya",
    "Paucarpata",
    "Polobaya",
    "Quequeña",
    "Sabandia",
    "Sachaca",
    "Socabaya",
    "Tiabaya",
    "Uchumayo",
    "Yanahuara",
    "Yarabamba",
    "Yura"
  ))



# -----------------------------
# 1. Clean district names
# -----------------------------
arequipa_city <- arequipa_city %>%
  mutate(
    district_clean = tolower(gsub("[^a-zA-Z0-9]", "", NAME_3))
  )

# -----------------------------
# 2. Convert metadata to sf
# -----------------------------
points_sf <- st_as_sf(meta, coords = c("lon", "lat"), crs = 4326)

# filter unwanted samples
points_sf <- points_sf[!points_sf$district %in% c("majes", "puno", "pedregal"), ]

# -----------------------------
# 3. Compute bbox (ONCE)
# -----------------------------
bbox <- st_bbox(points_sf)

# proportional buffer
pad_x <- (bbox["xmax"] - bbox["xmin"]) * 0.25
pad_y <- (bbox["ymax"] - bbox["ymin"]) * 0.25

bbox_expanded <- bbox
bbox_expanded[c("xmin","xmax")] <- bbox[c("xmin","xmax")] + c(-pad_x, pad_x)
bbox_expanded[c("ymin","ymax")] <- bbox[c("ymin","ymax")] + c(-pad_y, pad_y)

xlim <- c(bbox_expanded["xmin"], bbox_expanded["xmax"])
ylim <- c(bbox_expanded["ymin"], bbox_expanded["ymax"])

# -----------------------------
# 4. Load rivers
# -----------------------------
rivers <- st_read(
  "raw_data/spatial/shapefiles/peru-260528-free.shp/gis_osm_waterways_free_1.shp"
) %>%
  st_transform(4326)

# clip to study area (safe version)
arequipa_rivers <- st_intersection(rivers, st_as_sfc(bbox)) %>%
  dplyr::filter(name == "Río Chili")

adm_clip <- st_crop(arequipa_city, bbox_expanded)

arequipa_rabies_map <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 12)+
 # geom_sf(data = adm_clip, fill = NA, color = "grey40") +
  # 👇 dim the basemap
  # 👇 dim overlay (must come immediately after tiles)
  # annotate(
  #   "rect",
  #   xmin = -Inf, xmax = Inf,
  #   ymin = -Inf, ymax = Inf,
  #   fill = "white", alpha = 0.25
  # ) +
  # District boundaries only (no fill)
 # geom_sf(data = arequipa_city, fill = NA, color = "grey60", alpha=0.5,linewidth = 0.5) +
  #geom_sf(data = chili_river_sf, color = "#8ecae6", size = 1) +
  # Sampling points coloured by district
  geom_sf(data = points_sf, aes(color = district_pretty),
          size = 3.2, alpha = 0.8) +
  geom_sf(
    data = arequipa_rivers,
    colour =  "#4FC3F7",
    linewidth = 0.8,
    alpha = 0.8
  ) +
  # Border layer (underneath)
  geom_sf(
    data = points_sf,
    size =4.2,
    shape = 21,
    colour = "black",
    fill = NA,
    alpha = 0.9
  ) +
  scale_color_manual(values = geo_cols, name = "District") +
  # geom_sf(data = arequipa_city, fill = NA, color = "grey60", alpha=0.5,linewidth = 0.5) +
  # District boundaries only (no fill)

  # Optional map decorations
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    text_cex = 1.2,
    line_width = 0.8
  ) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
pad_x = unit(0.5, "cm"),
pad_y = unit(1.5, "cm"))+
  # coord_sf(
  #   xlim = xlim,
  #   ylim = ylim,
  #   expand = FALSE
  # ) +
  # Clean theme
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12)
  ) + theme(legend.position = "none"); arequipa_rabies_map
  
ggsave(
  filename = "results/figures/arequipa_rabies_map.png",
  plot = last_plot(),
  width = 180,
  height = 140,
  units = "mm",
  dpi = 600,
  bg = "white"
)
ggsave(
  filename = "results/figures/arequipa_rabies_map.pdf",
  plot = last_plot(),
  width = 180,
  height = 140,
  units = "mm",
  device = cairo_pdf
)
 


library(patchwork)

figure_plot <- (p2 +arequipa_rabies_map) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 14)
  );figure_plot

ggsave(
  filename = "results/figures/n166tree-map.pdf",  # PDF for vector quality
  plot = figure_plot ,
  width = 21,      # full A4 width in cm
  height = 30,  # half A4 height in cm
  units = "cm",
  dpi = 600
)

# Optional: PNG raster version
ggsave(
  filename = "results/figures/n166tree-map.png",
  plot = figure_plot ,
  width = 21,
  height = 30,
  units = "cm",
  dpi = 600
)
 # labs(title = "Sampling Locations by District")

library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggspatial)
library(dplyr)
library(ggplot2)
library(sf)
library(dplyr)

# Load shapefile
aqp <- st_read("raw_data/spatial/shapefiles/AQP_AQP_province_districts.shp")

# Clean and harmonise district names
aqp <- aqp %>%
  mutate(
    NAME_3 = NAME_3 %>%
      gsub("[^a-zA-Z0-9]", "", .) %>%   # remove non-alphanumeric
      tolower() %>%                     # lowercase
      gsub("cerrocolorado", "ccolorado", .) %>%
      gsub("altoselvaalegre", "asa", .) %>%
      sub("marianomelgar", "mmelgar", .) %>%
      gsub("caminaca|atuncolla|azangaro", "", .)
  )

# Your colour scheme
geo_levels <- unique(points_sf$district)        # district names in your metadata
geo_levels[geo_levels==""]
geo_cols <- setNames(glasbey(length(geo_levels)), geo_levels)

# Define fill scale for ggplot
fillScale_district <- scale_fill_manual(
  values = geo_cols,
  na.value = "#f8f4f0"   # optional background for unmatched
)

# Plot
ggplot() +
  geom_sf(data = aqp, aes(fill = NAME_3), colour = "grey40", linewidth = 0.5) +
  fillScale_district +               # use your colour scheme
  theme_void() +                     # minimal theme
  theme(legend.position = "none")+  # Optional map decorations
  annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.8) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) 



# Produce map with Chili River with all cases by urban/periurban and north/south
ggplot() +
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  
  # Add river
  geom_sf(data = chili_river_sf, color = "#8ecae6", size = 1) +
  
  geom_sf(
    data = points_sf,
    aes(color = district),
    size = 2.5
  ) +
  
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_minimal() +
  labs(color = "River group", shape = "Area type", title = "Sequenced Cases in Arequipa City with Chili River")
  

#Exclude waterchannels
# Keep only features named "Río Chili" if available
chili_river_sf <- chili_river_sf %>%
  filter(name == "Río Chili")

# Merge all lines into one MULTILINESTRING
chili_river_main <- chili_river_sf %>%
  st_union() %>%
  st_cast("LINESTRING")  # optional: convert to simple line(s)

###Plot figure
ggplot() + # Districts
  annotation_map_tile(type = "osm", zoom = 12)+
  geom_sf(data = arequipa_city, fill = "grey95", color = "grey40") +
  # Chili River
  geom_sf(data = chili_river_main, color = "#8ecae6", size = 5) +
  # Cases
  geom_sf(
    data =points_sf,
    aes(color = district),
    size = 2.5
  ) +
  # Zoom to city
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  # Scale bar
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1) +
  # Clean theme: remove axes
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  # Labels
  labs(
    color = "Position in relation to Chili River",
    shape = "Urbanicity",
    title = "Sequenced Rabies Cases in Arequipa City"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

  