# ==============================================================================
# Title: plot_contextual_map.R
# Description: Plot all resistence layers used in landscape analyses, aligning with paper's colour scheme
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))
# -------------------------
# COLOURS
# -------------------------
cols <- rev(hcl.colors(100, "YlOrRd"))

# -------------------------
# LOAD RASTERS
# -------------------------
r_list <- list(
  cropland = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","cropland_resistance_k100.asc")),
  roads_decay = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","Roads_decay_k100.asc")),
  roads_binary = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","Roads_binary_k100.asc")),
  habitation = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","SES_inhabited_binary_k100.asc")),
  affluence = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","SES_wealthyVdeprived_bkgNA_k100.asc")),
  ses_gradient = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","SES_gradient_highRaffluence_bkgNA_k100.asc")),
  river_decay = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","river_decay_k100.asc")),
  river_binary = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","river_binary_k100.asc")),
  water_decay = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","WaterChannels_decay_k100.asc")),
  water_binary = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","WaterChannels_binary_k100.asc")),
  accessibility_log = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","accessibility_log_resistance_k100.asc")),
  pop_density = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","pop2015_resistance_k100.asc")),
  abs_pop_change = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","pop_abschange_resistance_k100.asc")),
  pop_growth = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","pop_growth_resistance_k100.asc")),
  pop_decline = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","pop_decline_resistance_k100.asc")),
  pop_mag = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","pop_mag_resistance_k100.asc")),
  deprivation = rast(here("analysis","seraphim","resistance_rasters","grid_1kmbuffer","SES_deprivedVaffluent_bkgNA_k100.asc"))
)

# -------------------------
# ADD EMPTY FIRST PANEL (WILL MAPPED CASES)
# -------------------------
blank_raster <- r_list[[1]]
blank_raster[] <- NA

r_list <- c(list(overview = blank_raster), r_list)

# -------------------------
# PRETTY LABELS
# -------------------------
pretty_names <- c(
  overview = "",
  cropland = "Cropland",
  roads_decay = "Roads (decay)",
  roads_binary = "Roads (binary)",
  habitation = "Habitation",
  affluence = "Affluence",
  deprivation = "Deprivation",
  ses_gradient = "SES (gradient)",
  river_decay = "River (decay)",
  river_binary = "River (binary)",
  water_decay = "Water channels\n(decay)",
  water_binary = "Water channels\n(binary)",
 # accessibility = "Accessibility",
  accessibility_log = "Accessibility\n(log10)",
  pop_density = "Pop\ndensity",
  pop_growth = "Pop change:\ngrowth",
 pop_decline = "Pop change:\ndecline",
  abs_pop_change = "Pop change:\nabsolute",
 pop_mag="Pop change:\nmagnitude"
)

# -------------------------
# ORDER PANELS
# -------------------------
layer_order <- c(
  "overview",
  "river_decay", "river_binary",
  "cropland",
  "roads_decay", "roads_binary",
  "water_decay", "water_binary",
  "accessibility_log","habitation",
  "affluence", "deprivation", "ses_gradient",
  "pop_density","abs_pop_change",
   "pop_growth","pop_decline", "pop_mag"
)

# -------------------------
# ALIGN EXTENT
# -------------------------
ref_raster <- r_list[[2]]

r_list <- lapply(r_list, function(r) {
  crop(r, ref_raster)
})

# -------------------------
# LONG FORMAT
# -------------------------
df_all <- lapply(names(r_list), function(nm) {
  r <- r_list[[nm]]
  
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  colnames(df)[3] <- "value"
  
  df$layer <- nm
  df
}) %>% bind_rows()

# -------------------------
# ORDER FACTOR
# -------------------------
df_all$layer <- factor(df_all$layer, levels = layer_order)

# -------------------------
# NORMALISE
# -------------------------
df_all$value <- scales::rescale(df_all$value, to = c(0, 1), na.rm = TRUE)

df_all <- df_all %>%
  group_by(layer) %>%
  mutate(value = (value - min(value, na.rm = TRUE)) /
           (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>%
  ungroup()

df_all$value <- sqrt(df_all$value)

cols <- rev(hcl.colors(200, "Rocket"))
cols <- rev(hcl.colors(200, "Inferno"))
country_cols <- readRDS(here("scripts","R","phylogeny-country-cols.rds"))
cols <- rev(c("#3131CC", "#4F9EEB", "#F9D461"))
# -------------------------
# PLOT
# -------------------------
ggplot() +
  
  annotation_map_tile(type = "osm") +
  
  geom_raster(
    data = df_all,
    aes(x = x, y = y, fill = value)
  ) +
  
  scale_fill_gradientn(
    colours = cols,
    limits = c(0, 1),
    oob = squish,
    na.value = "white",
    name = "Resistance",
    labels = c("Low", "High"),
    breaks = c(0, 1)
  ) +
  
  facet_wrap(
    ~layer,
    ncol = 5,
    labeller = labeller(layer = pretty_names)
  ) +
  
  coord_sf(expand = FALSE) +
  
  theme_void() +
  
  theme(
    strip.text = element_text(
      face = "bold",
      size = 9,
      colour = "black"
    ),
    
    strip.background = element_rect(
      fill = "grey85",
      colour = "grey40",
      linewidth = 0.6
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    ),
    
    legend.position = "right"
  )

base_size <- 11
p <- ggplot() +
  
  annotation_map_tile(type = "osm") +
  
  geom_raster(
    data = df_all,
    aes(x = x, y = y, fill = value)
  ) +
  
  scale_fill_gradientn(
    colours = cols,
    limits = c(0, 1),
    oob = squish,
    na.value = "white",
    name = "Resistance",
    labels = c("Low", "High"),
    breaks = c(0, 1)
  ) +
  
  facet_wrap(
    ~layer,
    ncol = 5,
    labeller = labeller(layer = pretty_names)
  ) +
  
  coord_sf(expand = FALSE) +
  
  theme_void(base_size = base_size) +
  
  theme(
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    
    strip.background = element_rect(
      fill = "grey85",
      colour = "grey40",
      linewidth = 0.6
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.4
    ),
    
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.height = unit(0.8, "cm"),
    
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "right"
  );p

# ggsave(
#   filename = here("figures","landscape_resistance_publication.pdf"),
#   plot = p,
#   width = 190,
#   height = 160,
#   units = "mm",
#   device = cairo_pdf
# )

