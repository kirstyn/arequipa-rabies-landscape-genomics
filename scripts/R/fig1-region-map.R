library(sf)
library(ggplot2)
library(dplyr)
library(geodata)
library(ggrepel)
library(ggspatial)

# Level 1 = regions
peru_adm1 <- geodata::gadm(country = "PER", level = 1, path = tempdir()) |> 
  st_as_sf()

# Level 2 (optional, for more detail)
peru_adm2 <- geodata::gadm(country = "PER", level = 2, path = tempdir()) |> 
  st_as_sf()

regions <- peru_adm1 %>%
  filter(NAME_1 %in% c("Arequipa", "Puno"))

locations <- data.frame(
  name = c("Arequipa City", "El Pedregal", "Camaná"),
  lon = c(-71.5375, -72.0556, -72.7136),
  lat = c(-16.4090, -16.2333, -16.6230)
)

locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
locations_df <- locations_sf %>%
cbind(st_coordinates(.))
geo_cols <- readRDS("scripts/R/geo_cols.rds")
# Example (adapt to your actual names)
# 
# Cerro Colorado             Alto Selva Alegre                     Mollebaya
# "#3131CC"                     "#EE3838"                     "#62F962"
# El Pedregal                          Yura                          Puno
# "#030327"                     "#EB43AD"                     "#1B511B"
# Cayma                 Jacobo Hunter                Mariano Melgar
# "#F9D461"                     "#4F9EEB"                     "#92524A"
# José Luis Bustamante y Rivero                    Paucarpata                     Characato
# "#6AFAC5"                     "#724AAC"                     "#469395"
# Miraflores                      Socabaya                       Sachaca
# "#F4B3F3"                     "#B4CA85"                     "#E03963"
# Uchumayo
# "#F29561"

region_labels <- peru_adm1 %>%
  filter(NAME_1 %in% c("Arequipa", "Puno")) %>%
  sf::st_centroid()


ggplot() +
  
  # Base Peru
  geom_sf(data = peru_adm1, fill = "grey95", colour = "grey80", linewidth = 0.3) +
  
  # Regions
  geom_sf(data = peru_adm1 %>% filter(NAME_1 == "Arequipa"),
          fill = NA, colour = "#4F9EEB", linewidth = 1.2) +
  
  geom_sf(data = peru_adm1 %>% filter(NAME_1 == "Puno"),
          fill = "#1B511B", alpha = 0.5, colour = NA) +
  
  # Region labels
  geom_sf_text(data = region_labels,
               aes(label = toupper(NAME_1)),
               size = 6,
               fontface = "bold",
               colour = "grey10") +
  
  # Points
  geom_sf(data = locations_sf %>% filter(name == "Arequipa City"),
          colour = "#3131CC", size = 3.5) +
  
  geom_sf(data = locations_sf %>% filter(name == "Camaná"),
          colour = "#EE3838", size = 3.5) +
  
  geom_sf(data = locations_sf %>% filter(name == "El Pedregal"),
          colour = "#FF85FF", size = 3) +
  
  # City labels (PUBLICATION SIZE)
  geom_text_repel(
    data = locations_df,
    aes(X, Y, label = name),
    size = 6,
    box.padding = 0.4,
    point.padding = 0.3,
    min.segment.length = 0,
    seed = 1,
    max.overlaps = Inf
  )+
  
  coord_sf(xlim = c(-75, -68), ylim = c(-18, -13)) +
  
  theme_void(base_size = 14) +
  
  # Scale bar (bigger + readable)
  annotation_scale(
    location = "bl",
    width_hint = 0.35,
    text_cex = 1.2,
    line_width = 0.8
  ) +
  
  # North arrow (bigger for print)
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering(),
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm"),
    pad_y = unit(1.2, "cm")
  )

ggsave(
  filename = "results/figures/peru_map_timeline.pdf",
  width = 180,   # mm (single-column figure)
  height = 140,  # adjust if needed
  units = "mm",
  dpi = 600
)

