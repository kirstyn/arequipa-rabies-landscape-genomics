library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggspatial)
library(dplyr)

# look at data

districts<- st_read("raw_data/spatial/shapefiles/Peru Shapefiles/DISTRITOS_inei_geogpsperu_suyopomalia/DISTRITOS_inei_geogpsperu_suyopomalia.shp")
provinces <- st_read("raw_data/spatial/shapefiles/Peru Shapefiles/PROVINCIAS_inei_geogpsperu_suyopomalia/PROVINCIAS_inei_geogpsperu_suyopomalia.shp")
departments <- st_read("raw_data/spatial/shapefiles/Peru Shapefiles/DEPARTAMENTOS_inei_geogpsperu_suyopomalia/DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp")

#ADM1 – Departments (Departamentos)- 25 departments (e.g., Arequipa, Puno, Cusco, Lima, Piura)

#ADM2 – Provinces (Provincias)- 196 provinces (e.g., Arequipa Province, Puno Province, Cusco Province)

#ADM3 – Districts (Distritos)- 1,874+ districts (e.g., Yanahuara District, Puno District, Miraflores District)

aqp_districts <- districts %>%
  filter(NOMBDEP == "AREQUIPA")
aqp_districts <- districts %>%
  filter(NOMBDIST == "AREQUIPA")

ggplot(aqp_districts ) +
  geom_sf(colour = "grey20", linewidth = 0.4) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

saveRDS(aqp_districts, "processed_data/gis_data/aqp_district_latest.rds")

## extract centroids
centroids <- st_centroid(aqp_districts)

# Extract coordinates
coords <- st_coordinates(centroids)

# Combine attributes and coordinates
centroid_data <- aqp_districts %>%
  st_drop_geometry() %>%
  mutate(
    Longitude = coords[, "X"],
    Latitude = coords[, "Y"]
  )

# Select and rename relevant columns for CSV
centroid_csv <- centroid_data %>%
  select(Longitude, Latitude, region=NOMBDEP, province=NOMBPROV, district=NOMBDIST, capital=CAPITAL)

# Save to CSV
write.csv(centroid_csv, "processed_data/gis_data/district_latest_centroids.csv", row.names = FALSE)

##---
names(aqp_districts)
ggplot(aqp_districts ) +
  geom_sf(aes(fill=DENSIDAD))
          
# DENSIDAD population density
# POBURBANA Urban population
# POBRURAL Rural population
# HOGARES Households




          
          
          
  scale_fill_manual(values = c(
    "AREQUIPA" = "#D55E00",  # warm rust
    "PUNO"     = "#0072B2",  # deep blue
    "Other"    = "#E8E8E8"   # soft grey
  )) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )


#----------
# Basic map highlighting puno and arequipa - "NOMBDEP"
unique(departments$NOMBDEP)  # check names if unsure

highlight <- c("AREQUIPA", "PUNO")

departments <- departments %>%
  mutate(
    highlight = ifelse(NOMBDEP %in% highlight, NOMBDEP, "Other")
  )

ggplot(departments) +
  geom_sf(aes(fill = highlight), colour = "grey20", linewidth = 0.4) +
  scale_fill_manual(values = c(
    "AREQUIPA" = "#D55E00",  # warm rust
    "PUNO"     = "#0072B2",  # deep blue
    "Other"    = "#E8E8E8"   # soft grey
  )) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

ggplot(provinces) +
  geom_sf(aes(fill = NOMBPROV
)) +
  theme(legend.position = "none")

ggplot(provinces) +
  geom_sf(fill = NA, colour = "black") +
  theme_minimal()

library(sf)
library(ggplot2)
library(dplyr)

# Select numeric columns only
num_cols <- provinces %>% select(where(is.numeric)) %>% names()

for (col in num_cols) {
  print(
    ggplot(provinces) +
      geom_sf(aes_string(fill = col)) +
      ggtitle(col) +
      theme_minimal()
  )
}


peru_outline <- st_union(districts)  # dissolve internal boundaries

ggplot() +
  geom_sf(data = peru_outline, fill = NA, color = "black", size = 0.8) +
  theme_minimal()
