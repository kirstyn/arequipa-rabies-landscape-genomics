# Load required packages
library(sf)
library(dplyr)

# 1️⃣ Load your case data
cases <- read.csv("cases_with_coords.csv")  # must have latitude and longitude columns

# Convert to sf object (points)
cases_sf <- st_as_sf(cases, coords = c("longitude", "latitude"), crs = 4326)

# 2️⃣ Load the river shapefile
river <- st_read("Arequipa_River.shp")  # should be a polygon or line representing the river

# 3️⃣ Create north/south polygons
# If river is a line, create two polygons (north and south)
# We'll make a bounding box around the city and split along the river line
city_bbox <- st_as_sfc(st_bbox(cases_sf))  # bounding box of all points
city_poly <- st_as_sf(city_bbox)

# Split the city polygon by river line
# Note: if river is a polygon, you can use st_difference
south_poly <- st_difference(city_poly, river)  # area not river
north_poly <- river  # north polygon = river polygon (or flip if needed)

# 4️⃣ Classify points
cases_sf <- cases_sf %>%
  mutate(
    river_side = case_when(
      st_intersects(cases_sf, north_poly, sparse = FALSE)[,1] ~ "North",
      st_intersects(cases_sf, south_poly, sparse = FALSE)[,1] ~ "South",
      TRUE ~ NA_character_
    )
  )

# 5️⃣ Optional: Check visually
plot(st_geometry(city_poly), col="lightgrey", main="Cases by River Side")
plot(st_geometry(south_poly), add=TRUE, col="lightblue")
plot(st_geometry(north_poly), add=TRUE, col="lightgreen")
plot(st_geometry(cases_sf[cases_sf$river_side=="North",]), add=TRUE, col="red", pch=16)
plot(st_geometry(cases_sf[cases_sf$river_side=="South",]), add=TRUE, col="blue", pch=16)

# 6️⃣ Save updated metadata
st_write(cases_sf, "cases_with_river_side.shp")  # optional shapefile
write.csv(st_drop_geometry(cases_sf), "cases_with_river_side.csv", row.names = FALSE)