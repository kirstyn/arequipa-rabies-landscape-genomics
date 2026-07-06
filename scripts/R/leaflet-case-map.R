#install.packages("leaflet")
library(leaflet)
library(mapview)
library(htmltools)
library(shiny)
library(wesanderson)
library(rgdal)
library(viridis)
# Read your metadata
data <- read.csv("processed_data/processed_metadata/081025_epi-seq_n167.csv")
data <-data %>%
  filter(!is.na(lat) & !is.na(lon))
# Clean up column names if needed (optional)
# names(data)

# Make sure lat/lon columns exist
# e.g. data$lat and data$lon — rename if necessary
# data <- data %>% rename(lat = latitude, lon = longitude)

# Convert to spatial object
points_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
#initialising options
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

#plot basic map layer
m <- leaflet(data) %>% addTiles() 


# col palette
pal <- colorNumeric(
  palette = rev(viridis(14, option = "C")),
  domain = data$year
)

aqp <- st_read("~/Github/Rabies/Shapefiles AQP/outputs/AQP_districts_city_shp.new.shp")
mainRiver <- st_read("~/Github/Rabies/Shapefiles AQP/outputs/chiliriver_shp_new.shp")

#clusteroptions clusters close cases
  m %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
  #addPolygons(data=aqp,color="#666666", popup =paste(aqp$NAME_3), fill = FALSE)%>% 
  #addPolylines(data=mainRiver,color="lightblue")%>% 
  addCircleMarkers(
    data = data, lng=~lon, lat=~lat, radius = 5, fillOpacity=0.7,popup =paste("Sample id:", data$ID, "<br>","Year:", data$year, "<br>", "District:", data$district), color=~pal(year)
  )%>%
  addLegend(
    "topright", pal = pal, values = ~year,
    title = "Year",
    opacity = 1,     
    labFormat = labelFormat(transform = identity, big.mark = "", digits = 0)
  )

#mapshot(mappedWGS, url = paste0(getwd(), "/output/maps/mappedWGS.html"), file=paste0(getwd(), "/output/maps/mappedWGS_districtBorder.png"))


m %>% 
  addPolygons(data=aqp, color="black", popup =paste(aqp$NAME_3))%>% 
  addPolylines(data=mainRiver,color="red")%>%
  addCircleMarkers(
    data = data, lng=~lon, lat=~lat, radius = 4, fillOpacity=1,popup =paste("Sample id:", data$ID, "<br>","Year:", data$year, "<br>", "District:", data$district), fillColor="black")
