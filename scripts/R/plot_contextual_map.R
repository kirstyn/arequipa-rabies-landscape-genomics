## map

#############################################
#            INSTALL PACKAGES               #
#############################################
library(rnaturalearth)
library(rnaturalearthdata)
library(scatterpie)
library(sf)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(ggnewscale)
library(dplyr)
library(patchwork)   
library(RColorBrewer)
library(pals)
library(ggplot2)
library(pals)
library(wesanderson)
#############################################
#              MAPS               #
#############################################


## metadata (called annot)
annot=read.table("analysis/contextual_analysis/lac-all/la_tree_apr82026/La_complete_metadata.txt", sep="\t", header=T)

# base map of the world
world <- ne_countries(returnclass = "sf") 
lac=world[world$subregion=="South America"|world$subregion=="Central America"|world$subregion=="Caribbean",]
tiny=tiny_countries50
grenada=tiny[which(tiny$admin=="Grenada"),]

#' Find which country names do not match between data and map file
map_countries <- as.character(lac$admin)
countries <- unique(annot$sequence.m49_country.display_name)
no_match <- setdiff(countries, map_countries); message(length(no_match), " countries are mis-matched: \n", paste0(no_match, collapse="\n"))

# List all the countries included in the metadata
countries <- unique(annot$country)

# Have a quick look at the number of sequences per country, and the total number of countries
country_table<-table(annot$country); country_table
length(country_table)

sequences=lac[lac$admin %in% countries,]

sequences2=lac[lac$admin %in% c("Bolivia","Brazil", "Peru","Argentina"),]

#############################################
#              MAKE THE MAPS                #
#############################################

### LAC map

# add centroid points
lac_points<- st_point_on_surface(lac)
lac_points <- cbind(lac, st_coordinates(st_point_on_surface(lac$geometry)))

## colours for lots of data
n <-length(unique(annot$country))
# country_cols=kelly(n+3)[-c(1,2,9)]
# #country_cols=trubetskoy(n) # -1 removes the gray
# pie(rep(1, n), col=country_cols)
# names(country_cols) <- unique(annot$country)
# colScale1 <- scale_fill_manual(name ="Country",values = country_cols, na.translate=F)
n2=length(unique(annot$EPA_minor_clade))
clade_cols=trubetskoy(n2)
#clade_cols=kelly(n+3)[-c(1,2,9)]
pie(rep(1, length(unique(annot$EPA_minor_clade))), col=clade_cols)
names(clade_cols) <- unique(annot$EPA_minor_clade)
colScale2 <- scale_fill_manual(name ="Phylogenetic clade",values = clade_cols)

country_cols<- setNames(
  colorspace::desaturate(glasbey(n),amount=0.2),
  unique(annot$country)
)
colScale3 <- scale_fill_manual(name ="Country",values = country_cols)
saveRDS(country_cols, "scripts/R/phylogeny-country-cols.rds")

# Plot a basic map which we will add pies to
plot_lac<-
  ggplot(data = 
           lac) +
  geom_sf(data=lac, fill="white")+
  geom_sf(data=sequences, aes(fill=admin))+
 # colScale1+
  colScale3+
  geom_sf(data=grenada,pch=15, size=2, aes(col=admin))+
  scale_color_manual(name = "Country",values = country_cols, na.translate=F)+
  theme(panel.grid.major = element_blank())+ 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        #this bit removes the axis lables     
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_rect(fill = "white"))+
  guides(fill="none", col="none"); plot_lac
# geom_text(data=subset(lac_points, admin %in% countries),aes(x=X, y=Y, label=admin),color = "darkblue", fontface = "bold", check_overlap = F, size=3, nudge_y = -1.5)

# Subset the countries of interest
focus_countries <- c("Argentina", "Bolivia", "Brazil", "Peru")
lac_focus <- lac[lac$admin %in% focus_countries,]
lac_points_focus <- st_point_on_surface(lac_focus)
lac_points_focus <- cbind(lac_focus, st_coordinates(st_point_on_surface(lac_focus$geometry)))

# Subset sequences for these countries
sequences_focus <- sequences[sequences$admin %in% focus_countries,]

# nicer palette
wes_pal <- wes_palette("Rushmore1", 5, type = "discrete")[-1]
wes_pal
#colScale3 <- scale_fill_manual(name ="Country",values = wes_pal, na.translate=F)

# Correct font size 12 points
plot_focus_outline <- ggplot() +
  # All LAC outline in light grey
  geom_sf(data = lac, fill = NA, color = "grey80", size = 0.4) +
  # Highlight selected countries
  geom_sf(data = sequences_focus, aes(fill = admin), color = "grey30", size = 0.3, alpha=0.8) +
  colScale3 +  # re-use same colour scale
  # Add country labels with true 12pt font
  geom_text(data = lac_points_focus,
            aes(x = X, y = Y, label = admin),
            color = "black", fontface = "bold",
            size = 12 / ggplot2::.pt,  # convert points to geom_text units
            check_overlap = TRUE) +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white")) +
  guides(fill = "none", col = "none")

plot_focus_outline <- plot_focus_outline +
  theme(
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA)
  )
plot_focus_outline 
# ggsave("analysis/contextual_analysis/figures/focus_map.png",
#        plot = plot_focus_outline,
#        width = 8, height = 6, units = "in",
#        dpi = 300,  # high-res
#        bg = "transparent")  # <-- transparent background
