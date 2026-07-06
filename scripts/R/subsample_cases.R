# Load libraries
library(sf)
library(dplyr)
library(readxl)
library(lubridate)
library(incidence)
library(ggplot2)
library(cowplot)
library(wesanderson)

#-----------------------------
# Load shapefile and data
aqp <- st_read("raw_data/spatial/shapefiles/AQP_AQP_province_districts.shp")

all <- read_excel("raw_data/epi_metadata/forsampleselection_sep2025.xls", sheet = 1)%>%
  rename(X = `...4`)

df_per_80$sample_id %in% all$X
df_per_80$sample_id[!df_per_80$sample_id %in% epi$X]

#-----------------------------
# Create best_date column (priority order)
all <- all %>%
  mutate(across(c(symptom_date, complaint_date, kill_date,
                  sample_date, ship_date, contain_date),
                ~ suppressWarnings(ymd(.))),
         best_date = coalesce(symptom_date, complaint_date, kill_date,
                              sample_date, ship_date, contain_date))

#-----------------------------
# Convert dates to epi-months for subsampling
all$best_date <- as.Date(all$best_date)  # ensure Date class
origin <- min(all$best_date, na.rm = TRUE)
all$epi.month <- interval(origin, all$best_date) %/% months(1)

quarters <- seq(min(all$epi.month), max(all$epi.month), by = 4)
if(max(all$epi.month) != max(quarters)){
  quarters <- c(quarters, max(all$epi.month))
}

#-----------------------------
# Ensure your palette includes a "*new*" category
pal <- wes_palette(n = 3, name = "GrandBudapest1")
seq_colors <- c("N" = pal[1], "Y" = pal[2], "*new*" = pal[3])

# Transform your points to match shapefile CRS
all_sf <- all %>%
  mutate(
    lon = as.numeric(na_if(lon, "NA")),
    lat = as.numeric(na_if(lat, "NA"))
  ) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(aqp))

# Crop shapefile to points extent
aqp_crop <- st_crop(aqp, st_bbox(all_sf))

df <- data.frame()

for (i in 1:(length(quarters)-1)) {
  dir.create("processed_data/case_subsampling_output/quarterly_subsampling", showWarnings = FALSE)
  
  # Subset for this quarter
  sub <- all_sf %>%
    filter(epi.month >= quarters[i] & epi.month <= quarters[i+1])
  
  # Proportion already sequenced
  sampled.prop <- mean(sub$`sequenced?` == "Y", na.rm = TRUE)
  
  if (is.na(sampled.prop) || sampled.prop > 0.5) next
  
  # Remove duplicates in unsequenced subset
  sub.unseq <- sub %>%
    filter(`sequenced?` != "Y") %>%
    distinct(X, .keep_all = TRUE)
  
  # Number to sample
  prop <- ceiling((0.5 - sampled.prop) * nrow(sub))
  
  if (prop >= 1 && nrow(sub.unseq) >= 1) {
    # Probabilistic subsampling
    q.sub <- pp.subsample(sub.unseq, n = prop)
    new <- sub.unseq[as.integer(row.names(q.sub)), ]
  } else {
    new <- sub.unseq
  }
  
  # Mark new samples
  sub$`sequenced?`[sub$X %in% new$X] <- "*new*"
  sub$`sequenced?` <- factor(sub$`sequenced?`, levels = c("N", "Y", "*new*"))
  
  # Add to cumulative df, ensuring no duplicates
  df <- rbind(df, as.data.frame(new)) %>%
    distinct(X, .keep_all = TRUE)
  
  # --------------------
  # Plotting (optional, same as before)
  p1 <- plot(incidence(all$best_date, interval = "month", groups = all$`sequenced?`), stack = TRUE) +
    scale_fill_manual(values = seq_colors) +
    annotate("rect", xmin = min(sub$best_date), xmax = max(sub$best_date),
             ymin = 0, ymax = Inf, fill = NA, color = "black", linetype = "dotted") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45))
  
  p3 <- plot(incidence(sub$best_date, interval = "week", groups = sub$`sequenced?`), stack = TRUE) +
    scale_fill_manual(values = seq_colors) +
    theme(axis.text.x = element_text(angle = 45), legend.position = "none")
  
  p2 <- ggplot() +
    geom_sf(data = aqp_crop, colour = "gray", fill = NA) +
    geom_sf(data = sub, aes(color = `sequenced?`)) +
    scale_color_manual(values = seq_colors, name = "Sequenced") +
    theme_void()
  
  top_row <- plot_grid(p1, p3)
  pdf(paste0("processed_data/case_subsampling_output/quarterly_subsampling/q", i, ".pdf"))
  print(plot_grid(top_row, p2, ncol = 1))
  dev.off()
}

write.csv(df %>% distinct(X, .keep_all = TRUE), 
          "processed_data/case_subsampling_output/quarterly_subsampling/toSequence.csv",
          row.names = FALSE)

# Full incidence + map for all cases
temp <- all_sf
temp$`sequenced?`[temp$X %in% df$X] <- "*new*"
temp$`sequenced?` <- factor(temp$`sequenced?`, levels = c("N", "Y", "*new*"))

p4 <- plot(incidence(temp$best_date, interval = "month", groups = temp$`sequenced?`), stack = TRUE) +
  scale_fill_manual(values = seq_colors) +
  theme(axis.text.x = element_text(angle = 45))

p5 <- ggplot() +
  geom_sf(data = aqp_crop, colour = "gray", fill = NA) +
  geom_sf(data = temp, aes(color = `sequenced?`)) +
  scale_color_manual(values = seq_colors) +
  theme_void() +
  theme(legend.position = "none")
pdf(file.path(out_dir, "all_withNewSampling.pdf"))
print(plot_grid(p4, p5, ncol = 1))
dev.off()