library(patchwork)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(ggrepel)
library(tidyr)
library(patchwork)

# --- Load data ---
# Replace these with your actual CSV files
outbreak <- read_csv("analysis/outbreak_estimation/arequipa_yearly_estimates_updatedMay.csv") 
ne <-  read_table(
  "analysis/BEAST_runs/Logs/mainIntro_skygrid_rrw/aqp_mainIntro_skygrid.data3.txt",
  skip = 1)
actual_case_counts <- read_csv("raw_data/epi_metadata/rabiescases_aqp-v2.csv",name_repair = "universal")
# Convert year to numeric 
actual_case_counts <- actual_case_counts %>% 
  filter(Year != 2026)%>% mutate(Year = as.integer(Year))



# Convert year/time to numeric
#outbreak <- outbreak %>% mutate(year = as.numeric(year))
ne <- ne %>% mutate(time = as.numeric(time))
outbreak <- outbreak %>% mutate(year = as.integer(year))

country_cols <- readRDS("scripts/R/phylogeny-country-cols.rds")
# Brazil           Colombia             Mexico        Puerto Rico           Paraguay
# "#3131CC"          "#EE3838"          "#62F962"          "#030327"          "#EB43AD"
# Peru          Argentina            Bolivia               Cuba           Honduras
# "#1B511B"          "#F9D461"          "#4F9EEB"          "#92524A"          "#6AFAC5"
# El Salvador              Chile Dominican Republic            Grenada         Costa Rica
# "#724AAC"          "#469395"          "#F4B3F3"          "#B4CA85"          "#E03963"
# Guatemala              Haiti
# "#F29561"          "#C947E5"

# --- % Detection (reported/estimated) ---

detection <- outbreak %>%
  filter(year < 2025) %>%
  left_join(
    actual_case_counts %>%
      transmute(Year, reported = Total.cases.AQP.city),
    by = c("year" = "Year")
  ) %>%
  mutate(
    det_mean = (reported / annual_mean) * 100,
    det_low  = (reported / annual_upper) * 100,
    det_high = (reported / annual_lower) * 100
  )

theme_pub <- theme(
  axis.title = element_text(size = 16),
  axis.text  = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 15),
  plot.title = element_text(size = 18, face = "bold")
)

# --- MAIN PLOT ---

p_main <- ggplot() +
  
  # reported cases
  
  geom_col(
    data = actual_case_counts,
    aes(x = Year, y = Total.cases.AQP.city, fill = "Reported"),
    width = 0.9
  ) +
  
  # sequenced cases
  
  geom_col(
    data = outbreak,
    aes(x = year, y = n_seqs, fill = "Sequenced"),
    width = 0.9
  ) +
  
  scale_fill_manual(values = c(
    "Reported" = "#2C3E9F",
    "Sequenced" = "#7FB3FF"
  )) +
  
  # model uncertainty
  geom_ribbon(
    data = outbreak[outbreak$year < 2025,],
    aes(x = year, ymin = annual_lower, ymax = annual_upper),
    fill = "#444444",
    alpha = 0.2
  ) +
  
  # model estimate (MAPPED for legend)
  geom_line(
    data = outbreak[outbreak$year < 2025,],
    aes(x = year, y = annual_mean, color = "Model estimate"),
    linewidth = 1.2
  ) +
  
  scale_color_manual(values = c(
    "Model estimate" = "#444444"
  )) +
  
  scale_y_continuous(
    name = "Cases",
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000,10000),
    labels = scales::label_number()
  ) +
  
  scale_x_continuous(breaks = seq(2015, 2025, 2)) +
  labs(x = "Year") +
  guides(
    fill = guide_legend(title = NULL),
    color = guide_legend(title = NULL)
    
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme_pub
p_main <- p_main +
  theme(
    legend.position = "top"
  );p_main

# --- INSET: detection (%)

p_det <- ggplot(detection, aes(x = year, y = det_mean)) +
  geom_ribbon(
    aes(ymin = det_low, ymax = det_high),
    fill = "#F29561",
    alpha = 0.3
  ) +
  geom_line(color = "#F29561", linewidth = 1) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = NULL, y = "% detection") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    
    # ✔ full box around inset
    panel.border = element_rect(
      colour = "grey30",
      fill = NA,
      linewidth = 0.6
    ),
    
    # ticks
    axis.ticks = element_line(colour = "grey30", linewidth = 0.5),
    axis.ticks.length = unit(2, "mm"),
    
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 9),
    #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    
    plot.background = element_rect(fill = "white", colour = NA)
  );p_det

# --- COMBINE ---

p_final <- p_main +
  inset_element(
    p_det,
    left = 0.65, bottom = 0.4,
    right = 1, top = 0.7
  )

p_final

# --- Plot 2: Effective population size + model estimates ---
p2 <- ggplot() +
  # geom_ribbon(data = outbreak, 
  #             aes(x = year, ymin = annual_lower, ymax = annual_upper),
  #             fill = "skyblue", alpha = 0.3) +
  # geom_line(data = outbreak, aes(x = year, y = annual_mean), color = "blue", size = 1) +
  geom_ribbon(data = ne,
              aes(x = time, ymin = lower, ymax = upper),
              fill = "#F9D461", alpha = 0.3) +
  geom_line(data = ne, aes(x = time, y = mean), color = "#F9D461", size = 1) +
  scale_y_continuous(
    name = "Effective population size",
    trans = "log10"
  ) +
  labs(x = "Year") +  theme_minimal(base_size = 14) +   # base font larger than 12pt
  theme(
    # axis.title.y.left = element_text(color = "blue", size = 14),
    # axis.title.y.right = element_text(color = "red", size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 14)
  )


combined_plot <- (p1 + theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank())
                  + p2 + theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank())
) +
  plot_annotation(tag_levels = "A", tag_suffix = "",
                  theme = theme(plot.tag = element_text(size = 16, face = "bold"))) &
  theme(plot.margin = margin(10, 10, 10, 10))
# Display the combined figure
combined_plot

# Save as high-res PNG
ggsave(
  filename = "results/figures/outbreak-estimates-plusSkygrid.png",  # path/filename
  plot = combined_plot,
  width = 12,      # width in inches
  height = 6,      # height in inches
  dpi = 300        # high-resolution for publication
)

# Save as PDF (vector format, ideal for journals)
ggsave(
  filename = "results/figures/outbreak-estimates-plusSkygrid.pdf",
  plot = combined_plot,
  width = 12,
  height = 6
)

# Just outbreak estimates plot
# Save as high-res PNG
ggsave(
  filename = "results/figures/outbreak-estimates-only.png",  # path/filename
  plot = p_main,
  width = 12,      # width in inches
  height = 6,      # height in inches
  dpi = 600        # high-resolution for publication
)

# Save as PDF (vector format, ideal for journals)
ggsave(
  filename = "results/figures/outbreak-estimates-only.pdf",
  plot = p_main,
  width = 12,
  height = 6
)
