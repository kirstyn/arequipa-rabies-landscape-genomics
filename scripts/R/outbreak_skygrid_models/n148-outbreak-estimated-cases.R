# ==============================================================================
# Title: n148-outbreak-estimated-cases.R
# Description: Plot to show sequenced cases, actual epi case counts and model outbreak estimates (per year)
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================


source(here("scripts","R","global-packages.R"))

# --- Load data ---
# Replace these with your actual CSV files
outbreak <- read_csv(here("analysis", "outbreak_estimation", "arequipa_yearly_estimates_updatedMay.csv"))

actual_case_counts <- read_csv(here("raw_data","epi_metadata","rabiescases_aqp-v2.csv"),name_repair = "universal")

ne <- read.table(
  here("analysis", "BEAST_runs", "continuous-trait-runs", "n148", "peru-148-rrw.skygrid-data"),
  skip = 1,
  header = TRUE
)

# Convert year to numeric 
actual_case_counts <- actual_case_counts %>% 
  filter(Year != 2026)%>% mutate(Year = as.integer(Year))

# Convert numbers to integer/numeric
ne <- ne %>% mutate(time = as.numeric(time))
outbreak <- outbreak %>% mutate(year = as.integer(year))

country_cols <- readRDS(here("scripts","R","phylogeny-country-cols.rds"))

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

# ==============================================================================
# Publication theme
# ==============================================================================

theme_pub <- theme_minimal(base_size = 14) +
  theme(
    text = element_text(size = 14),
    
    axis.title = element_text(
      size = 16,
      face = "bold"
    ),
    
    axis.text = element_text(
      size = 13,
      colour = "black"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    
    legend.text = element_text(size = 13),
    
    legend.title = element_text(size = 14),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    plot.margin = margin(8, 8, 8, 8)
  )


# ==============================================================================
# MAIN PLOT: Reported cases, sequenced cases and outbreak estimates
# ==============================================================================

p_main <- ggplot() +
  
  geom_col(
    data = actual_case_counts,
    aes(
      x = Year,
      y = Total.cases.AQP.city,
      fill = "Reported"
    ),
    width = 0.85
  ) +
  
  geom_col(
    data = outbreak,
    aes(
      x = year,
      y = n_seqs,
      fill = "Sequenced"
    ),
    width = 0.85
  ) +
  
  scale_fill_manual(
    values = c(
      "Reported" = "#2C3E9F",
      "Sequenced" = "#7FB3FF"
    )
  ) +
  
  geom_ribbon(
    data = outbreak[outbreak$year < 2025, ],
    aes(
      x = year,
      ymin = annual_lower,
      ymax = annual_upper
    ),
    fill = "#444444",
    alpha = 0.2
  ) +
  
  geom_line(
    data = outbreak[outbreak$year < 2025, ],
    aes(
      x = year,
      y = annual_mean,
      color = "Model estimate"
    ),
    linewidth = 1.2
  ) +
  
  scale_color_manual(
    values = c(
      "Model estimate" = "#444444"
    )
  ) +
  
  scale_y_continuous(
    name = "Cases",
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(
      0,1,2,5,10,20,50,
      100,200,500,1000,
      2000,5000,10000
    ),
    labels = scales::label_number()
  ) +
  
  scale_x_continuous(
    breaks = seq(2015, 2025, 2)
  ) +
  
  labs(
    x = ""
  ) +
  
  guides(
    fill = guide_legend(title = NULL),
    color = guide_legend(title = NULL)
  ) +
  
  theme_pub +
  
  theme(
    legend.position = "bottom"
  );p_main


# ==============================================================================
# EFFECTIVE POPULATION SIZE
# ==============================================================================

p2 <- ggplot() +
  
  geom_ribbon(
    data = ne,
    aes(
      x = time,
      ymin = lower,
      ymax = upper
    ),
    fill = "#F9D461",
    alpha = 0.3
  ) +
  
  geom_line(
    data = ne,
    aes(
      x = time,
      y = mean
    ),
    colour = "#F9D461",
    linewidth = 1.2
  ) +
  # First detected case
  geom_vline(
    xintercept =  2015.20,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  scale_y_log10(
    name = expression(bold(N[e] * tau)),
    breaks = c(0.1,1,10,100),
    labels = c("0.1","1","10","100")
  ) +
  
  scale_x_continuous(
    breaks = seq(2011,2025,2)
  ) +
  
  labs(
    x = "Year"
  ) +
  
  theme_pub +
  
  theme(
    legend.position = "none",
    axis.title.y = element_text(
      size = 16,
      face = "bold"
    )
  );p2


# ==============================================================================
# Combine panels
# ==============================================================================

combined_plot <- 
  p_main /
  p2 +
  
  plot_layout(
    heights = c(1.4,1),
    guides = "collect"
  ) +
  
  plot_annotation(
    tag_levels = "A"
  ) &
  
  theme(
    plot.tag = element_text(
      size = 18,
      face = "bold"
    ),
    
    legend.position = "bottom",
    
    plot.margin = margin(
      10,10,10,10
    )
  )


combined_plot


# ==============================================================================
# Save publication versions
# ==============================================================================

ggsave(
  filename = here(
    "figures",
    "arequipa_outbreak_dynamics.pdf"
  ),
  plot = combined_plot,
  width = 8,
  height = 9,
  units = "in",
  device = cairo_pdf
)


ggsave(
  filename = here(
    "figures",
    "arequipa_outbreak_dynamics.png"
  ),
  plot = combined_plot,
  width = 8,
  height = 9,
  units = "in",
  dpi = 600
)

