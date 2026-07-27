# ==============================================================================
# Title: n148-beast-skygrid.R
# Description: Skygrid plot for Arequipa city analysis
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

sky <- read.table(
  here("analysis", "BEAST_runs", "continuous-trait-runs", "n148", "peru-148-rrw.skygrid-data"),
  skip = 1,
  header = TRUE
)

# First detected case
first_case <- 2015.20

# Plot
skygrid <- ggplot(sky, aes(x = time, y = median)) +
  
  # 95% HPD interval
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = "grey70",
    alpha = 0.5
  ) +
  
  # Median population trajectory
  geom_line(
    linewidth = 0.8
  ) +
  
  # First detected case
  geom_vline(
    xintercept = first_case,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = c("1.E-2", "1.E-1", "1", "10", "100", "1000", "10000")
  ) +
  
  scale_x_continuous(
    breaks = pretty(sky$time)
  ) +
  
  labs(
    x = "Years",
    y = expression(N[e] * tau)
  ) +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 9
    ),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.border = element_rect(
      fill = NA,
      linewidth = 0.8
    )
  )

skygrid
# PDF (vector quality)
# ggsave(
#   filename = here("figures", "n148-skygrid.pdf"),
#   plot = skygrid,
#   width = 90,
#   height = 80,
#   units = "mm",
#   device = cairo_pdf
# )
# 
# # PNG (600 dpi)
# 
# ggsave(
#   filename = here("figures", "n148-skygrid.png"),
#   plot = skygrid,
#   width = 90,
#   height = 80,
#   units = "mm",
#   dpi = 600
# )
