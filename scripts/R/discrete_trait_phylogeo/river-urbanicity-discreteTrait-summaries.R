library(tidyverse)
library(here)
source("scripts/R/discrete_trait_phylogeo/river-discrete-plot.R")
source("scripts/R/discrete_trait_phylogeo/urbanicity-discrete-plot.R")

lv <- c("North / south of river", "Urban / periurban")

# % time in each state (origin reward)
bars <- tibble(
  analysis = factor(rep(lv, each = 2), levels = lv),
  state    = factor(c("North", "South", "Urban", "Periurban"),
                    levels = c("North", "South", "Urban", "Periurban")),
  pct      = c(river_summary$reward_percent[river_summary$from == "north"],
               river_summary$reward_percent[river_summary$from == "south"],
               area_summary$reward_percent[area_summary$from == "urban"],
               area_summary$reward_percent[area_summary$from == "periurban"])
)

# Mean Markov jumps in each direction
jumps <- tibble(
  analysis = factor(rep(lv, each = 2), levels = lv),
  x    = c(1.15, 1.85, 1.15, 1.85),
  xend = c(1.85, 1.15, 1.85, 1.15),
  y    = c(95, 78, 95, 78),
  n    = c(river_summary$avg_jumps[river_summary$from == "north"],
           river_summary$avg_jumps[river_summary$from == "south"],
           area_summary$avg_jumps[area_summary$from == "urban"],
           area_summary$avg_jumps[area_summary$from == "periurban"])
)

summary_plot <- ggplot() +
  geom_col(data = bars, aes(x = state, y = pct),
           fill = "grey60", width = 0.6, colour = "white") +
  geom_segment(data = jumps,
               # aes(x = x, xend = xend, y = y, yend = y, linewidth = n), # for arrow thickness sized
               aes(x = x, xend = xend, y = y, yend = y),
               linewidth=1,
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               lineend = "round", colour = "grey20") +
  geom_text(data = jumps,
            aes(x = 1.5, y = y + 5, label = sprintf("%.0f jumps", n)),
            size = 3.4) +
  facet_wrap(~ analysis, scales = "free_x") +
  scale_linewidth(range = c(0.5, 2.2), guide = "none") +
  scale_y_continuous(limits = c(0, 105), breaks = c(0, 25, 50, 75),
                     name = "Time in state (%)") +
  labs(x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.35),
    strip.background = element_rect(fill = "grey94", colour = "grey65", linewidth = 0.35),
    strip.text = element_text(face = "bold")
  );summary_plot
summary_plot <- ggplot() +
  geom_col(data = bars, aes(x = state, y = pct),
           fill = "grey60", width = 0.6, colour = "white") +
  geom_segment(data = jumps,
               aes(x = x, xend = xend, y = y, yend = y, linewidth = n),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               lineend = "round", colour = "grey20") +
  geom_text(data = jumps,
            aes(x = 1.5, y = y + 5, label = sprintf("%.0f jumps", n)),
            size = 3.4) +
  facet_wrap(~ analysis, ncol = 1, scales = "free_x") +   # <- stacked
  scale_linewidth(range = c(0.5, 2.2), guide = "none") +
  scale_y_continuous(limits = c(0, 105), breaks = c(0, 25, 50, 75),
                     name = "Time in state (%)") +
  labs(x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.35),
    strip.background = element_rect(fill = "grey94", colour = "grey65", linewidth = 0.35),
    strip.text = element_text(face = "bold")
  )
summary_plot

ggsave(
  filename = here("figures","river-urbanicity-discreteTrait-summaries.pdf"),
  plot = summary_plot,
  width = 6,
  height = 3,
  dpi = 600
)
ggsave(
  filename = here("figures","river-urbanicity-discreteTrait-summaries.png"),
  plot = summary_plot,
  width = 6, height = 3, dpi = 600
)
