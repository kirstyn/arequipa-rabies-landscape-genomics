
# ==============================================================================
# DIVERGING BAR PLOT: BARRIERS vs FACILITATORS
# ==============================================================================

library(tidyverse)
library(ggplot2)
install.packages("ggh4x")
library(ggh4x)

# Convert BF evidence strength to an ordinal score
# 0 = weak evidence; 1–4 = moderate to decisive evidence

diverging_plot <- ggplot(
  df_diverging,
  aes(x = signed_strength, y = factor, fill = direction)
) +
  # Centre: no evidence; dashed lines: boundaries of very strong / decisive evidence
  geom_vline(
    xintercept = 0,
    colour = "grey25",
    linewidth = 0.55
  ) +
  # geom_vline(
  #   xintercept = c(-4, 4),
  #   colour = "grey65",
  #   linewidth = 0.65,
  #   linetype = "dashed"
  # )+
  # geom_vline(
  #   xintercept = c(-3, 3),
  #   colour = "grey65",
  #   linewidth = 0.65,
  #   linetype = "dotted"
  # )+
  geom_col(
    width = 0.68,
    position = "identity",
    colour = "white",
    linewidth = 0.2
  ) +
  ggh4x::facet_nested(
    rows = vars(group),
    cols = vars(k, type),
    scales = "free_y",
    space = "free_y",
    switch = "y",
    nest_line = element_line(colour = "grey35", linewidth = 0.6)
  ) +
  scale_x_continuous(
    limits = c(-4.5, 4.5),
    breaks = -4:4,
    labels = c(
      "Decisive", "Very strong", "Strong", "Moderate",
      "Weak",
      "Moderate", "Strong", "Very strong", "Decisive"
    ),
    name = "Evidence strength (Bayes factor category)"
  ) +
  scale_fill_manual(
    values = c(
      "Barrier" = "#D9A62E",
      "Facilitator" = "#344C9A"
    ),
    name = "Landscape association"
  ) +
  labs(y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      colour = "grey80",
      fill = NA,
      linewidth = 0.35
    ),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5),
    strip.placement = "outside",
    strip.background = element_rect(
      fill = "grey94",
      colour = "grey65",
      linewidth = 0.35
    ),
    strip.text = element_text(face = "bold"),
    strip.text.y.left = element_text(angle = 90, face = "bold"),
    panel.spacing = unit(0.55, "lines"),
    panel.spacing.x = unit(0.35, "lines"),
    legend.position = "bottom"
  )

diverging_plot

df_diverging_100 <- df_diverging %>%
  filter(as.character(k) == "100") %>%
  mutate(
    type = factor(
      recode(as.character(type),
             "E" = "Persistence (E)",
             "R" = "Movement (R)"),
      levels = c("Persistence (E)", "Movement (R)")
    )
  )

df_diverging_100 <- df_diverging %>%
  filter(as.character(k) == "100") %>%
  mutate(
    type = factor(
      recode(as.character(type),
             "E" = "Persistence (E)",
             "R" = "Movement (R)"),
      levels = c("Persistence (E)", "Movement (R)")
    )
  )

diverging_plot_k100 <- ggplot(
  df_diverging_100,
  aes(x = signed_strength, y = factor, fill = direction)
) +
  # Centre line: no evidence either way
  geom_vline(
    xintercept = 0,
    colour = "grey25",
    linewidth = 0.55
  ) +
  geom_col(
    width = 0.68,
    position = "identity",
    colour = "white",
    linewidth = 0.2
  ) +
  ggh4x::facet_nested(
    rows = vars(group),
    cols = vars(type),
    scales = "free_y",
    space = "free_y",
    switch = "y",
    nest_line = element_line(colour = "grey35", linewidth = 0.6)
  ) +
  scale_x_continuous(
    limits = c(-4.5, 4.5),
    breaks = -4:4,
    labels = c(
      "Decisive", "Very strong", "Strong", "Moderate",
      "Weak",
      "Moderate", "Strong", "Very strong", "Decisive"
    ),
    name = "Evidence strength (Bayes factor category)"
  ) +
  scale_fill_manual(
    values = c(
      "Barrier" = "#D9A62E",
      "Facilitator" = "#344C9A"
    ),
    name = "Landscape association"
  ) +
  labs(y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      colour = "grey80",
      fill = NA,
      linewidth = 0.35
    ),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5),
    strip.placement = "outside",
    strip.background = element_rect(
      fill = "grey94",
      colour = "grey65",
      linewidth = 0.35
    ),
    strip.text = element_text(face = "bold"),
    strip.text.y.left = element_text(
      angle = 0,
      face = "bold",
      size = 9,
      margin = margin(t = 4, r = 6, b = 4, l = 6)
    ),
    panel.spacing = unit(0.55, "lines"),
    panel.spacing.x = unit(0.35, "lines"),
    legend.position = "bottom"
  );diverging_plot_k100 

ggsave(
  filename = "figures/seraphim-diverging-k100.pdf",
  plot = diverging_plot_k100,
  width = 14,
  height = 5,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  filename = "figures/seraphim-diverging-k100.png",
  plot = diverging_plot_k100,
  width = 14,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

df_k <- df_diverging %>%
  mutate(
    k = factor(as.character(k), levels = c("10", "100", "1000")),
    type = factor(
      recode(as.character(type),
             "E" = "Persistence (E)",
             "R" = "Movement (R)"),
      levels = c("Persistence (E)", "Movement (R)")
    )
  )
df_k <- df_k %>%
  mutate(
    k_direction = interaction(k, direction, sep = "\n")
  )
k_plot <- ggplot(
  df_k,
  aes(x = k_direction, y = factor, fill = signed_strength)
)
k_plot <- ggplot(
  df_k,
  aes(x = k_direction, y = factor, fill = signed_strength)
) +
  
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_x_discrete(
    labels = c("10", "100", "1000", "10", "100", "1000"),
    name = expression(italic(k))
  )+
  ggh4x::facet_nested(
    rows = vars(group),
    cols = vars(type),
    scales = "free_y",
    space = "free_y",
    switch = "y",
    nest_line = element_line(colour = "grey35", linewidth = 0.6)
  ) +
  scale_fill_gradient2(
    low = "#D9A62E", mid = "white", high = "#344C9A",
    midpoint = 0, limits = c(-4, 4),
    breaks = c(-4, -2, 0, 2, 4),
    labels = c("Decisive\nbarrier", "Strong\nbarrier", "No\nevidence",
               "Strong\nfacilitator", "Decisive\nfacilitator"),
    name = NULL
  ) +
  labs(x = "Resistance scaling factor (k)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.35),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle=45),
    strip.placement = "outside",
    # Keep the top strips
    strip.text.x = element_text(face = "bold"),
    strip.background.x = element_rect(
      fill = "grey94",
      colour = "grey65",
      linewidth = 0.35
    ),
    # Remove only left-side group strips
    strip.text.y.left = element_blank(),
    strip.background.y = element_blank(),
    strip.switch.pad.grid = unit(0, "pt"),
    legend.position = "bottom",
    legend.key.width = unit(1.6, "cm"),
    axis.title.x = element_text(
      margin = margin(t = 2)
    ),
    legend.box.spacing = unit(2, "pt"),
    legend.box.margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0
    ),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 1,
      l = 5
    )
  );k_plot

ggsave(
  filename = "figures/seraphim-kvalues-heatmap.pdf",
  plot = k_plot,
  width = 6,
  height = 5,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  filename = "figures/seraphim-kvalues-heatmap.png",
  plot = k_plot,
  width = 6,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)
# ==============================================================================
# DIVERGING DOT PLOT: barriers left, facilitators right; colour = k
# ==============================================================================

# Keep the strongest BF category within each factor/k/model-type/direction.
# This preserves the possibility that a factor has both barrier and facilitator
# evidence within the same E or R model.
df_dots <- df_long %>%
  mutate(
    evidence = case_when(
      BF_plot >= 100 ~ 4L,  # decisive
      BF_plot >= 30  ~ 3L,  # very strong
      BF_plot >= 10  ~ 2L,  # strong
      BF_plot >= 3   ~ 1L,  # moderate
      TRUE           ~ 0L   # weak
    )
  ) %>%
  group_by(factor, group, k, type, direction) %>%
  summarise(
    evidence = max(evidence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    signed_evidence = if_else(
      direction == "Barrier",
      -evidence,
      evidence
    ),
    k = factor(k, levels = c("10", "100", "1000"))
  ) %>%
  # Do not plot BF < 3: otherwise barrier and facilitator points both sit at zero.
  filter(evidence > 0)

diverging_dots <- ggplot(
  df_dots,
  aes(
    x = signed_evidence,
    y = factor,
    colour = k,
    group = k
  )
) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.45,
    colour = "grey35"
  ) +
  geom_vline(
    xintercept = c(-3.5, -2.5, -1.5, 1.5, 2.5, 3.5),
    linewidth = 0.3,
    linetype = "dashed",
    colour = "grey82"
  ) +
  geom_point(
    position = position_dodge(width = 0.55),
    size = 3.2,
    alpha = 0.9
  ) +
  facet_grid(
    group ~ type,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_colour_manual(
    values = c(
      "10" = "#0072B2",
      "100" = "#E69F00",
      "1000" = "#009E73"
    ),
    name = expression(italic(k))
  ) +
  scale_x_continuous(
    limits = c(-4.5, 4.5),
    breaks = -4:4,
    labels = c(
      "Decisive",
      "Very strong",
      "Strong",
      "Moderate",
      "No evidence",
      "Moderate",
      "Strong",
      "Very strong",
      "Decisive"
    ),
    name = "Evidence strength (Bayes factor category)"
  ) +
  labs(
    y = NULL,
    subtitle = "Barriers \u2190                                   Facilitators \u2192"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.8, "lines"),
    strip.placement = "outside",
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text.x = element_text(face = "bold", size = 14),
    strip.text.y.left = element_text(
      angle = 90,
      face = "bold",
      size = 13
    ),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10,angle=90),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

diverging_dots

df_dots <- df_dots %>%
  mutate(
    k_num = as.numeric(as.character(k)),
    k_offset = case_when(
      k_num == 10   ~ -0.18,
      k_num == 100  ~  0,
      k_num == 1000 ~  0.18
    ),
    factor_position = as.numeric(factor) + k_offset
  ) %>%
  arrange(group, type, factor, direction, k_num)

factor_labels <- levels(df_dots$factor)

connected_dots <- ggplot(
  df_dots,
  aes(x = signed_evidence, y = factor_position)
) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.45,
    colour = "grey35"
  ) +
  geom_vline(
    xintercept = c(-3.5, -2.5, -1.5, 1.5, 2.5, 3.5),
    linewidth = 0.3,
    linetype = "dashed",
    colour = "grey82"
  ) +
  geom_path(
    aes(group = interaction(factor, direction)),
    colour = "grey65",
    linewidth = 0.55,
    lineend = "round"
  ) +
  geom_point(
    aes(colour = k),
    size = 3.2,
    alpha = 0.95
  ) +
  facet_grid(
    group ~ type,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_colour_manual(
    values = c(
      "10" = "#0072B2",
      "100" = "#E69F00",
      "1000" = "#009E73"
    ),
    name = expression(italic(k))
  ) +
  scale_x_continuous(
    limits = c(-4.5, 4.5),
    breaks = -4:4,
    labels = c(
      "Decisive", "Very strong", "Strong", "Moderate",
      "No evidence",
      "Moderate", "Strong", "Very strong", "Decisive"
    ),
    name = "Evidence strength (Bayes factor category)"
  ) +
  scale_y_continuous(
    breaks = seq_along(factor_labels),
    labels = factor_labels,
    expand = expansion(add = 0.5)
  ) +
  labs(
    y = NULL,
    subtitle = "Barriers \u2190                                   Facilitators \u2192"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.8, "lines"),
    strip.placement = "outside",
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text.x = element_text(face = "bold", size = 14),
    strip.text.y.left = element_text(angle = 90, face = "bold", size = 13),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

connected_dots
