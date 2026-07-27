# ==============================================================================
# Title: n148-seraphim-diffusion-heatmap.R
# Description: Plot results from seraphim landscape model testing 
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))
# -------------------------
# LOAD DATA
# -------------------------
df <- read_excel(here("analysis","seraphim","outputs","Collective_results.xlsx"),
                 sheet = "1kmbuff_grid")

# -------------------------
# CLEAN DATA
# -------------------------
df <- df %>%
  rename(
    factor = `Environmental factor`,
    k = k,
    E_barrier = `E: barrier`,
    E_facilitator = `E: facilitator`,
    R_barrier = `R: barrier`,
    R_facilitator = `R: facilitator`
  ) %>%
  mutate(
    k = as.numeric(str_remove(k, "k")),
    across(c(E_barrier, E_facilitator, R_barrier, R_facilitator), as.numeric)
  )

# -------------------------
# RESHAPE
# -------------------------
df_long <- df %>%
  pivot_longer(
    cols = c(E_barrier, E_facilitator, R_barrier, R_facilitator),
    names_to = "model",
    values_to = "BF"
  ) %>%
  mutate(
    # -------------------------
    # KEEP INF BUT MAKE PLOTTABLE
    # -------------------------
    BF_plot = ifelse(is.infinite(BF), 150, BF),
    
    model = recode(model,
                   E_barrier = "Barrier (E)",
                   R_barrier = "Barrier (R)",
                   E_facilitator = "Facilitator (E)",
                   R_facilitator = "Facilitator (R)"
    ),
    
    model = factor(model,
                   levels = c(
                     "Barrier (E)",
                     "Barrier (R)",
                     "Facilitator (E)",
                     "Facilitator (R)"
                   )
    ),
    
    k = factor(k),
    
    # -------------------------
    # BF CLASSIFICATION (WITH DECISIVE)
    # -------------------------
    BF_class = case_when(
      BF_plot < 3 ~ "Weak (<3)",
      BF_plot < 10 ~ "Moderate (3–10)",
      BF_plot < 30 ~ "Strong (10–30)",
      BF_plot <= 100 ~ "Very strong (30–100)",
      BF_plot > 100 ~ "Decisive (>100)"
    ),
    
    BF_class = factor(BF_class,
                      levels = c(
                        "Weak (<3)",
                        "Moderate (3–10)",
                        "Strong (10–30)",
                        "Very strong (30–100)",
                        "Decisive (>100)"
                      )
    )
  )

# remove sensitivity related results
df_long <- df_long %>%
  filter(!factor %in% c(
    "SES gradient, bkg 1",
    "SES gradient, bkg penalty",
    "Relative Pop change , Zexp",
    "Combined pop change",
    "Relative Pop change"
  )) %>%
  mutate(
    factor = recode(factor,
                    "Pop density 2024" = "Human pop density",
                    "Pop growth" = "Pop change: growth",
                    "Pop decline" = "Pop change: decline",
                    "Pop magnitude" = "Pop change: magnitude",
                    "Absolute Pop change"="Pop change: absolute",
                    "Affluence, bkg NA" = "Affluence",
                    "Deprivation, bkg NA" = "Deprivation",
                    "SES gradient, bkg NA" = "SES gradient"
    )
  )

# -------------------------
# FACTOR ORDER
# -------------------------
df_long$factor <- factor(df_long$factor,
                         levels = c(
                           "Major river (decay)",
                           "Major river (binary)",
                           "Croplands",
                           "Major roads (decay)",
                           "Major roads (binary)",
                           "Water Channel (decay)",
                           "Water Channel (binary)",
                           "Habitation",
                           "Affluence",
                           "Deprivation",
                           "SES gradient",
                           "Accessibility time, raw",
                           "Accessibility time, log",
                           "Pop density",
                           "Human pop density",
                           "Pop change: absolute",
                           "Pop change: growth",
                           "Pop change: decline",
                           "Pop change: magnitude"
                         )
)
# -------------------------
# GROUPS
# -------------------------

df_long <- df_long %>%
  mutate(group = case_when(
    str_detect(factor, "river|Water Channel|roads") ~ "Movement barriers",
    str_detect(factor, "Croplands|Habitation") ~ "Land cover",
    str_detect(factor, "SES|Affluence|Deprivation") ~ "Socioeconomic context",
    str_detect(factor, "Accessibility time") ~ "Accessibility",
    str_detect(factor, "pop density|Pop density|Pop change") ~ "Population dynamics",
    TRUE ~ "Other"
  ))
df_long$group <- factor(df_long$group,
                        levels = c(
                          "Movement barriers",
                          "Land cover",
                          "Accessibility",
                          "Socioeconomic context",
                          "Population dynamics",
                          "Other"
                        ))

# -------------------------
# TYPE + DIRECTION
# -------------------------
df_long <- df_long %>%
  mutate(
    model = str_trim(model),
    
    type = case_when(
      str_detect(model, "\\(E\\)") ~ "E",
      str_detect(model, "\\(R\\)") ~ "R"
    ),
    
    direction = case_when(
      str_detect(model, "Barrier") ~ "Barrier",
      str_detect(model, "Facilitator") ~ "Facilitator"
    )
  )

df_barrier <- df_long %>% filter(direction == "Barrier")
df_facilitator <- df_long %>% filter(direction == "Facilitator")


country_cols <- readRDS(here("scripts","R","phylogeny-country-cols.rds"))

## combined heatmap

df_plot <- df_long %>%
  mutate(
    score = case_when(
      direction == "Barrier" & BF_plot >= 100 ~ -5,
      direction == "Barrier" & BF_plot >= 30  ~ -4,
      direction == "Barrier" & BF_plot >= 10  ~ -3,
      direction == "Barrier" & BF_plot >= 3   ~ -2,
      
      direction == "Facilitator" & BF_plot >= 100 ~ 5,
      direction == "Facilitator" & BF_plot >= 30  ~ 4,
      direction == "Facilitator" & BF_plot >= 10  ~ 3,
      direction == "Facilitator" & BF_plot >= 3   ~ 2,
      
      TRUE ~ 0
    )
  ) %>%
  group_by(factor, group, k, type) %>%
  summarise(score = score[which.max(abs(score))],
            .groups = "drop")

combined_heatmap <- ggplot(df_plot,
       aes(type, factor, fill = score)) +
  geom_tile(colour = "white") +
  facet_grid(group ~ k,
             switch = "y",
             scales = "free_y",
             space = "free_y") +
  scale_fill_gradient2(
    low = "#fff1a8",
    mid = "white",
    high = "#1a1a70",
    midpoint = 0,
    limits = c(-5, 5),
    breaks = c(-5, -4, -3, -2, 0, 2, 3, 4, 5),
    labels = c(
      ">100",
      "30–100",
      "10–30",
      "3–10",
      "Weak (<3)",
      "Moderate (3–10)",
      "Strong (10–30)",
      "Very strong (30–100)",
      "Decisive (>100)"
    ),
    name = "<b>Bayes Factor</b><br><i>Blue = Facilitator<br>Yellow = Barrier</i>"
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(face = "bold",size=14),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(face = "bold"),
    panel.spacing.y = unit(0.8, "lines"),
    strip.background = element_rect(fill = "grey95", colour = NA), 
    strip.text.y.right = element_text(angle = 0, face = "bold")
  ) +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, face = "bold", size=14),
    strip.text.y.right = element_text(angle = 0, face = "bold",size=14)
  )+
      guides(
        fill = guide_colourbar(
          title.position = "top",
          barwidth = unit(0.4, "cm"),
          barheight = unit(5, "cm"),   # <- increase length here
          ticks = TRUE
        )
      )+
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_markdown(size = 14)); combined_heatmap

# ggsave(
#   filename = "results/figures/seraphim-diffusion-combined-heatmaps.png",
#   plot = combined_heatmap,
#   width = 14,
#   height = 10,
#   units = "in",
#   dpi = 600,
#   bg = "white"
# )
# 
# ggsave(
#   filename = "results/figures/seraphim-diffusion-combined-heatmaps.pdf",
#   plot = combined_heatmap,
#   width = 14,
#   height = 10,
#   units = "in",
#   device = cairo_pdf
# )

