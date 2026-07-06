# ---- Load packages ----
library(ape)
library(ggplot2)
library(dplyr)
library(lubridate)
library(phytools)
library(adephylo)
library(ggtree)
library(stringr)

# ---- 1. Input files ----
tree_file <- "/Users/kirstyn.brunker/GitHub/RABV_Arequipa_2025/analysis/fasttree_trees/redcap_download_20251009_0935_n167_wOutgp.renamed.aln.ft.tre"
#tree_file <- "/Users/kirstyn.brunker/GitHub/RABV_Arequipa_2025/analysis/fasttree_trees/redcap_download_20251009_arequipa_only_n163.renamed.support.tre"
meta_file <- "/Users/kirstyn.brunker/GitHub/RABV_Arequipa_2025/processed_data/processed_metadata/081025_epi-seq_n167_stdDistrict.csv"

# ---- 2. Read in tree and metadata ----
tree <- read.tree(tree_file)
meta <- read.csv(meta_file, stringsAsFactors = FALSE)

# Trim whitespace from tree tip labels
tree$tip.label <- str_trim(tree$tip.label)

# Trim whitespace from key metadata columns
meta <- meta %>%
  mutate(
    ID = str_trim(ID),
  )
# ---- 3. Parse dates ----
meta <- meta %>%
  mutate(
    best_date = dmy(best_date),
    decimal_date = decimal_date(best_date)
  )

# ---- 4. Check which metadata entries are in the tree ----
meta <- meta %>%
  mutate(in_tree = ID %in% tree$tip.label)
cat(sum(meta$in_tree), "metadata entries matched to tree tips\n")
cat(sum(!meta$in_tree), "metadata entries NOT matched to tree tips\n")


# ---- 5. Inspect unmatched entries ----
unmatched_meta_tree <- meta %>%
  filter(!in_tree) 
unmatched_meta_tree

# ---- 5. Root tree ----
# Root using outgroup (specify tip labels of outgroup)
tree$tip.label[tree$tip.label == "KF154998.1"] <- "KF154998_af4"
# Identify tips containing "_SEA2a" or "_SEA2b"
outgroup_tips <- tree$tip.label[grep("_af4", tree$tip.label)]

# Root the tree using these tips
tree_rooted <- root(tree, outgroup = outgroup_tips, resolve.root = TRUE)

# # Root using midpoint (less preferred)
# tree_rooted <- midpoint.root(tree)
# Drop the outgroup tips from the tree
tree_final <- drop.tip(tree_rooted, outgroup_tips)
# drop puno too
puno <- tree$tip.label[grep("1203037|1101787|1101786|3553", tree$tip.label)]
tree_final <- drop.tip(tree_final, puno)


ggtree(tree_final) +
  geom_tiplab(size = 3) +
  theme_tree2() +
  ggtitle("Rooted Phylogenetic Tree (Outgroup Removed)")


# ---- 6. Calculate root-to-tip distances ----
rtt_data <- data.frame(
  tip = tree_final$tip.label,
  distance = distRoot(tree_final)
)

# ---- 7. Merge with metadata ----
rtt_meta <- rtt_data %>%
  left_join(meta, by = c("tip" = "ID")) %>%
  filter(!is.na(decimal_date))
rtt_missing_dates <- rtt_data %>%
  left_join(meta, by = c("tip" = "ID")) %>%
  filter(is.na(decimal_date))

# Summary
cat(nrow(rtt_missing_dates), "tips have no associated date\n")

# View them
rtt_missing_dates

# ---- 8. Root-to-tip regression ----
fit <- lm(distance ~ decimal_date, data = rtt_meta)
summary(fit)

# ---- Estimate root date ----
intercept <- coef(fit)[1]
slope     <- coef(fit)[2]

root_decimal_year <- -intercept / slope
root_decimal_year

# ---- 9. Plot root-to-tip regression ----
p <- ggplot(rtt_meta, aes(x = decimal_date, y = distance)) +
  geom_point(aes(color =district_std), size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Root-to-tip Regression",
    x = "Sampling Date (Decimal Year)",
    y = "Root-to-tip Distance",
    color = "Major_Island"
  ) +
  annotate("text",
           x = min(rtt_meta$decimal_date, na.rm = TRUE),
           y = max(rtt_meta$distance, na.rm = TRUE),
           hjust = 0,
           label = paste0("R² = ", round(summary(fit)$r.squared, 3),
                          "\nRate = ", signif(coef(fit)[2], 3), " subs/site/year")
  )

print(p)

# ---- 10. Save outputs ----
write.tree(tree_rooted, file = "/Users/kirstyn.brunker/GitHub/RABV_Arequipa_2025/analysis/fasttree_trees/redcap_download_20251009_0935_n167_outgpRooted.newick")
ggsave("analysis/temporal_signal/redcap_download_20251009_0935_n167_outgpRooted_RTTplot.png", p, width = 8, height = 6, dpi = 300)
write.csv(rtt_meta, "analysis/temporal_signal/redcap_download_20251009_0935_n167_outgpRooted.newick_RTTmetadata.csv", row.names = FALSE)
#write.csv(meta,  "processed_data/processed_metadata/gathered_metadata/27Oct25_gathered_metadata_n782_raddl_and_manual_Corrected_stdGeo_seqNames.csv", row.names = FALSE)

## Compare districts groups
# Root-to-tip regression
district_signals <-rtt_meta %>%
  filter(!is.na(district_std)) %>%
  group_by(district_std) %>%
  summarise(
    n = n(),
    min_year = min(decimal_date, na.rm = TRUE),
    max_year = max(decimal_date, na.rm = TRUE),
    year_span = max_year - min_year,
    rate = coef(lm(distance ~ decimal_date))[2],
    R2 = summary(lm(distance ~ decimal_date))$r.squared,
    .groups = "drop"
  )
write.csv(district_signals, "analysis/temporal_signal/redcap_download_20251009_0935_n167_outgpRooted.newick_RTTbyDistrict_metadata.csv")

p_pub <- ggplot(
  rtt_meta %>% filter(!is.na(district_std)),
  aes(x = decimal_date, y = distance, colour = district_std)
) +
  geom_point(size = 1.8, alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 12),
    plot.title   = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = "Root-to-tip Regression by Major Island Group",
    x = "Sampling date (decimal year)",
    y = "Root-to-tip genetic distance",
    colour = "Major island"
  )

print(p_pub)

ggsave(
  filename = "analysis/temporal_signal/redcap_download_20251009_0935_n167_outgpRooted.newick_RTT_by_district.svg",
  plot = p_pub,
  width = 180,
  height = 140,
  units = "mm"
)



