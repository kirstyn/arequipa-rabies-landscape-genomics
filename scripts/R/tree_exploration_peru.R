library(ape)
library(ggtree)
library(dplyr)
library(ggplot2)

# Read tree
tree <- read.tree("peru/150825_filt_seq_wOutgp_fasttree.nwk")

# Root tree by outgroup
tree_rooted <- root(tree, outgroup = "KF154998.1", resolve.root = TRUE)

# Example tip data
tip_data <- data.frame(
  tip = tree_rooted$tip.label
) %>%
  mutate(year = as.numeric(sub(".*_(\\d{4})$", "\\1", tip)),
         group = case_when(
           year <= 2016 ~ "2015-2016",
           year <= 2019 ~ "2017-2019",
           year <= 2021 ~ "2020-2021",
           TRUE ~ "2022-2024"
         ))

# Plot rooted tree with tip coloring
p <- ggtree(tree_rooted) %<+% tip_data +
  geom_tiplab(aes(color = group), size=3) +
  theme_tree2() +
  scale_color_brewer(palette="Set1")

print(p)

library(ggtree)
library(ggplot2)
library(dplyr)

# choose thresholds (edit as you like)
thr_small <- 1e-6        # ~0.000001 (captures your ~6e-9 "almost identical")
thr_large <- 7e-4        # ~0.0007 (your “noticeably diverged”)

p <- ggtree(tree_rooted)
df <- p$data |>
  mutate(bl = branch.length,
         bl_cat = case_when(
           is.na(bl)            ~ "NA",
           bl <= thr_small      ~ "tiny (≤1e-6)",
           bl >= thr_large      ~ "large (≥7e-4)",
           TRUE                 ~ "medium"
         ))

p_col <- p + 
  geom_tree(data = df, aes(colour = bl_cat), linewidth = 0.6) +
  geom_tiplab(data = df[df$isTip, ], aes(label = label), size = 3) +
  scale_colour_manual(values = c("tiny (≤1e-6)" = "#2c7bb6",
                                 "medium"       = "#aaaaaa",
                                 "large (≥7e-4)"= "#d7191c",
                                 "NA"           = "#000000")) +
  guides(colour = guide_legend(title = "Branch length")) +
  theme_tree2() 
print(p_col)
