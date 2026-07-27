# ==============================================================================
# Title: n167-genetic-distances.R
# Description: Sequence divergence and diversity of the full dataset (167 wgs)
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

# -----------------------------
# Load required libraries
# -----------------------------
library(ape)
library(phangorn)
library(pegas)
library(ggplot2)
library(treeio)

# -----------------------------
# 1. Read DNA alignment
# -----------------------------
alignment <- read.dna(here(
  "processed_data","nt_alignments","gene_alignments","redcap_download_20251009_0935_n167.renamed.aln.date_appended.fasta"),
  format = "fasta"
)

# -----------------------------
# 2. Define clusters
# -----------------------------
pedregal_seq_names <- grep(
  "^(147_2021|52_2021|32_2021|250_2021|251_2021|73_2021|71_2021|107_2021|202_2021|68_2021|312_2019|289_2019|225_2019)",
  labels(alignment),
  value = TRUE
)

other_introductions <- c("219_2024_2024-08-07", "75_2021_2021-03-09")

main_cluster_seq_names <- labels(alignment)[
  !labels(alignment) %in% c(pedregal_seq_names, other_introductions)
]

# -----------------------------
# 3. Pairwise genetic distances (TN93)
# -----------------------------
dist_matrix <- as.matrix(dist.dna(alignment, model = "TN93"))

# Within-cluster distances
within_main <- dist_matrix[main_cluster_seq_names, main_cluster_seq_names][upper.tri(dist_matrix[main_cluster_seq_names, main_cluster_seq_names])]
within_ped  <- dist_matrix[pedregal_seq_names, pedregal_seq_names][upper.tri(dist_matrix[pedregal_seq_names, pedregal_seq_names])]

# Outlier distances to clusters
dist_219_main <- dist_matrix["219_2024_2024-08-07", main_cluster_seq_names]
dist_75_main  <- dist_matrix["75_2021_2021-03-09", main_cluster_seq_names]

dist_219_ped  <- dist_matrix["219_2024_2024-08-07", pedregal_seq_names]
dist_75_ped   <- dist_matrix["75_2021_2021-03-09", pedregal_seq_names]

# -----------------------------
# 4. Patristic distances from MCC tree
# -----------------------------
tree <- read.beast(here("analysis","BEAST_runs","no-trait","n167","strict-constant","n167-strict-constant.hipstr.tre"))
phy <- tree@phylo
cophenetic_matrix <- cophenetic.phylo(phy)

# Within-cluster patristic distances
within_main_pat <- cophenetic_matrix[main_cluster_seq_names, main_cluster_seq_names][upper.tri(cophenetic_matrix[main_cluster_seq_names, main_cluster_seq_names])]
within_ped_pat  <- cophenetic_matrix[pedregal_seq_names, pedregal_seq_names][upper.tri(cophenetic_matrix[pedregal_seq_names, pedregal_seq_names])]

# Outlier patristic distances
pat_219_main <- cophenetic_matrix["219_2024_2024-08-07", main_cluster_seq_names]
pat_75_main  <- cophenetic_matrix["75_2021_2021-03-09", main_cluster_seq_names]

pat_219_ped <- cophenetic_matrix["219_2024_2024-08-07", pedregal_seq_names]
pat_75_ped  <- cophenetic_matrix["75_2021_2021-03-09", pedregal_seq_names]

# -----------------------------
# 5. Nucleotide diversity (pi)
# -----------------------------
pi_main <- nuc.div(alignment[main_cluster_seq_names, ])
pi_pedregal <- nuc.div(alignment[pedregal_seq_names, ])
pi_219 <- nuc.div(alignment["219_2024_2024-08-07", ])
pi_75  <- nuc.div(alignment["75_2021_2021-03-09", ])

# -----------------------------
# 6. Compile results table
# -----------------------------
genome_length <- 11923  # genome length for SNP conversion

results_table <- data.frame(
  Cluster_or_Sequence = c("Main cluster", "Pedregal cluster", "219", "75"),
  
  # Nucleotide diversity converted to SNPs
  Nucleotide_diversity_SNPs = c(pi_main, pi_pedregal, pi_219, pi_75) * genome_length,
  
  # Mean within-cluster distances (genetic) converted to SNPs
  Mean_within_genetic_SNPs = c(mean(within_main), mean(within_ped), NA, NA) * genome_length,
  
  # Outlier distances to clusters (genetic) in SNPs
  Mean_distance_to_main_SNPs = c(NA, NA, mean(dist_219_main), mean(dist_75_main)) * genome_length,
  Mean_distance_to_ped_SNPs  = c(NA, NA, mean(dist_219_ped), mean(dist_75_ped)) * genome_length,
  
  # Mean within-cluster patristic distances (keep in tree units, NOT SNPs)
  Mean_within_patristic = c(mean(within_main_pat), mean(within_ped_pat), NA, NA),
  
  # Outlier patristic distances to clusters (tree units)
  Patristic_to_main = c(NA, NA, mean(pat_219_main), mean(pat_75_main)),
  Patristic_to_ped  = c(NA, NA, mean(pat_219_ped), mean(pat_75_ped))
)

# Round numeric columns for readability
results_table[, 2:8] <- round(results_table[, 2:8], 1)

results_table

#write.csv(results_table,here("analysis","genetic_diversity","phylogenetic-cluster-diversity.csv"), row.names=F)
# -----------------------------
# 7. Optional: Distance plot highlighting outliers
# -----------------------------
# Combine genetic distances for plotting
plot_df <- data.frame(
  distance = c(within_main * genome_length,
               within_ped * genome_length,
               dist_219_main * genome_length,
               dist_75_main * genome_length),
  cluster = rep(c("Main cluster", "Pedregal cluster", "219", "75"),
                times = c(length(within_main), length(within_ped), length(dist_219_main), length(dist_75_main)))
)

ggplot(plot_df, aes(x = cluster, y = distance, fill = cluster)) +
  geom_violin(alpha = 0.6, trim = FALSE) +  # violin plot only
  labs(
    y = "Pairwise genetic distance (SNPs)",
    x = "",
  ) +
  theme_minimal() +
  theme(legend.position = "none")

