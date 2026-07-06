# Load libraries
library(treeio)   # for read.beast
library(ape)      # for write.tree

# 1. Read the BEAST tree
tree_file <- "analysis/BEAST_runs/no-trait/n148/aqp_mainIntro_skygrid.hipstr.tre"
beast_tree <- read.beast(tree_file)

# 2. Extract the phylo object
phylo_tree <- beast_tree@phylo

# 3. Save as Newick
write.tree(phylo_tree, file = "analysis/BEAST_runs/no-trait/n148/aqp_mainIntro_skygrid.hipstr.tre.newick")


## Then download treemmer trees and check the sample size estiamtes 
treemmer_file <- "analysis/BEAST_runs/no-trait/n148/treemmer-pruned-5/aqp_mainIntro_skygrid.hipstr.tre.newick_trimmed_list_X_100"
treemmer_tips <- readLines(treemmer_file)

cases <- read.table("analysis/BEAST_runs/trait_files/n148-teamDefinedTraits-v2.txt", header=T)

# 3. Keep only metadata for tips that remain in the pruned list
case_meta_pruned <- cases %>%
  filter(Taxon %in% treemmer_tips)

# Count number of samples per district
sample_counts <- case_meta_pruned %>%
  group_by(district) %>%
  summarise(n_samples = n())

sample_counts

write.table(
  case_meta_pruned ,
  file = "analysis/BEAST_runs/trait_files/n148-teamDefinedTraits-pruned-5.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

fasta_concat <- "/Users/kirstyn.brunker/GitHub/RABV_Arequipa_2025/processed_data/nt_alignments/redcap_download_20251009_arequipa_only_mainIntro_n148.renamed_nextalign/gene_alignments/redcap_download_20251009_arequipa_only_mainIntro_n148.renamed.nextalign.aligned.beast_concat.fasta"
fasta_nc     <- "/Users/kirstyn.brunker/GitHub/RABV_Arequipa_2025/processed_data/nt_alignments/redcap_download_20251009_arequipa_only_mainIntro_n148.renamed_nextalign/gene_alignments/redcap_download_20251009_arequipa_only_mainIntro_n148.renamed.nextalign.aligned.beast_nc.fasta"

# Read alignments
seqs_concat <- readDNAStringSet(fasta_concat)
seqs_nc     <- readDNAStringSet(fasta_nc)

# -----------------------------
# 3. Keep only sequences in Treemmer tip list
# -----------------------------
seqs_concat_pruned <- seqs_concat[names(seqs_concat) %in% treemmer_tips]
seqs_nc_pruned     <- seqs_nc[names(seqs_nc) %in% treemmer_tips]

# -----------------------------
# 4. Write out new downsampled FASTAs
# -----------------------------
writeXStringSet(seqs_concat_pruned, filepath = "processed_data/nt_alignments/n148_concat_treemmer_pruned_5.fasta")
writeXStringSet(seqs_nc_pruned,     filepath = "processed_data/nt_alignments/n148_nc_treemmer_pruned_5.fasta")

# -----------------------------
# 5. Optional check
# -----------------------------
length(seqs_concat_pruned)  # should match length of treemmer_tips
all(names(seqs_concat_pruned) %in% treemmer_tips)  # should be TRUE
