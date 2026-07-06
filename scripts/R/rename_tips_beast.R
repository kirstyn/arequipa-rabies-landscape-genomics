# Load required package
library(Biostrings)   # from Bioconductor

# 1. Read the fasta file
fasta_file <- "processed_data/150825_filt_seq.aln.fasta"
seqs <- readDNAStringSet(fasta_file)

# Inspect the sequence names
names(seqs)[1:5]

# 2. Read your metadata file
metadata <- read.csv("processed_data/epi_subset99_clockor2_tipstandarddate.csv", stringsAsFactors = FALSE)

# Clean metadata$tip -> keep only the part before the first "|"
metadata$tip_id <- sub("\\|.*", "", metadata$tip)

# 3. Merge and rename
for (i in seq_along(seqs)) {
  tip <- names(seqs)[i]
  # Match using cleaned metadata$tip_id
  date_val <- metadata$date[match(tip, metadata$tip_id)]
  if (!is.na(date_val)) {
    names(seqs)[i] <- paste0(tip, "|", date_val)  # e.g. 88_2019|2019-03-16
  }
}

# 4. Write out the new fasta
writeXStringSet(seqs, "processed_data/150825_filt_seq.aln.fasta_renamed_with_dates.fasta")
