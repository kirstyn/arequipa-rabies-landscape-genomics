## Prepare data for beast
library(seqinr)
library(dplyr)
library(lubridate)
#-----------
# SEQUENCES
#-----------
# Prepare sequences: add date to name (useful to have for downstream analyses)
#fasta=read.fasta("processed_data/nt_alignments/redcap_download_20251009_arequipa_only_n163.renamed.aln.fasta")
fasta=read.fasta("processed_data/nt_alignments/redcap_download_20251009_arequipa_only_mainIntro_n148.renamed_nextalign/redcap_download_20251009_arequipa_only_mainIntro_n148.renamed.nextalign.aligned.fasta")
# Import metadata
data=read.csv("processed_data/processed_metadata/081025_epi-seq_n167.csv")

# match sequence names to meta
names(fasta) %in% data$X

# Convert and format best_date as YYYY-MM-DD
data <- data %>%
  mutate(best_date = as.Date(dmy(best_date), format = "%Y-%m-%d"),
         best_date = format(best_date, "%Y-%m-%d"))

# Create lookup vector: names = sample IDs, values = formatted dates
date_lookup <- setNames(data$best_date, data$ID)

# Match to FASTA sequence names
seq_names <- names(fasta)
matched_dates <- date_lookup[seq_names]

# Append date to FASTA names (only where date exists)
new_names <- ifelse(!is.na(matched_dates) & matched_dates != "",
                    paste0(seq_names, "_", matched_dates),
                    seq_names)

# Assign new names back to fasta
names(fasta) <- new_names

# Write new FASTA
write.fasta(sequences = fasta,
            names = names(fasta),
            file.out = "processed_data/nt_alignments/redcap_download_20251009_arequipa_only_mainIntro_n148.renamed_nextalign/redcap_download_20251009_arequipa_only_mainIntro_n148.renamed.nextalign.aligned.renamed.fasta")

#-----------
# METADATA
#-----------

# Prepare metadata for beast - match fasta names above and add trait cols
# Add beast formatted names
data <- data %>%
  mutate(
    beast_name = ifelse(!is.na(best_date) & best_date != "",
                        paste0(ID, "_", best_date),
                        ID)
  )

# confirm that beast_name matches fasta names
all(data$beast_name %in% names(fasta))

# subset the data for traits
beast_trait0 <- data %>%
  select(taxon=beast_name, district) %>%
# tidy the district names 
  mutate(
    district = district %>%
      gsub("[^a-zA-Z0-9]", "", .) %>%   # remove all non-alphanumeric characters
      tolower()                         # convert to lowercase
  ) %>%
# partially abbreviate some long names
  mutate(
    district=district %>%
      gsub("cerrocolorado", "ccolorado",.) %>%
      gsub("altoselvaalegre", "asa",.) %>%
      sub("marianomelgar", "mmelgar",.) %>%
      gsub("caminaca|atuncolla|azangaro" , "",.)
  )
write.table(beast_trait0,"analysis/BEAST_runs/trait_files/redcap_download_20251009_0935_n167.trait0.txt", sep="\t", row.names = F, quote=F)

beast_trait1 <- data %>%
  select(taxon=beast_name, latitude=lat, longitude=lon) 
write.table(beast_trait1, "analysis/BEAST_runs/trait_files/redcap_download_20251009_0935_n167.latlongs.txt", sep="\t", row.names = F, quote=F)

## Also prepare a metadata annotation file that will match the ids
beast_annot <- data %>%
  mutate(
    # Make district formatting consistent with trait file
    district = district %>%
      gsub("[^a-zA-Z0-9]", "", .) %>%   # remove non-alphanumeric
      tolower() %>%                     # lowercase
      gsub("cerrocolorado", "ccolorado", .) %>%
      gsub("altoselvaalegre", "asa", .) %>%
      sub("marianomelgar", "mmelgar", .) %>%
      gsub("caminaca|atuncolla|azangaro", "", .)
  ) %>%
  # Reorder so that X (sample ID) is first column
  select(beast_name,ID, district, everything())

# Write out the annotation file
write.table(
  beast_annot,
  "processed_data/processed_metadata/171025_epi-seq_n167_beast_annot.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
