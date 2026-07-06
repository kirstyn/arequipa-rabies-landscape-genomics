source("scripts/ngs_functions.R")
source("scripts/redcap-getMetadata.R")
library(seqinr)
# combine Peru aqp16-18 sequences
concat_fasta("peru/raw_data/sequences", "peru/150825_all_seq.fasta")

# import that fasta and remove the seq with coverage <80% (this is threshold used in pedregal paper)
per_seq=read.fasta("peru/150825_all_seq.fasta")
fix_names <- sapply(strsplit(names(per_seq), "_"), function(x) paste(x[1:2], collapse = "_"))
fix_names <- gsub("_meta-illumina","", fix_names)
# tidy names
names(per_seq)=fix_names
# manual fix- convert accessions to sample ids
names(per_seq)[names(per_seq)=="KU938752_NA"]="1101787"
names(per_seq)[names(per_seq)=="KU938829_NA"]="1203037"
# manual fixes to pedregal paper names
names(per_seq)[names(per_seq)=="173_2023"]="173_2022"
names(per_seq)[names(per_seq)=="225_2019"]="227_2019"



# peru lab metadata = df_per
keep_per=df_per_80$sample_id

# Subset sequences to only those in metadata
per_seq_subset <- per_seq[names(per_seq) %in% keep_per]
names(per_seq)[!names(per_seq) %in% keep_per]

# write subset to new fasta
write.fasta(sequences = per_seq_subset, 
            names = names(per_seq_subset), 
            file.out = "peru/150825_filt_seq.fasta")
