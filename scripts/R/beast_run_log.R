library(tibble)
library(readr)

# Define run metadata

run_log <- tibble(
  Run_ID = "PER_RABV_2024_SS_11",
  Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  User = Sys.info()[["user"]],
  BEAST_version = "v1.10.5",
  Alignment = "redcap_download_20251009_0935_n167.renamed.aln.date_appended.fasta",
  XML_file = "analysis/BEAST_runs/XML/PER_RABV_2024_SS_11.xml",
  Clock_model = "Uncorrelated Lognormal Relaxed Clock (ucld)",
  Clock_rate_prior = "Lognormal(mean=1.895E-4, sd=0.000216, initial=0.000185, meanInRealSpace=TRUE)",
  Tree_prior = "Coalescent Exponential Growth",
  Chain_length = 5e8,
  Sampling_freq = 50000,
  Burn_in = "10%",
  SS_path_steps = 500,
  SS_chain_length_per_step = 5e6,
  SS_sampling_freq = 1000,
  Location_trait = "location",
  Migration_model = "Cauchy RRW",
  BSSVS = F,  # not specified in XML
  Location_strategy = "exact coords",
  Manual_xml_edits = "",
  Notes = "Repeat of prev runs where incorrect coords used for Puno seq",
  Result_notes=""
)


# File to write log into
log_file <- "analysis/BEAST_runs/BEAST_runs_log.csv"

if (!file.exists(log_file)) {
  write_csv(run_log, log_file)  # automatically writes header
} else {
  write_csv(run_log, log_file, append = TRUE)  # appends without writing header
}

