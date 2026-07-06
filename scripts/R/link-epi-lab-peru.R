library(dplyr)
library(lubridate)
library(stringr)
library(readxl)

## link genetic and epi data

source("scripts/R/redcap-getMetadata.R")
dim(df_per_80)

# import epi data
epi_filler <- read.csv("raw_data/epi_metadata/Rabies focus control data_SEQ-unclean_v2.csv")
#epi <- read.csv("raw_data/epi_metadata/forsampleselection_sep2025.csv")
epi <- read_excel("raw_data/epi_metadata/forsampleselection_sep2025.xls")
epi <- epi %>%
  rename(X = `...4`)

# match gen to epi
# Identify which samples are in epi
in_epi <- df_per_80$sample_id %in% epi$X

# subset samples in epi 
epi_from_main <- epi %>%
  filter(X %in% df_per_80$sample_id)
write.csv(epi_from_main, "processed_data/processed_metadata/epi-seq_matches.csv",quote=F,row.names=F)


# For samples not in epi, get from epi_filler
epi_from_filler <- epi_filler %>%
  filter(ID %in% df_per_80$sample_id[!in_epi]) 
write.csv(epi_from_filler , "processed_data/processed_metadata/epi_missing_from_unclean_v2.csv",quote=F,row.names=F)

# MANUAL STEP: Add missing records to the epi-seq file. Manual because columns are different.
# "processed_data/processed_metadata/epi-seq_matches_withmissing.csv"
# Import manually enhanced metadata
epi_complete=read.csv("processed_data/processed_metadata/epi-seq_matches_withmissing.csv")

# Check for matches with seq data
df_per_80$sample_id[!df_per_80$sample_id %in% epi_complete$X]
# one still has no match: 50_2025


# # keep all cols with dates
# dates <- epi_subset %>%
#   select(ID,matches("day|month|year", ignore.case = TRUE))
# 
# # Get all column names
# 
# # Define all date groups manually
# date_groups <- list(
#   symptoms_appeared = c("day_sympoms_appeared", "month_sympoms_appeared", "year_sympoms_appeared"),
#   killed           = c("day_killed", "month_killed", "year_killed"),
#   Sampling         = c("Sampling_day", "Sampling_month", "Sampling_year"),
#   containment       = c("day_containent_started", "month_containent_started", "year_containent_started"),
#   Confirmation     = c("Confirmation_day", "Confirmation_month", "Confirmation_year"),
#   brain_shipping   = c("brain_sampling_shipping_day", "brain_sampling_shipping_month", "brain_sampling_shipping_year")
# )
# 
# #  Create full date columns
# for(group_name in names(date_groups)) {
#   cols <- date_groups[[group_name]]
#   if(all(cols %in% names(epi_subset))) {
#     # Create Date object
#     epi_subset[[paste0("date_", group_name)]] <- make_date(
#       year  = epi_subset[[cols[3]]],
#       month = epi_subset[[cols[2]]],
#       day   = epi_subset[[cols[1]]]
#     )
#     # Optional: human-readable format
#     epi_subset[[paste0("date_", group_name, "_formatted")]] <- format(epi_subset[[paste0("date_", group_name)]], "%d-%b-%Y")
#   }
# }

# #  Create best_date column (priority order)
# epi_complete <- epi_complete %>%
#   mutate(
#     best_date = coalesce(
#       date_symptoms_appeared_formatted,
#       date_killed_formatted,
#       date_Sampling_formatted,
#       date_containment_formatted,
#       date_Confirmation_formatted,
#       date_brain_shipping_formatted
#       
#     )
#   )

#  Create best_date column (priority order)


epi_complete <- epi_complete %>%
  mutate(
    across(
      c(symptom_date, kill_date, sample_date, ship_date, contain_date),
      ~ suppressWarnings(lubridate::dmy(.))  # correctly parse DD-MMM-YY
    ),
    best_date = coalesce(symptom_date, kill_date, sample_date, ship_date, contain_date),
    best_date = format(best_date, "%d-%b-%y")  # keep display as 22-Mar-15
  )
write.csv(epi_complete, "processed_data/processed_metadata/081025_epi-seq_n166.csv",row.names=F)
epi_complete=read.csv("processed_data/processed_metadata/081025_epi-seq_n167.csv")
# create data for clockor2/phylogenetic reconstructions
temporal <- epi_complete %>%
  select(tip=X, date=best_date, group=district)

# Function to convert Date to decimal year
decimal_date <- function(date) {
  year(date) + (yday(date) - 1) / ifelse(leap_year(year(date)), 366, 365)
}
#temporal$date <- dmy(temporal$date)
temporal <- temporal %>%
  mutate(
    date = decimal_date(dmy(date))
  )
temporal$group <- str_to_title(temporal$group)
temporal$group <- gsub(" ","_", temporal$group)
temporal <- temporal %>%
  mutate(
    group = group %>%
      gsub("[^a-zA-Z0-9]", "", .) %>%   # remove all non-alphanumeric characters
      tolower()                         # convert to lowercase
  )
write.csv(temporal, "processed_data/processed_metadata/091025_epi-seq_n167_clockor2.csv",row.names=F)
write.table(temporal, "processed_data/epi_subset99_beast_standarddate.txt",row.names=F, sep="\t")
