# Tidy location data in Philippines metadata
library(dplyr)
library(stringdist)
library(stringr)
library(tidyr)

# Input data files
# Province to region mapping data:
district_centroids=read.csv("processed_data/gis_data/district_latest_centroids.csv")
# Gathered metadata file to add geo standardisations to
input_file="processed_data/processed_metadata/081025_epi-seq_n167.csv"
data_all=read.csv(input_file)


## Function to standardise Region information. 

# improved standardise_region: handles "Central Luzon (III)", "Region III", "III (Central Luzon)", etc.
standardise_district <- function(district_col, adm, max_dist = 6) {
  library(stringr)
  library(stringdist)
  library(dplyr)
  
  # Extract and prepare official district names
  adm_districts <- adm %>%
    mutate(district = str_squish(district)) %>%
    pull(district)
  
  adm_clean <- adm_districts %>%
    tolower() %>%
    str_replace_all("[^a-z]", "")   # letters only
  
  # List of districts to keep as-is (parentheses stripped)
  keep_as_is <- c("CAMINACA", "ATUNCOLLA", "AZANGARO")  # add more if needed
  
  sapply(district_col, function(x) {
    
    # Empty or NA
    if (is.na(x) || x == "") return(NA_character_)
    
    x_clean <- str_squish(as.character(x))
    x_lower <- tolower(x_clean)
    
    # Remove parentheses
    x_no_paren <- gsub("\\s*\\(.*?\\)", "", x_lower)
    x_no_paren <- str_squish(x_no_paren)
    x_letters <- str_replace_all(x_no_paren, "[^a-z]", "")
    
    # ---- Special cases ----
    if (str_detect(x_letters, "pedregal")) {
      return("MAJES")
    }
    
    if (str_detect(x_letters, "jlbyr")) {
      return("JOSÉ LUIS BUSTAMANTE Y RIVERO")
    }
    
    if (toupper(x_no_paren) %in% keep_as_is) {
      return(toupper(x_no_paren))
    }
    
    # ---- Exact cleaned match ----
    idx_exact <- which(adm_clean == x_letters)
    if (length(idx_exact) > 0) return(adm_districts[idx_exact[1]])
    
    # ---- Partial (substring) match ----
    idx_partial <- which(
      str_detect(adm_clean, fixed(x_letters)) |
        str_detect(x_letters, fixed(adm_clean))
    )
    if (length(idx_partial) > 0) return(adm_districts[idx_partial[1]])
    
    # ---- Fuzzy matching ----
    dists <- stringdist(x_letters, adm_clean, method = "lv")
    best_idx <- which.min(dists)
    if (!is.infinite(dists[best_idx]) && dists[best_idx] <= max_dist)
      return(adm_districts[best_idx])
    
    # ---- If nothing matches, return cleaned name without parentheses ----
    return(toupper(x_no_paren))
    
  }, USE.NAMES = FALSE)
}

# Apply functions
data_all$district_std <- standardise_district(data_all$district, district_centroids)
table(is.na(data_all$district_std))  # check unmatched
# 3. Extract unmatched rows (should be none)
unmatched_districts <- data_all %>%
  filter(is.na(district_std)) %>%
  select(district, district_std, everything()) 
unique(unmatched_districts$district)


## write the standardised data to file. 
write.csv(data_all, paste0(gsub(".csv","",input_file),"_stdDistrict.csv"), row.names=F)
