# =========================
# SERAPHIM FULL PIPELINE (FIXED + ROBUST)
# =========================

library(seraphim)
library(raster)
library(terra)
library(dplyr)

# -------------------------
# INPUTS
# -------------------------

localTreesDirectory <- "analysis/BEAST_runs/continuous-trait-runs/n147/extracted_trees/"
nberOfExtractionFiles <- 100
outdir <- "analysis/seraphim/outputs"

# -------------------------
# SAFE HELPERS
# -------------------------

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_vec <- function(x) {
  if (length(x) == 0) return(numeric(0))
  unlist(x)
}

# -------------------------
# MAIN FUNCTION
# -------------------------

run_seraphim_rc <- function(files,
                            analysis_name,
                            pathModel,
                            randomisation = 0,
                            randomProcedure = 3,
                            outdir = "analysis/seraphim/outputs") {
  
  all_results <- list()
  
  for (f in files) {
    
    base <- tools::file_path_sans_ext(basename(f))
    
    run_one <- function(res_flag, label) {
      
      output_prefix <- file.path(
        outdir,
        paste0(analysis_name, "_", base, "_", label)
      )
      
      cat("\nRunning:", output_prefix, "\n")
      
      spreadFactors(
        localTreesDirectory,
        nberOfExtractionFiles,
        list(raster(f)),
        pathModel,
        resistances = list(res_flag),
        avgResistances = list(TRUE),
        fourCells = FALSE,
        nberOfRandomisations = randomisation,
        randomProcedure = randomProcedure,
        output_prefix
      )
      
      # -------------------------
      # LOAD OUTPUT
      # -------------------------
      
      result_file <- list.files(
        path = outdir,
        pattern = paste0(base, "_", label, ".*linear_regression_results\\.txt$"),
        full.names = TRUE,
        recursive = TRUE
      )
      
      if (length(result_file) == 0) {
        warning("Missing file: ", base, " ", label)
        return(NULL)
      }
      
      tab <- read.table(result_file[1], header = TRUE)
      
      # =====================================================
      # R2 EXTRACTION (CORRECT FOR YOUR OUTPUT STRUCTURE)
      # =====================================================
      
      r2_null <- safe_mean(c(
        tab$LR1_R2_null_raster,
        tab$LR2_R2_null_raster
      ))
      
      r2_cols <- grep("_R2_", colnames(tab), value = TRUE)
      r2_cols <- r2_cols[!grepl("null", r2_cols)]
      
      r2_env <- safe_mean(safe_vec(tab[r2_cols]))
      
      # =====================================================
      # Q
      # =====================================================
      
      q_cols <- grep("_Q_", colnames(tab), value = TRUE)
      q_cols <- q_cols[!grepl("null", q_cols)]
      q_vals <- safe_vec(tab[q_cols])
      
      # =====================================================
      # COEFFICIENTS
      # =====================================================
      
      coef_cols <- grep("coefficients_", colnames(tab), value = TRUE)
      coef_cols <- coef_cols[!grepl("null", coef_cols)]
      coef_vals <- safe_vec(tab[coef_cols])
      
      # =====================================================
      # OUTPUT ROW
      # =====================================================
      
      data.frame(
        analysis = analysis_name,
        raster = base,
        pathModel = pathModel,
        type = label,
        
        R2_null = r2_null,
        R2_env = r2_env,
        delta_R2 = r2_env - r2_null,
        
        Q_mean = safe_mean(q_vals),
        Q_positive_prop = ifelse(length(q_vals) > 0, mean(q_vals > 0, na.rm = TRUE), NA),
        
        coef_mean = safe_mean(coef_vals),
        coef_pos_prop = ifelse(length(coef_vals) > 0, mean(coef_vals > 0, na.rm = TRUE), NA)
      )
    }
    
    res1 <- run_one(TRUE, "resistance")
    res2 <- run_one(FALSE, "conductance")
    
    all_results[[base]] <- bind_rows(res1, res2)
  }
  
  bind_rows(all_results)
}

# =========================
# RASTERS
# =========================

river_files <- file.path(
  "analysis/seraphim/resistance_rasters",
  paste0("river_resistance_k", c(10,100,1000), ".asc")
)

water_files <- file.path(
  "analysis/seraphim/resistance_rasters",
  paste0("WaterChannels_resistance_k", c(10,100,1000), ".asc")
)

elev_files <- "analysis/seraphim/resistance_rasters/Elevation_200m.asc"

# =========================
# RUN MODELS
# =========================

river_velocity <- run_seraphim_rc(river_files, "river_velocity", 2)
water_velocity <- run_seraphim_rc(water_files, "water_velocity", 2)
elev_velocity  <- run_seraphim_rc(list(elev_files), "elevation_velocity", 2)

river_location <- run_seraphim_rc(river_files, "river_location", 0, randomisation = 1)
water_location <- run_seraphim_rc(water_files, "water_location", 0, randomisation = 1)
elev_location  <- run_seraphim_rc(list(elev_files), "elevation_location", 0, randomisation = 1)

# =========================
# COMBINE RESULTS
# =========================

final_results <- bind_rows(
  river_velocity,
  river_location,
  water_velocity,
  water_location,
  elev_velocity,
  elev_location
)

# =========================
# PAIRWISE RC COMPARISON (FIXED)
# =========================

final_results$R2_env_conductance <- NA_real_
final_results$delta_R2_RC <- NA_real_

for (i in seq_len(nrow(final_results))) {
  
  if (final_results$type[i] == "resistance") {
    
    match <- final_results %>%
      filter(
        raster == final_results$raster[i],
        analysis == final_results$analysis[i],
        type == "conductance"
      )
    
    if (nrow(match) == 1) {
      
      final_results$R2_env_conductance[i] <- match$R2_env
      
      final_results$delta_R2_RC[i] <-
        final_results$R2_env[i] - match$R2_env
    }
  }
}

# =========================
# EXPORT
# =========================

write.csv(
  final_results,
  file.path(outdir, "seraphim_RC_summary_results.csv"),
  row.names = FALSE
)

print(final_results)