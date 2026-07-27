# ==============================================================================
# Title: implement_seraphim_analysis.R
# Description: Run seraphim analyses on all resistance layers, including Q,E,R stats
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

# =========================================================
# 1. GLOBAL SETTINGS
# =========================================================
localTreesDirectory <- here("analysis","BEAST_runs","continuous-trait-runs","n148","extracted_trees/")

burnIn <- 0
randomSampling <- FALSE
nberOfTreesToSample <- 100
coordinateAttributeName <- "location"

# =========================================================
# 2. CORE FUNCTION
# =========================================================
run_spread_factors <- function(raster_files,
                               pathModel = 2,
                               resistance = TRUE,
                               avgResistances = TRUE,
                               nberOfRandomisations = 0,
                               randomProcedure = 3,
                               output_prefix = "SERAPHIM_output",
                               juliaCSImplementation = TRUE,
                               nberOfCores = 1) {
  
  for (r_file in raster_files) {
    
    rast <- raster(r_file)
    base_name <- tools::file_path_sans_ext(basename(r_file))
    
    cat("Running:", base_name, "\n")
    
    spreadFactors(
      localTreesDirectory,
      nberOfTreesToSample,
      list(rast),
      pathModel,
      resistances = list(resistance),
      avgResistances = list(avgResistances),
      fourCells = FALSE,
      nberOfRandomisations = nberOfRandomisations,
      randomProcedure = randomProcedure,
      outputName = paste0(output_prefix, "_", base_name),
      juliaCSImplementation = juliaCSImplementation,
      nberOfCores=nberOfCores
    )
  }
}

# =========================================================
# ISOLATION BY RESISTANCE WRAPPER
# =========================================================
run_isolation_by_resistance <- function(raster_files,
                                        localTreesDirectory,
                                        nberOfExtractionFiles,
                                        pathModel = 2,
                                        resistance = TRUE,
                                        avgResistances = TRUE,
                                        nberOfRandomisations = 0,
                                        randomProcedure = 3,
                                        output_prefix = "IBR_output",
                                        juliaCSImplementation = TRUE) {
  
  for (r_file in raster_files) {
    
    rast <- raster(r_file)
    base_name <- tools::file_path_sans_ext(basename(r_file))
    
    cat("Running IBR:", base_name, "\n")
    
    isolationByResistance(
      localTreesDirectory = localTreesDirectory,
      nberOfExtractionFiles = nberOfExtractionFiles,
      envVariables = list(rast),
      pathModel = pathModel,
      resistances = list(resistance),
      avgResistances = list(avgResistances),
      fourCells = FALSE,
      nberOfRandomisations = nberOfRandomisations,
      randomProcedure = randomProcedure,
      outputName = paste0(output_prefix, "_", base_name)
    )
  }
}

# =========================================================
# 3. RESULT SUMMARISER
# =========================================================
summarise_results <- function(file, layer = "C") {
  
  df <- read.table(file, header = TRUE, sep = "\t")
  
  # helper function to format median + range
  fmt <- function(x) {
    paste0(
      round(median(x, na.rm = TRUE), 3),
      " [",
      round(quantile(x, 0.025, na.rm = TRUE), 3),
      ", ",
      round(quantile(x, 0.975, na.rm = TRUE), 3),
      "]"
    )
  }
  
  # -------------------------
  # build column names
  # -------------------------
  coef_col_LR1 <- paste0("LR1_coefficients_resistance_class_", layer)
  q_col_LR1    <- paste0("LR1_Q_resistance_class_", layer)
  
  coef_col_LR2 <- paste0("LR2_coefficients_resistance_class_", layer)
  q_col_LR2    <- paste0("LR2_Q_resistance_class_", layer)
  
  # -------------------------
  # safety check
  # -------------------------
  cols_needed <- c(coef_col_LR1, q_col_LR1, coef_col_LR2, q_col_LR2)
  
  missing <- cols_needed[!cols_needed %in% names(df)]
  
  if (length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }
  
  # -------------------------
  # output table
  # -------------------------
  data.frame(
    metric = c("LR1_beta", "LR1_Q", "LR2_beta", "LR2_Q"),
    
    value = c(
      fmt(df[[coef_col_LR1]]),
      fmt(df[[q_col_LR1]]),
      fmt(df[[coef_col_LR2]]),
      fmt(df[[q_col_LR2]])
    )
  )
}

# =========================================================
# 4. GENERIC RUN WRAPPER
# =========================================================
run_model_set <- function(files, prefix, pathModel, resistance, nrand = 0, nberOfCores=1) {
  
  lapply(files, function(f) {
    
    run_spread_factors(
      f,
      pathModel = pathModel,
      resistance = resistance,
      avgResistances = TRUE,
      nberOfRandomisations = nrand,
      randomProcedure = 3,
      output_prefix = prefix,
      nberOfCores=nberOfCores
    )
    
  })
}

# =========================================================
# 5. RASTER GROUPS
# =========================================================

# -------------------------
# Rivers
# -------------------------
river_k_vals <- c(10, 100, 1000)
river_files <- file.path(
  here("analysis","seraphim","resistance_rasters","grid_1kmbuffer/"),
  paste0("river_decay_k", river_k_vals, ".asc")
)

run_model_set(
  river_files,
  here("analysis","seraphim","outputs","1km_buffer/river_decay"),
  pathModel = 2,
  resistance = TRUE,
  nrand = 1
)

run_model_set(
  river_files,
  here9=("analysis","seraphim","outputs","river_decay_dispersal"),
  pathModel = 0,
  resistance = TRUE,
  nrand = 1
)

run_model_set(
  river_files,
  here("analysis","seraphim","outputs","1km_buffer","river_decay_conductance"),
  pathModel = 2,
  resistance = FALSE,
  nrand = 1
)

run_model_set(
  river_files,
  here("analysis","seraphim","outputs","1km_buffer","river_decay_conductance_dispersal"),
  pathModel = 0,
  resistance = FALSE,
  nrand = 1
)

# circuitscape
run_model_set(
  river_files,
  "RABV_river_cs",
  pathModel = 3,
  resistance = TRUE, 
  nrand=1,
  nberOfCores=2
)

# -------------------------
# Water channels
# -------------------------
water_k_vals <- c(10, 100, 1000)
water_files <- file.path(
  here("analysis","seraphim","resistance_rasters","grid_1kmbuffer/"),
  paste0("WaterChannels_decay_k", water_k_vals, ".asc")
)

run_model_set(
  water_files,
  here("analysis","seraphim","outputs","1km_buffer","waterchannels_decay"),
  pathModel = 2,
  resistance = TRUE, 
  nrand=1
)

run_model_set(
  water_files,
  here("analysis","seraphim","outputs","1km_buffer","waterchannels_decay_dispersal"),
  pathModel = 0,
  resistance = TRUE,
  nrand = 1
)

run_model_set(
  water_files,
  here("analysis","seraphim","outputs","1km_buffer", "waterchannels_decay_conductance"),
  pathModel = 2,
  resistance = FALSE,
  nrand = 1
)

run_model_set(
  water_files,
  here("analysis","seraphim","outputs","1km_buffer",
 "waterchannels_decay_conductance_dispersal"),
  pathModel = 0,
  resistance = FALSE,
  nrand = 1
)

## circuitscape
# resistance
run_model_set(
  water_files,
  "RABV_water_cs",
  pathModel = 3,
  resistance = TRUE, 
  nrand=1
)
# conductance
run_model_set(
  water_files,
  "RABV_water_conductor_cs",
  pathModel = 3,
  resistance = FALSE, 
  nrand=1
)


run_model_set(
  water_files,
  "RABV_water_cs_location",
  pathModel = 0,
  resistance = TRUE, 
  nrand=1,
  nberOfCores=5
)
run_model_set(
  water_files,
  here("analysis","seraphim","outputs","expanded-grid","RABV_water_cs"),
  pathModel = 3,
  nberOfTreesToSample=1,
  resistance = FALSE, 
  nrand=1, 
  juliaCSImplementation = TRUE
)


## isolation by resistance

run_isolation_by_resistance(
  raster_files = water_files,
  localTreesDirectory = localTreesDirectory,
  nberOfExtractionFiles = nberOfExtractionFiles,
  pathModel = 2,
  resistance = TRUE,
  avgResistances = TRUE,
  nberOfRandomisations = 1,
  randomProcedure = 3,
  output_prefix = "IBR_test"
)
# -------------------------
# Elevation
# -------------------------
elev_file <- here("analysis","seraphim","resistance_rasters","Elevation_200m.asc")

run_spread_factors(
  list(elev_file),
  pathModel = 2,
  resistance = TRUE,
  output_prefix = here("analysis","seraphim","outputs","RABV_elevation_resistance")
)

run_spread_factors(
  list(elev_file),
  pathModel = 0,
  resistance = TRUE,
  nberOfRandomisations = 1,
  output_prefix = here("analysis","seraphim","outputs","RABV_elevation_dispersal")
)

# -------------------------
# Inhabited areas
# -------------------------
ses_k_vals <- c(10, 100, 1000)
ses_files <- file.path(
  here("analysis","seraphim","resistance_rasters","grid_1kmbuffer/"),
  paste0("SES_gradient_highRdeprivation_bkgNA_k", ses_k_vals, ".asc")
)
#ses_file <- "analysis/seraphim/resistance_rasters/expanded_grid/ses/SES_inhabited_k10.asc"

run_model_set(
  ses_files,
  here("analysis","seraphim","outputs","1km_buffer","SES_gradient_highRdeprivation_bkgNA"),
  pathModel = 2,
  resistance = TRUE, 
  nrand=1
)

run_model_set(
  ses_files,
  here("analysis","seraphim","outputs","1km_buffer","SES_gradient_highRdeprivation_bkgNA_dispersal"),
  pathModel = 0,
  resistance = TRUE,
  nrand = 1
)

run_model_set(
  ses_files,
  here("analysis","seraphim","outputs","1km_buffer","SES_gradient_highRdeprivation_bkgNA_conductance"),
  pathModel = 2,
  resistance = FALSE,
  nrand = 1
)

run_model_set(
  ses_files,
  here("analysis","seraphim","outputs","1km_buffer","SES_gradient_highRdeprivation_bkgNA_conductance_dispersal"),
  pathModel = 0,
  resistance = FALSE,
  nrand = 1
)

# -------------------------
# SES k-scenarios
# -------------------------
ses_k_vals <- c(10, 100, 1000)

ses_files <- file.path(
  here("analysis","seraphim","resistance_rasters","expanded_grid"),
  paste0("SES_resistance_bkgNA_k", ses_k_vals, ".asc")
)

run_model_set(
  ses_files,
  here("analysis","seraphim","outputs","expanded-grid","RABV_ses_bkgNA_k_resistance"),
  pathModel = 2,
  resistance = TRUE,
  nrand = 1
)

run_model_set(
  ses_files,
  here("analysis","seraphim","outputs","expanded-grid","RABV_ses_bkgNA_k_dispersal"),
  pathModel = 0,
  resistance = TRUE,
  nrand = 1
)


run_model_set(
  ses_files,
  here("analysis","seraphim","outputs","expanded-grid","RABV_ses_bkg10000_k_conductance"),
  pathModel = 2,
  resistance = FALSE,
  nrand = 1
)

run_model_set(
  ses_files,
  here("analysis","seraphim","outputs","expanded-grid","RABV_ses_bkgNA_k_conductance_dispersal"),
  pathModel = 0,
  resistance = FALSE,
  nrand = 1
)
# =========================================================
# SES — INHABITED SCENARIO (A–F = 1, scaled by k)
# =========================================================
ses_k_vals <- c(10, 100, 1000)

inhabited_files <- file.path(
  here("analysis","seraphim","resistance_rasters","grid_1kmbuffer/"),
  paste0("SES_deprivedVaffluent_bkgNA_k", ses_k_vals, ".asc")
)
# -------------------------
# 5. RUN SERAPHIM MODELS
# -------------------------

# Resistance (path model 2)
run_model_set(
  inhabited_files,
  here("analysis","seraphim","outputs","1km_buffer","SES_deprivedVaffluent_bkgNA"),
  pathModel = 2,
  resistance = TRUE
)

# Dispersal location (path model 0)
run_model_set(
  inhabited_files,
  here("analysis","seraphim","outputs","1km_buffer","SES_deprivedVaffluent_bkgNA_dispersal"),
  pathModel = 0,
  resistance = TRUE,
  nrand = 1
)

# Conductance (path model 2)
run_model_set(
  inhabited_files,
  here("analysis","seraphim","outputs","1km_buffer","SES_deprivedVaffluent_bkgNA_conductance"),
  pathModel = 2,
  resistance = FALSE
)

# Conductance dispersal (path model 0)
run_model_set(
  inhabited_files,
  here("analysis","seraphim","outputs","1km_buffer","SES_deprivedVaffluent_bkgNA_conductance_dispersal"),
  pathModel = 0,
  resistance = FALSE,
  nrand = 1
)

# =========================================================
# Accessibility
# =========================================================
access_k_vals <- c(10, 100, 1000)

access_files <- file.path(
  here("analysis","seraphim","resistance_rasters","grid_1kmbuffer/"),
  paste0("accessibility_log_resistance_k", ses_k_vals, ".asc")
)
# -------------------------
# 5. RUN SERAPHIM MODELS
# -------------------------

# Resistance (path model 2)
run_model_set(
  access_files,
  here("analysis","seraphim","outputs","1km_buffer","accessibility_log"),
  pathModel = 2,
  resistance = TRUE, 
  nrand=1
)

# Dispersal location (path model 0)
run_model_set(
  access_files,
  here("analysis","seraphim","outputs","1km_buffer","accessibility_log_dispersal"),
  pathModel = 0,
  resistance = TRUE,
  nrand = 1
)

# Conductance (path model 2)
run_model_set(
  access_files,
  here("analysis","seraphim","outputs","1km_buffer","accessibility_log_conductance"),
  pathModel = 2,
  resistance = FALSE,
  nrand = 1
)

# Conductance dispersal (path model 0)
run_model_set(
  access_files,
  here("analysis","seraphim","outputs","1km_buffer","accessibility_log_conductance_dispersal"),
  pathModel = 0,
  resistance = FALSE,
  nrand = 1
)

# =========================================================
# Pop density
# =========================================================
pop_k_vals <- c(10, 100, 1000)

pop_files <- file.path(
  here("analysis","seraphim","resistance_rasters","grid_1kmbuffer"),
  paste0("pop_mag_resistance_k", ses_k_vals, ".asc")
)
# -------------------------
# 5. RUN SERAPHIM MODELS
# -------------------------

# Resistance (path model 2)
run_model_set(
  pop_files,
  here("analysis","seraphim","outputs","1km_buffer","pop_mag_resistance"),
  pathModel = 2,
  resistance = TRUE, 
  nrand=1
)

# Dispersal location (path model 0)
run_model_set(
  pop_files,
  here("analysis","seraphim","outputs","1km_buffer","pop_mag_resistance_dispersal"),
  pathModel = 0,
  resistance = TRUE,
  nrand = 1
)

# Conductance (path model 2)
run_model_set(
  pop_files,
  here("analysis","seraphim","outputs","1km_buffer","pop_mag_resistance_conductance"),
  pathModel = 2,
  resistance = FALSE,
  nrand = 1
)

# Conductance dispersal (path model 0)
run_model_set(
  pop_files,
  here("analysis","seraphim","outputs","1km_buffer",
"pop_mag_resistance_conductance_dispersal"),
  pathModel = 0,
  resistance = FALSE,
  nrand = 1
)

# -------------------------
# Croplands
# -------------------------
crop_k_vals <- c(10, 100, 1000)
crop_files <- file.path(
  here("analysis","seraphim","resistance_rasters","grid_1kmbuffer/"),
  paste0("cropland_resistance_k", crop_k_vals, ".asc")
)


run_model_set(
  crop_files,
  here("analysis","seraphim","outputs","1km_buffer","cropland"),
  pathModel = 2,
  resistance = TRUE, 
  nrand=1
)

run_model_set(
  crop_files,
  here("analysis","seraphim","outputs","1km_buffer","cropland_dispersal"),
  pathModel = 0,
  resistance = TRUE,
  nrand = 1
)

run_model_set(
  crop_files,
  here("analysis","seraphim","outputs","1km_buffer","cropland_conductance"),
  pathModel = 2,
  resistance = FALSE,
  nrand = 1
)

run_model_set(
  crop_files,
  here("analysis","seraphim","outputs","1km_buffer","cropland_conductance_dispersal"),
  pathModel = 0,
  resistance = FALSE,
  nrand = 1
)


# =========================================================
# 6. EXAMPLE SUMMARIES
# =========================================================
summarise_seraphim_lr2 <- function(path) {
  
  files <- list.files(path, full.names = TRUE,
                      pattern = "linear_regression_results.txt")
  
  hpd <- function(x) {
    c(
      median(x, na.rm = TRUE),
      quantile(x, 0.025, na.rm = TRUE),
      quantile(x, 0.975, na.rm = TRUE)
    )
  }
  
  fmt <- function(x) {
    paste0(
      round(x[1], 3),
      " [",
      round(x[2], 3),
      ", ",
      round(x[3], 3),
      "]"
    )
  
  out <- lapply(files, function(f) {
    
    tab <- read.table(f, header = TRUE, sep = "\t")
    
    fname <- basename(f)
    
    k <- sub(".*_k([0-9]+)_.*", "\\1", fname)
    layer <- sub(".*_resistance_class_([A-Z]).*", "\\1", fname)
    
    # -------------------------
    # COLUMN-BASED EXTRACTION
    # -------------------------
    beta <- tab[[7]]   # column G
    Q    <- tab[[10]]  # column J
    
    data.frame(
      file = fname,
      k = k,
      layer = layer,
      
      beta = fmt(hpd(beta)),
      
      Q = paste0(
        fmt(hpd(Q)),
        ", p(Q>0)= ",
        round(mean(Q > 0, na.rm = TRUE), 3)
      )
    )
  })
  
  do.call(rbind, out)
}
results <- summarise_seraphim_lr2(here("analysis","seraphim","outputs"))
write.csv(results,here("analysis","seraphim","outputs","all_lm_results.csv"))
results[is.na(results$LR1_beta) | is.na(results$LR2_beta), ]

## 
# -------------------------
# Roads
# -------------------------
road_k_vals <- c(10, 100, 1000)
road_files <- file.path(
  here("analysis","seraphim","resistance_rasters","grid_1kmbuffer/"),
  paste0("Roads_decay_k", road_k_vals, ".asc")
)

run_model_set(
  road_files,
  here("analysis","seraphim","outputs","1km_buffer","roads_decay"),
  pathModel = 2,
  resistance = TRUE,
  nrand=1
)

run_model_set(
  road_files,
  here("analysis","seraphim","outputs","1km_buffer","roads_decay_dispersal"),
  pathModel = 0,
  resistance = TRUE,
  nrand = 1
)


# Conductance (path model 2)
run_model_set(
  road_files,
  here("analysis","seraphim","outputs","1km_buffer","roads_decay_conductance"),
  pathModel = 2,
  resistance = FALSE,
  nrand=1
)

# Conductance dispersal (path model 0)
run_model_set(
  road_files,
  here("analysis","seraphim","outputs","1km_buffer","roads_decay_conductance_dispersal"),
  pathModel = 0,
  resistance = FALSE,
  nrand = 1
)

# -------------------------
# NULL
# -------------------------

base_raster <- rast(here("analysis","seraphim","aqpCity_200m_1kmbuffer.asc"))

# convert to RasterLayer
base_raster_r <- raster(base_raster)
values(base_raster_r) <- 1
# make it a true null
values(base_raster_r) <- 1
spreadFactors(
  localTreesDirectory,
  nberOfTreesToSample,
  list(base_raster_r),   
  pathModel = 2,
  resistances = list(TRUE),
  avgResistances = list(TRUE),
  fourCells = FALSE,
  nberOfRandomisations = 1,
  randomProcedure = 3,
  outputName = here("analysis","seraphim","outputs","1km_buffer","null_model"),
  juliaCSImplementation = TRUE,
  nberOfCores = 1
)

spreadFactors(
  localTreesDirectory,
  nberOfTreesToSample,
  list(base_raster_r),   
  pathModel = 0,
  resistances = list(TRUE),
  avgResistances = list(TRUE),
  fourCells = FALSE,
  nberOfRandomisations = 1,
  randomProcedure = 3,
  outputName = here("analysis","seraphim","outputs","1km_buffer","null_model_dispersal"),
  juliaCSImplementation = TRUE,
  nberOfCores = 1
)


spreadFactors(
  localTreesDirectory,
  nberOfTreesToSample,
  list(base_raster_r), 
  pathModel = 2,
  resistances = list(FALSE),
  avgResistances = list(TRUE),
  fourCells = FALSE,
  nberOfRandomisations = 1,
  randomProcedure = 3,
  outputName = here("analysis","seraphim","outputs","1km_buffer","null_model_conductance"),
  juliaCSImplementation = TRUE,
  nberOfCores = 1
)

spreadFactors(
  localTreesDirectory,
  nberOfTreesToSample,
  list(base_raster_r),  
  pathModel = 0,
  resistances = list(FALSE),
  avgResistances = list(TRUE),
  fourCells = FALSE,
  nberOfRandomisations = 1,
  randomProcedure = 3,
  outputName = here("analysis","seraphim","outputs","1km_buffer","null_model_conductance_dispersal"),
  juliaCSImplementation = TRUE,
  nberOfCores = 1
)



