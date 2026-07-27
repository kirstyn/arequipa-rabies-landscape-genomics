# ==============================================================================
# Title: seraphim-dispersalStats.R
# Description: Seraphim: Dispersal Statistics from RRW Tree
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================

source(here("scripts","R","global-packages.R"))

# -----------------------------
# Step 1: Load MCC summary 
# -----------------------------
# This CSV may contain median/HPD values of branch times, rates, or other RRW info
mcc_tab <- read.csv(
here("analysis","BEAST_runs","continuous-trait-runs","n148","peru-148-rrw.extracted.csv"),
  header = TRUE
)

# -----------------------------
# Step 2: Define the location of posterior trees
# -----------------------------
# Directory containing trees with spatio-temporal annotations extracted from BEAST
localTreesDirectory <- here("analysis","BEAST_runs","continuous-trait-runs","n148","extracted_trees/")

# Load the subset of posterior trees
allTrees <- scan(
  file = here("analysis","BEAST_runs","continuous-trait-runs","peru-148-rrw-subsample.trees"),
  what = "",
  sep = "\n",
  quiet = TRUE
)

# -----------------------------
# Step 3: Define sampling / burn-in parameters
# -----------------------------
burnIn <- 0                 # burn-in already discarded in your 900 trees
randomSampling <- FALSE     # use sequential trees instead of random subset
nberOfTreesToSample <- 100  # total number of trees to include in dispersal stats

coordinateAttributeName <- "location"  # attribute in trees storing coordinates

# -----------------------------
# Step 4: Set Seraphim parameters for dispersal statistics
# -----------------------------
nberOfExtractionFiles <- nberOfTreesToSample  # number of trees to include
timeSlices <- 20                           # number of time slices for wavefront evolution
onlyTipBranches <- FALSE                      # include internal branches in statistics
showingPlots <- FALSE                         # whether to display plots interactively
outputName <- "analysis/seraphim/outputs/dispersal-stats/RABV_n148"                          # prefix for output files
nberOfCores <- 2                              # number of cores for parallel computation
slidingWindow <- 1                             # time window (years) for diffusion coefficient evolution

# -----------------------------
# Step 5: Run dispersal statistics
# -----------------------------
spreadStatistics(
  localTreesDirectory,
  nberOfExtractionFiles,
  timeSlices,
  onlyTipBranches,
  showingPlots,
  outputName,
  nberOfCores,
  slidingWindow
)

