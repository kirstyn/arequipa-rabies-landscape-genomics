## -----------------------------
## Seraphim: Dispersal Statistics from RRW Tree
## -----------------------------

# Load necessary libraries
library(seraphim)

# -----------------------------
# Step 1: Load MCC summary 
# -----------------------------
# This CSV may contain median/HPD values of branch times, rates, or other RRW info
mcc_tab <- read.csv(
"analysis/BEAST_runs/continuous-trait-runs/n148/peru-148-rrw.extracted.csv",
  header = TRUE
)

# -----------------------------
# Step 2: Define the location of posterior trees
# -----------------------------
# Directory containing trees with spatio-temporal annotations extracted from BEAST
localTreesDirectory <- "analysis/BEAST_runs/continuous-trait-runs/n148/extracted_trees/"

# Load the full set of posterior trees
allTrees <- scan(
  file = "analysis/BEAST_runs/continuous-trait-runs/n148/peru-148-rrw-subsample.trees",
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

# -----------------------------
# Step 6: Notes / outputs
# -----------------------------
# After running, Seraphim will generate:
# 1. Kernel density plots for branch velocities (v_branch, v_weighted)
# 2. Kernel density plots for diffusion coefficients (D_original, D_weighted)
# 3. Wavefront evolution plots:
#    - Maximal spatial wavefront (distance from root to furthest tip)
#    - Maximal patristic wavefront (cumulative branch distance from root to furthest tip)
# 4. Tab-delimited CSVs with median and 95% HPD values for all metrics
# 
# Recommendations:
# - Use weighted metrics (v_weighted, D_weighted) for robust comparisons
# - Sliding window allows you to visualize temporal changes in diffusion rate
# - OnlyTipBranches = TRUE can simplify analysis if internal branches are less informative