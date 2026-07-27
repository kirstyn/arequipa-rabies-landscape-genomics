# ==============================================================================
# Title: global-packages.R
# Description: Install packages for project
#
# Author: Kirstyn Brunker
# Date: 2025/2026
# ==============================================================================
# -----------------------------
# 0. Load packages
# -----------------------------

# List required packages
packages <- c(
  "ape",
  "apex",
  "Biostrings",
  "castor",
  "cowplot",
  "DECIPHER",
  "devtools",
  "diagram",
  "dplyr",
  "here",
  "fields",
  "gdistance",
  "geodata",
  "ggh4x",
  "ggimage",
  "ggplot2",
  "ggpubfigs",
  "ggpubr",
  "ggnewscale",
  "ggplotify",
  "ggraph",
  "ggrepel",
  "ggspatial",
  "ggstar",
  "ggtext",
  "ggtree",
  "ggtreeExtra",
  "gridExtra",
  "igraph",
  "leaflet",
  "lubridate",
  #"maptools",
  "ORFik",
  "osmdata",
  "pals",
  "patchwork",
  "pegas",
  "phangorn",
  "phytools",
  "Polychrome",
  "prettymapr",
  "RColorBrewer",
  "readr",
  "rnaturalearth",
  "rnaturalearthdata",
  "raster",
  "readr",
  "readxl",
  "scales",
  "scatterpie",
  "scico",
  "seqinr",
  "seraphim",
  "sf",
  "stringr",
  "terra",
  "tidyr",
  "tidyverse",
  "treeio",
  "vegan",
  "viridis",
  "wesanderson"
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(packages, install_if_missing))

# Load packages
lapply(packages, library, character.only = TRUE)
