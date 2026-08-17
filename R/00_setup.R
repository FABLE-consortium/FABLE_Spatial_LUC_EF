# ============================================================
# 00_setup.R
# Project setup for FABLE Spatial LUC Emission Factors
# ============================================================

library(here)

# ============================================================
# 1. Project paths
# ============================================================

project_root <- here::here()

madrat_root <- file.path(
  project_root,
  "madrat_data"
)

cache_root <- file.path(
  project_root,
  "cache"
)

out_dir <- file.path(
  project_root,
  "output"
)

mapping_dir <- file.path(
  project_root,
  "mapping_versions"
)

dir.create(madrat_root, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(mapping_dir, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 2. Initialize madrat configuration for a new user
# ============================================================

library(madrat)

# madrat::setConfig() internally calls getConfig() first.
# On a fresh installation this can trigger the interactive:
#
# "madrat mainfolder for data storage not set!"
#
# Therefore, if madrat has not yet been configured in this R session,
# initialize the configuration option before calling setConfig().

if (is.null(getOption("madrat_cfg"))) {
  
  options(
    madrat_cfg = list(
      regionmapping       = NULL,
      extramappings       = NULL,
      packages            = c("madrat", "mrorganic"),
      globalenv           = FALSE,
      verbosity           = 1,
      mainfolder          = madrat_root,
      sourcefolder        = file.path(madrat_root, "sources"),
      cachefolder         = file.path(madrat_root, "cache", "default"),
      mappingfolder       = file.path(madrat_root, "mappings"),
      outputfolder        = file.path(madrat_root, "output"),
      pucfolder           = file.path(madrat_root, "puc"),
      tmpfolder           = file.path(madrat_root, "tmp"),
      nolabels            = NULL,
      forcecache          = FALSE,
      ignorecache         = NULL,
      cachecompression    = "gzip",
      hash                = "xxhash32",
      diagnostics         = FALSE,
      debug               = FALSE,
      maxLengthLogMessage = 200,
      redirections        = NULL
    )
  )
}

# Create madrat directories
dirs <- c(
  file.path(madrat_root, "sources"),
  file.path(madrat_root, "cache", "default"),
  file.path(madrat_root, "mappings"),
  file.path(madrat_root, "output"),
  file.path(madrat_root, "puc"),
  file.path(madrat_root, "tmp")
)

invisible(
  lapply(
    dirs,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)


# ============================================================
# 3. Apply madrat configuration normally
# ============================================================

madrat::setConfig(
  mainfolder    = madrat_root,
  sourcefolder  = file.path(madrat_root, "sources"),
  mappingfolder = file.path(madrat_root, "mappings"),
  outputfolder  = file.path(madrat_root, "output"),
  pucfolder     = file.path(madrat_root, "puc"),
  tmpfolder     = file.path(madrat_root, "tmp"),
  .verbose      = FALSE
)


# ============================================================
# 4. Now load mrorganic and remaining packages
# ============================================================

library(mrorganic)
library(magclass)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(scales)


# ============================================================
# 5. Mapping versions
# ============================================================

soc_version <- "v1"

biomass_version <- "v2_urban"


# ============================================================
# 6. Precomputed FABLE cache locations
# ============================================================

cache_soc <- file.path(
  cache_root,
  paste0("cache_SOC_FABLE_", soc_version)
)

cache_biomass <- file.path(
  cache_root,
  paste0("cache_BIOMASS_FABLE_", biomass_version)
)


# ============================================================
# 7. Check precomputed caches
# ============================================================

soc_cache_available <- dir.exists(cache_soc)
biomass_cache_available <- dir.exists(cache_biomass)

if (!soc_cache_available) {
  warning(
    "\nPrecomputed SOC cache not found:\n",
    cache_soc,
    "\nSee README for download instructions."
  )
}

if (!biomass_cache_available) {
  warning(
    "\nPrecomputed biomass cache not found:\n",
    cache_biomass,
    "\nSee README for download instructions."
  )
}

soc_file <- file.path(
  out_dir,
  paste0(
    "soc_custom_FABLE_",
    soc_version,
    ".rds"
  )
)

biomass_above_file <- file.path(
  out_dir,
  paste0(
    "biomass_aboveground_custom_FABLE_",
    biomass_version,
    ".rds"
  )
)

biomass_below_file <- file.path(
  out_dir,
  paste0(
    "biomass_belowground_custom_FABLE_",
    biomass_version,
    ".rds"
  )
)

# ============================================================
# 8. Setup summary
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "FABLE Spatial LUC Emission Factors\n",
  "============================================================\n",
  "Project root:             ", project_root, "\n",
  "Local madrat directory:   ", madrat_root, "\n",
  "SOC version:              ", soc_version, "\n",
  "SOC cache found:          ", soc_cache_available, "\n",
  "SOC RDS:                  ", soc_file, "\n",
  "Biomass version:          ", biomass_version, "\n",
  "Biomass cache found:      ", biomass_cache_available, "\n",
  "Aboveground RDS:          ", biomass_above_file, "\n",
  "Belowground RDS:          ", biomass_below_file, "\n",
  "============================================================\n\n",
  sep = ""
)