# ============================================================
# 00_download_mrorganic_sources.R
#
# Prepare source datasets required to recompute carbon stocks
# with mrorganic.
#
# IMPORTANT:
# This script is ONLY required for the full-reproduction pathway.
#
# Users relying on the precomputed madrat caches do not need
# to run this script.
#
# Full-reproduction users should first download the prepared
# madrat_data ZIP described in the README and extract it into
# the project root.
# ============================================================


# ============================================================
# 1. Project setup
# ============================================================

source(here::here("R", "00_setup.R"))

options(timeout = 600)


# ============================================================
# 2. Expected source-data structure
# ============================================================

# After extracting the prepared ZIP, the project should contain:
#
# FABLE_Spatial_LUC_EF/
# └── madrat_data/
#     └── sources/
#         ├── ESACCI/
#         ├── GSOCseq/
#         ├── Spawn/
#         └── ...
#
# madrat_data/ is intentionally excluded from Git because the
# source datasets are large (~10 GB).

source_root <- file.path(
  madrat_root,
  "sources"
)

if (!dir.exists(source_root)) {
  stop(
    "\nmrorganic source directory not found:\n",
    source_root,
    "\n\nDownload the prepared madrat_data ZIP described in the README ",
    "and extract it into the project root before continuing."
  )
}


# ============================================================
# 3. Check ESA CCI
# ============================================================

esacci_file <- file.path(
  source_root,
  "ESACCI",
  "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010-v2.0.7.tif"
)

if (!file.exists(esacci_file)) {
  stop(
    "\nESA CCI source file not found:\n",
    esacci_file,
    "\n\nCheck that the prepared madrat_data ZIP was extracted correctly."
  )
}

message("ESA CCI file found.")

# Verify that terra can read it
esacci_test <- terra::rast(esacci_file)
print(esacci_test)

# Verify that madrat can read it
madrat::readSource(
  "ESACCI",
  subtype = "landcover2010",
  convert = FALSE
)

message("ESA CCI check successful.")


# ============================================================
# 4. Check GSOCseq
# ============================================================

gsoc_file <- file.path(
  source_root,
  "GSOCseq",
  "ini",
  "GSOCseq_T0_Map030.tif"
)

if (!file.exists(gsoc_file)) {
  stop(
    "\nGSOCseq source file not found:\n",
    gsoc_file,
    "\n\nCheck that the prepared madrat_data ZIP was extracted correctly."
  )
}

message("GSOCseq file found.")

# Verify that terra can read it
gsoc_test <- terra::rast(gsoc_file)
print(gsoc_test)

message("GSOCseq check successful.")


# ============================================================
# 5. Check SPAWN
# ============================================================

spawn_dir <- file.path(
  source_root,
  "Spawn"
)

spawn_files <- if (dir.exists(spawn_dir)) {
  list.files(
    spawn_dir,
    recursive = TRUE,
    full.names = TRUE
  )
} else {
  character(0)
}

if (length(spawn_files) == 0) {
  stop(
    "\nSPAWN source data were not found in:\n",
    spawn_dir,
    "\n\nCheck that the prepared madrat_data ZIP was extracted correctly."
  )
}

message(
  "SPAWN source data found: ",
  length(spawn_files),
  " file(s)."
)


# ============================================================
# 6. Source-data summary
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "mrorganic source-data check\n",
  "============================================================\n",
  "ESA CCI: FOUND\n",
  "GSOCseq: FOUND\n",
  "SPAWN: FOUND\n",
  "Source root: ", source_root, "\n",
  "============================================================\n\n",
  sep = ""
)


# ============================================================
# 7. Run mrorganic data preparation
# ============================================================

message(
  "All required source datasets are available.\n",
  "Starting mrorganic retrieveData()."
)

mrorganic_path <- mrorganic::retrieveData(
  model = "organic",
  rev = 2.1,
  regionmapping = "regionmappingGTAP11.csv"
)


# ============================================================
# 8. Completion
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "mrorganic source-data preparation complete\n",
  "============================================================\n",
  "retrieveData() output:\n",
  mrorganic_path,
  "\n\n",
  "You can now run:\n",
  "R/01_carbon_stocks.R\n",
  "============================================================\n\n",
  sep = ""
)