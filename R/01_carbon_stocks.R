# ============================================================
# 01_carbon_stocks.R
#
# Compute soil organic carbon (SOC) and biomass carbon stocks
# by FABLE land-cover class using mrorganic.
#
# Two different ESA CCI -> FABLE mappings are used:
#
#   SOC:
#     cropland | pasture | other
#
#   Biomass:
#     cropland | forest | pasture | otherland | other
#
# The SOC and biomass calculations therefore use separate
# versioned madrat caches.
#
# If the versioned .rds outputs already exist, they are loaded
# instead of recomputing the expensive mrorganic calculations.
# ============================================================


# ============================================================
# 0. Setup
# ============================================================

source(here::here("R", "00_setup.R"))


# ============================================================
# 1. User options
# ============================================================

# Set TRUE only when you deliberately want to recompute
# carbon stocks for the current mapping version.
#
# Normally these should remain FALSE.

recompute_soc     <- FALSE
recompute_biomass <- FALSE


# ============================================================
# 2. ESA CCI -> FABLE land-cover mappings
# ============================================================

# NB_LAB corresponds to ESA CCI land-cover codes.
#
# IMPORTANT:
# If either mapping column below is modified, create a NEW
# soc_version or biomass_version in 00_setup.R.
#
# Never reuse an existing version name with a changed mapping.

custom_mapping <- data.frame(
  
  NB_LAB = c(
    20, 10, 30, 40, 12, 11,
    100, 61, 60, 62, 50, 160, 170, 90, 81, 80, 82, 71, 70, 72,
    130, 110,
    0, 200, 201, 220, 202, 210,
    140, 180, 120, 122, 121, 153, 152, 151, 150, 190
  ),
  
  # ----------------------------------------------------------
  # SOC mapping
  # ----------------------------------------------------------
  
  calc_SOClandType = c(
    
    # Cropland
    "cropland", "cropland", "cropland",
    "cropland", "cropland", "cropland",
    
    # Other
    "other", "other", "other", "other", "other",
    "other", "other", "other", "other", "other",
    "other", "other", "other", "other",
    
    # Pasture
    "pasture", "pasture",
    
    # Other
    "other", "other", "other", "other", "other", "other",
    
    # Other
    "other", "other", "other", "other", "other",
    "other", "other", "other", "other", "other"
  ),
  
  # ----------------------------------------------------------
  # Biomass mapping
  # ----------------------------------------------------------
  
  calc_biomasslandtype = c(
    
    # Cropland
    "cropland", "cropland", "cropland",
    "cropland", "cropland", "cropland",
    
    # Forest
    "forest", "forest", "forest", "forest", "forest",
    "forest", "forest", "forest", "forest", "forest",
    "forest", "forest", "forest", "forest",
    
    # Pasture
    "pasture", "pasture",
    
    # Other / non-vegetated
    "other", "other", "other",
    "other", "other", "other",
    
    # Otherland
    "otherland", "otherland", "otherland",
    "otherland", "otherland", "otherland",
    "otherland", "otherland", "otherland",
    
    # Urban / residual
    "other"
  )
)


# ============================================================
# 3. Helper functions
# ============================================================


# ------------------------------------------------------------
# 3.1 Build a custom calcLandTypeAreas() function
# ------------------------------------------------------------

make_custom_calcLandTypeAreas <- function(mapping_column, label) {
  
  force(mapping_column)
  force(label)
  
  function(categories = label) {
    
    map_df <- custom_mapping[
      ,
      c("NB_LAB", mapping_column)
    ]
    
    names(map_df) <- c(
      "code",
      "landtype"
    )
    
    landtypes <- unique(
      map_df$landtype
    )
    
    ids <- seq_along(
      landtypes
    )
    
    names(ids) <- landtypes
    
    recode_mat <- cbind(
      map_df$code,
      ids[map_df$landtype]
    )
    
    # Read ESA CCI 2010
    land <- madrat::readSource(
      "ESACCI",
      subtype = "landcover2010",
      convert = FALSE
    )
    
    # Reclassify ESA CCI into FABLE classes
    landSplit <- terra::classify(
      land,
      recode_mat,
      others = ids["other"]
    )
    
    # Land-cover area in each source raster cell
    out <- terra::segregate(
      landSplit
    ) *
      terra::cellSize(
        land,
        unit = "ha"
      )
    
    names(out) <- landtypes
    
    list(
      x = out,
      description = paste(
        "Custom FABLE land-type areas based on ESA CCI:",
        label
      ),
      unit = "ha",
      class = "SpatRaster"
    )
  }
}


# ------------------------------------------------------------
# 3.2 Temporarily replace mrorganic::calcLandTypeAreas()
# ------------------------------------------------------------

install_calcLandTypeAreas <- function(fun) {
  
  ns <- asNamespace(
    "mrorganic"
  )
  
  unlockBinding(
    "calcLandTypeAreas",
    ns
  )
  
  assign(
    "calcLandTypeAreas",
    fun,
    envir = ns
  )
  
  lockBinding(
    "calcLandTypeAreas",
    ns
  )
}


# ------------------------------------------------------------
# 3.3 Mapping version check
# ------------------------------------------------------------

check_mapping_version <- function(
    mapping,
    mapping_type,
    version
) {
  
  mapping_file <- file.path(
    mapping_dir,
    paste0(
      mapping_type,
      "_mapping_",
      version,
      ".csv"
    )
  )
  
  if (file.exists(mapping_file)) {
    
    old_mapping <- read.csv(
      mapping_file,
      stringsAsFactors = FALSE
    )
    
    old_mapping$NB_LAB <- as.numeric(
      old_mapping$NB_LAB
    )
    
    if (!identical(
      old_mapping,
      mapping
    )) {
      
      stop(
        "\nThe ",
        mapping_type,
        " mapping has changed but version '",
        version,
        "' already exists.\n\n",
        "Create a new ",
        mapping_type,
        "_version in R/00_setup.R before continuing.\n\n",
        "This protects against accidentally using a cache ",
        "generated with a different mapping."
      )
    }
    
    message(
      "Existing ",
      mapping_type,
      " mapping verified: ",
      version
    )
    
  } else {
    
    write.csv(
      mapping,
      mapping_file,
      row.names = FALSE
    )
    
    message(
      "Saved new ",
      mapping_type,
      " mapping version: ",
      version
    )
  }
  
  invisible(mapping_file)
}


# ============================================================
# 4. Check mapping versions
# ============================================================


# ------------------------------------------------------------
# 4.1 SOC
# ------------------------------------------------------------

current_soc_mapping <- custom_mapping[
  ,
  c(
    "NB_LAB",
    "calc_SOClandType"
  )
]

soc_mapping_file <- check_mapping_version(
  mapping = current_soc_mapping,
  mapping_type = "soc",
  version = soc_version
)


# ------------------------------------------------------------
# 4.2 Biomass
# ------------------------------------------------------------

current_biomass_mapping <- custom_mapping[
  ,
  c(
    "NB_LAB",
    "calc_biomasslandtype"
  )
]

biomass_mapping_file <- check_mapping_version(
  mapping = current_biomass_mapping,
  mapping_type = "biomass",
  version = biomass_version
)


# ------------------------------------------------------------
# Print current mappings
# ------------------------------------------------------------

cat(
  "\n============================================================\n",
  "SOC mapping: ",
  soc_version,
  "\n============================================================\n",
  sep = ""
)

print(
  current_soc_mapping |>
    dplyr::arrange(
      calc_SOClandType,
      NB_LAB
    )
)


cat(
  "\n============================================================\n",
  "Biomass mapping: ",
  biomass_version,
  "\n============================================================\n",
  sep = ""
)

print(
  current_biomass_mapping |>
    dplyr::arrange(
      calc_biomasslandtype,
      NB_LAB
    )
)


# ============================================================
# 5. SOC
# ============================================================

if (
  !recompute_soc &&
  file.exists(soc_file)
) {
  
  # ----------------------------------------------------------
  # Load previously saved SOC result
  # ----------------------------------------------------------
  
  message(
    "\nSaved SOC result found.\n",
    "Loading:\n",
    soc_file
  )
  
  soc_custom <- readRDS(
    soc_file
  )
  
} else {
  
  # ----------------------------------------------------------
  # Configure SOC cache
  # ----------------------------------------------------------
  
  message(
    "\nSOC will be computed using cache:\n",
    cache_soc
  )
  
  dir.create(
    cache_soc,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  madrat::setConfig(
    cachefolder = cache_soc
  )
  
  
  # ----------------------------------------------------------
  # Install SOC-specific mapping
  # ----------------------------------------------------------
  
  install_calcLandTypeAreas(
    make_custom_calcLandTypeAreas(
      mapping_column = "calc_SOClandType",
      label = soc_version
    )
  )
  
  
  # ----------------------------------------------------------
  # Calculate/load land-type areas
  # ----------------------------------------------------------
  
  message(
    "\nComputing/loading SOC land-type areas..."
  )
  
  time_soc_landareas <- system.time({
    
    soc_landareas <- madrat::calcOutput(
      "LandTypeAreas",
      aggregate = FALSE
    )
    
  })
  
  print(
    time_soc_landareas
  )
  
  
  # ----------------------------------------------------------
  # Verify classes
  # ----------------------------------------------------------
  
  actual_soc_classes <- names(
    soc_landareas
  )
  
  expected_soc_classes <- c(
    "cropland",
    "pasture",
    "other"
  )
  
  if (!setequal(
    actual_soc_classes,
    expected_soc_classes
  )) {
    
    stop(
      "\nUnexpected SOC classes.\n",
      "Expected: ",
      paste(
        expected_soc_classes,
        collapse = ", "
      ),
      "\nFound: ",
      paste(
        actual_soc_classes,
        collapse = ", "
      )
    )
  }
  
  message(
    "SOC land-cover classes verified."
  )
  
  
  # ----------------------------------------------------------
  # Calculate SOC
  # ----------------------------------------------------------
  
  message(
    "\nComputing/loading SOC by land type..."
  )
  
  time_soc <- system.time({
    
    soc_custom <- madrat::calcOutput(
      "SOCbyLandType",
      aggregate = FALSE
    )
    
  })
  
  print(
    time_soc
  )
  
  
  # ----------------------------------------------------------
  # Save immediately
  # ----------------------------------------------------------
  
  saveRDS(
    soc_custom,
    soc_file
  )
  
  message(
    "SOC saved to:\n",
    soc_file
  )
}


# ============================================================
# 6. Biomass
# ============================================================


# ------------------------------------------------------------
# Check whether both saved biomass outputs already exist
# ------------------------------------------------------------

biomass_outputs_exist <-
  file.exists(biomass_above_file) &&
  file.exists(biomass_below_file)


if (
  !recompute_biomass &&
  biomass_outputs_exist
) {
  
  # ----------------------------------------------------------
  # Load saved biomass outputs
  # ----------------------------------------------------------
  
  message(
    "\nSaved biomass results found."
  )
  
  bio_above_custom <- readRDS(
    biomass_above_file
  )
  
  bio_below_custom <- readRDS(
    biomass_below_file
  )
  
} else {
  
  # ----------------------------------------------------------
  # Configure biomass cache
  # ----------------------------------------------------------
  
  message(
    "\nBiomass will be computed using cache:\n",
    cache_biomass
  )
  
  dir.create(
    cache_biomass,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  madrat::setConfig(
    cachefolder = cache_biomass
  )
  
  
  # ----------------------------------------------------------
  # Install biomass-specific mapping
  # ----------------------------------------------------------
  
  install_calcLandTypeAreas(
    make_custom_calcLandTypeAreas(
      mapping_column = "calc_biomasslandtype",
      label = biomass_version
    )
  )
  
  
  # ----------------------------------------------------------
  # Calculate/load biomass land-type areas
  # ----------------------------------------------------------
  
  message(
    "\nComputing/loading biomass land-type areas..."
  )
  
  time_biomass_landareas <- system.time({
    
    biomass_landareas <- madrat::calcOutput(
      "LandTypeAreas",
      aggregate = FALSE
    )
    
  })
  
  print(
    time_biomass_landareas
  )
  
  
  # ----------------------------------------------------------
  # Verify classes
  # ----------------------------------------------------------
  
  actual_biomass_classes <- names(
    biomass_landareas
  )
  
  expected_biomass_classes <- c(
    "cropland",
    "forest",
    "pasture",
    "other",
    "otherland"
  )
  
  if (!setequal(
    actual_biomass_classes,
    expected_biomass_classes
  )) {
    
    stop(
      "\nUnexpected biomass classes.\n",
      "Expected: ",
      paste(
        expected_biomass_classes,
        collapse = ", "
      ),
      "\nFound: ",
      paste(
        actual_biomass_classes,
        collapse = ", "
      )
    )
  }
  
  message(
    "Biomass land-cover classes verified."
  )
  
  
  # ==========================================================
  # 6.1 Aboveground biomass
  # ==========================================================
  
  message(
    "\nComputing/loading aboveground biomass..."
  )
  
  time_above <- system.time({
    
    bio_above_custom <- madrat::calcOutput(
      "BiomassByLandType",
      subtype = "aboveground",
      aggregate = FALSE
    )
    
  })
  
  print(
    time_above
  )
  
  
  # Save immediately after expensive calculation
  saveRDS(
    bio_above_custom,
    biomass_above_file
  )
  
  message(
    "Aboveground biomass saved to:\n",
    biomass_above_file
  )
  
  
  # ==========================================================
  # 6.2 Belowground biomass
  # ==========================================================
  
  message(
    "\nComputing/loading belowground biomass..."
  )
  
  time_below <- system.time({
    
    bio_below_custom <- madrat::calcOutput(
      "BiomassByLandType",
      subtype = "belowground",
      aggregate = FALSE
    )
    
  })
  
  print(
    time_below
  )
  
  
  # Save immediately
  saveRDS(
    bio_below_custom,
    biomass_below_file
  )
  
  message(
    "Belowground biomass saved to:\n",
    biomass_below_file
  )
}


# ============================================================
# 7. Final checks
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "Carbon-stock calculations complete\n",
  "============================================================\n",
  "\nSOC version:\n  ",
  soc_version,
  "\nSOC file:\n  ",
  soc_file,
  "\n\nBiomass version:\n  ",
  biomass_version,
  "\nAboveground file:\n  ",
  biomass_above_file,
  "\nBelowground file:\n  ",
  biomass_below_file,
  "\n",
  "============================================================\n\n",
  sep = ""
)


# ============================================================
# 8. Basic object checks
# ============================================================

message(
  "SOC land types: ",
  paste(
    magclass::getNames(
      soc_custom
    ),
    collapse = ", "
  )
)

message(
  "Aboveground biomass land types: ",
  paste(
    magclass::getNames(
      bio_above_custom
    ),
    collapse = ", "
  )
)

message(
  "Belowground biomass land types: ",
  paste(
    magclass::getNames(
      bio_below_custom
    ),
    collapse = ", "
  )
)


message(
  "\nDone. You can now run R/02_emission_factors.R"
)