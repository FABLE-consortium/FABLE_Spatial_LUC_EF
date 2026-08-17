# ============================================================
# 02_emission_factors.R
#
# Compute FABLE grid-level and ecoregion-level land-use change
# emission factors from carbon stocks generated in
# 01_carbon_stocks.R
# ============================================================


# ============================================================
# 0. Setup
# ============================================================

source(here::here("R", "00_setup.R"))

# ============================================================
# User mapping options
# ============================================================

# Countries for which both individual transition maps and
# faceted transition matrices will be generated.
#
# Set to character(0) to skip country-specific maps.

countries_to_map <- c(
  "IND",
  "BRA",
  "ZAF"
)


# ============================================================
# 0.1 Create run-specific output directory
# ============================================================

# One timestamp is generated for the entire run.
# All outputs generated below will therefore be grouped together.

run_timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M"
)

run_dir <- file.path(
  out_dir,
  paste0(
    "run_",
    run_timestamp
  )
)

dir.create(
  run_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

message(
  "\n============================================================\n",
  "Starting emission-factor run: ", run_timestamp, "\n",
  "Outputs will be saved to:\n",
  run_dir,
  "\n============================================================\n"
)


# ============================================================
# 1. Load saved carbon-stock results
# ============================================================

required_inputs <- c(
  soc_file,
  biomass_above_file,
  biomass_below_file
)

missing_inputs <- required_inputs[
  !file.exists(required_inputs)
]

if (length(missing_inputs) > 0) {
  stop(
    "\nMissing carbon-stock input file(s):\n",
    paste(missing_inputs, collapse = "\n"),
    "\n\nRun R/01_carbon_stocks.R first."
  )
}


message("Loading carbon-stock results...")

soc_custom <- readRDS(
  soc_file
)

bio_above_custom <- readRDS(
  biomass_above_file
)

bio_below_custom <- readRDS(
  biomass_below_file
)


# ============================================================
# 2. Slice land-type layers
# ============================================================

# ------------------------------------------------------------
# SOC mapping
# ------------------------------------------------------------

# cropland | pasture | other
#
# soc_other is a broad catch-all containing all land that is
# neither cropland nor pasture.

cropland_soc <- soc_custom[, , "cropland"]
pasture_soc  <- soc_custom[, , "pasture"]
other_soc    <- soc_custom[, , "other"]


# ------------------------------------------------------------
# Biomass mapping
# ------------------------------------------------------------

# cropland | pasture | forest | otherland | other | urban

cropland_biomassAbove  <- bio_above_custom[, , "cropland"]
pasture_biomassAbove   <- bio_above_custom[, , "pasture"]
forest_biomassAbove    <- bio_above_custom[, , "forest"]
otherland_biomassAbove <- bio_above_custom[, , "otherland"]
other_biomassAbove     <- bio_above_custom[, , "other"]
urban_biomassAbove     <- bio_above_custom[, , "urban"]


cropland_biomassBelow  <- bio_below_custom[, , "cropland"]
pasture_biomassBelow   <- bio_below_custom[, , "pasture"]
forest_biomassBelow    <- bio_below_custom[, , "forest"]
otherland_biomassBelow <- bio_below_custom[, , "otherland"]
other_biomassBelow     <- bio_below_custom[, , "other"]
urban_biomassBelow     <- bio_below_custom[, , "urban"]


# ============================================================
# 3. Convert magclass objects to SpatRaster
# ============================================================

r_soc_cropland <- as.SpatRaster(cropland_soc)
names(r_soc_cropland) <- "soc_cropland"

r_soc_pasture <- as.SpatRaster(pasture_soc)
names(r_soc_pasture) <- "soc_pasture"

r_soc_other <- as.SpatRaster(other_soc)
names(r_soc_other) <- "soc_other"


r_biomass_above_cropland <- as.SpatRaster(
  cropland_biomassAbove
)
names(r_biomass_above_cropland) <- "biomass_above_cropland"


r_biomass_above_pasture <- as.SpatRaster(
  pasture_biomassAbove
)
names(r_biomass_above_pasture) <- "biomass_above_pasture"


r_biomass_above_forest <- as.SpatRaster(
  forest_biomassAbove
)
names(r_biomass_above_forest) <- "biomass_above_forest"


r_biomass_above_otherland <- as.SpatRaster(
  otherland_biomassAbove
)
names(r_biomass_above_otherland) <- "biomass_above_otherland"


r_biomass_above_other <- as.SpatRaster(
  other_biomassAbove
)
names(r_biomass_above_other) <- "biomass_above_other"


r_biomass_above_urban <- as.SpatRaster(
  urban_biomassAbove
)
names(r_biomass_above_urban) <- "biomass_above_urban"


r_biomass_below_cropland <- as.SpatRaster(
  cropland_biomassBelow
)
names(r_biomass_below_cropland) <- "biomass_below_cropland"


r_biomass_below_pasture <- as.SpatRaster(
  pasture_biomassBelow
)
names(r_biomass_below_pasture) <- "biomass_below_pasture"


r_biomass_below_forest <- as.SpatRaster(
  forest_biomassBelow
)
names(r_biomass_below_forest) <- "biomass_below_forest"


r_biomass_below_otherland <- as.SpatRaster(
  otherland_biomassBelow
)
names(r_biomass_below_otherland) <- "biomass_below_otherland"


r_biomass_below_other <- as.SpatRaster(
  other_biomassBelow
)
names(r_biomass_below_other) <- "biomass_below_other"


r_biomass_below_urban <- as.SpatRaster(
  urban_biomassBelow
)
names(r_biomass_below_urban) <- "biomass_below_urban"


# ============================================================
# 4. Read actual FABLE 50x50 km polygons
# ============================================================

grid50_file <- here::here(
  "data",
  "grid50_equal_area.gpkg"
)

if (!file.exists(grid50_file)) {
  stop(
    "\n50x50 km grid file not found:\n",
    grid50_file,
    "\n\nPlace grid50_equal_area.gpkg in the project data/ folder."
  )
}


grid50_sf <- sf::st_read(
  grid50_file,
  quiet = TRUE
)

stopifnot(
  "id_c" %in% names(grid50_sf)
)

stopifnot(
  !anyDuplicated(grid50_sf$id_c)
)


# ============================================================
# 5. Build multilayer carbon raster
# ============================================================

carbon_stack <- c(
  r_soc_cropland,
  r_soc_pasture,
  r_soc_other,
  
  r_biomass_above_cropland,
  r_biomass_above_pasture,
  r_biomass_above_forest,
  r_biomass_above_otherland,
  r_biomass_above_other,
  r_biomass_above_urban,
  
  r_biomass_below_cropland,
  r_biomass_below_pasture,
  r_biomass_below_forest,
  r_biomass_below_otherland,
  r_biomass_below_other,
  r_biomass_below_urban
)


names(carbon_stack) <- c(
  "soc_cropland",
  "soc_pasture",
  "soc_other",
  
  "biomass_above_cropland",
  "biomass_above_pasture",
  "biomass_above_forest",
  "biomass_above_otherland",
  "biomass_above_other",
  "biomass_above_urban",
  
  "biomass_below_cropland",
  "biomass_below_pasture",
  "biomass_below_forest",
  "biomass_below_otherland",
  "biomass_below_other",
  "biomass_below_urban"
)


# ============================================================
# 6. Aggregate carbon stocks onto actual 50x50 km polygons
# ============================================================

grid50_extract <- sf::st_transform(
  grid50_sf,
  crs = sf::st_crs(
    terra::crs(
      carbon_stack
    )
  )
)


# The source carbon rasters are longitude/latitude rasters.
# Actual cell area therefore changes with latitude.

cell_area <- terra::cellSize(
  carbon_stack[[1]],
  unit = "ha"
)

names(cell_area) <- "cell_area"


message(
  "\nAggregating carbon stocks onto FABLE 50x50 km grid..."
)


time_grid50 <- system.time({
  
  carbon_50 <- exactextractr::exact_extract(
    carbon_stack,
    grid50_extract,
    fun = "weighted_mean",
    weights = cell_area,
    force_df = TRUE,
    progress = TRUE
  )
  
})

print(
  time_grid50
)


names(carbon_50) <- names(
  carbon_stack
)


grid50 <- cbind(
  sf::st_drop_geometry(
    grid50_sf
  ),
  carbon_50
)


# ============================================================
# 7. Total biomass carbon stocks
# ============================================================

grid50 <- grid50 %>%
  mutate(
    
    biomass_total_cropland =
      biomass_above_cropland +
      biomass_below_cropland,
    
    biomass_total_pasture =
      biomass_above_pasture +
      biomass_below_pasture,
    
    biomass_total_forest =
      biomass_above_forest +
      biomass_below_forest,
    
    biomass_total_otherland =
      biomass_above_otherland +
      biomass_below_otherland,
    
    biomass_total_other =
      biomass_above_other +
      biomass_below_other,
    
    biomass_total_urban =
      biomass_above_urban +
      biomass_below_urban
  )


# ============================================================
# 8. Save grid-level carbon stocks immediately
# ============================================================

# This is saved early because the exact_extract step can be slow.

grid50_file_rds <- file.path(
  run_dir,
  "grid50_carbon_stocks.rds"
)

saveRDS(
  grid50,
  grid50_file_rds
)

message(
  "\nSaved grid-level carbon stocks to:\n",
  grid50_file_rds
)


# ============================================================
# 9. Diagnostics
# ============================================================

summary(
  grid50[, names(carbon_stack)]
)

summary(
  grid50[, c(
    "biomass_total_cropland",
    "biomass_total_pasture",
    "biomass_total_forest",
    "biomass_total_otherland",
    "biomass_total_other",
    "biomass_total_urban"
  )]
)


# ============================================================
# 10. Diagnostic carbon-stock maps
# ============================================================

carbon_map_dir <- file.path(
  run_dir,
  "maps_carbon_stocks"
)

dir.create(
  carbon_map_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


diagnostic_vars <- list(
  
  list(
    var = "biomass_above_forest",
    title = "Biomass aboveground on forest",
    unit = "Biomass (Mg C ha-1)"
  ),
  
  list(
    var = "biomass_below_forest",
    title = "Biomass belowground on forest",
    unit = "Biomass (Mg C ha-1)"
  ),
  
  list(
    var = "biomass_total_forest",
    title = "Total biomass on forest",
    unit = "Biomass (Mg C ha-1)"
  )
)


for (v in diagnostic_vars) {
  
  p <- ggplot(
    grid50,
    aes(
      x = x,
      y = y,
      colour = .data[[v$var]]
    )
  ) +
    geom_point(
      size = 0.05
    ) +
    scale_color_gradientn(
      colours = grDevices::terrain.colors(
        8,
        rev = TRUE
      )
    ) +
    coord_equal() +
    labs(
      title = paste(
        v$title,
        "on 50x50 km grid"
      ),
      colour = v$unit
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank()
    )
  
  print(p)
  
  ggsave(
    filename = file.path(
      carbon_map_dir,
      paste0(
        v$var,
        ".png"
      )
    ),
    plot = p,
    width = 10,
    height = 5.5,
    units = "in",
    dpi = 300
  )
}


# ============================================================
# 10.a Comparison with mrorganic vignette
# ============================================================

vignette_map_dir <- file.path(
  carbon_map_dir,
  "vignette_comparison"
)

dir.create(
  vignette_map_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


forest_vars <- list(
  
  list(
    var = "biomass_above_forest",
    title = "Biomass aboveground on forest",
    unit = "Biomass (Mg C ha-1)",
    limits = c(0, 275),
    breaks = seq(
      0,
      275,
      by = 50
    )
  ),
  
  list(
    var = "biomass_below_forest",
    title = "Biomass belowground on forest",
    unit = "Biomass (Mg C ha-1)",
    limits = c(0, 78),
    breaks = seq(
      0,
      78,
      by = 10
    )
  )
)


for (v in forest_vars) {
  
  p <- ggplot(
    grid50,
    aes(
      x = x,
      y = y,
      colour = .data[[v$var]]
    )
  ) +
    geom_point(
      size = 0.05
    ) +
    scale_color_gradientn(
      colours = grDevices::terrain.colors(
        8,
        rev = TRUE
      ),
      limits = v$limits,
      breaks = v$breaks,
      oob = scales::squish
    ) +
    coord_equal() +
    labs(
      title = paste(
        v$title,
        "on 50x50 km grid"
      ),
      colour = v$unit
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank()
    )
  
  print(p)
  
  ggsave(
    filename = file.path(
      vignette_map_dir,
      paste0(
        v$var,
        "_vignette_scale.png"
      )
    ),
    plot = p,
    width = 10,
    height = 5.5,
    units = "in",
    dpi = 300
  )
}


# ============================================================
# 11. Variables used for emission factors
# ============================================================

ef_cols <- c(
  
  # SOC
  "soc_cropland",
  "soc_pasture",
  "soc_other",
  
  # Aboveground biomass
  "biomass_above_cropland",
  "biomass_above_pasture",
  "biomass_above_forest",
  "biomass_above_otherland",
  "biomass_above_other",
  "biomass_above_urban",
  
  # Belowground biomass
  "biomass_below_cropland",
  "biomass_below_pasture",
  "biomass_below_forest",
  "biomass_below_otherland",
  "biomass_below_other",
  "biomass_below_urban"
)


# ============================================================
# 12. Aggregate to ecoregion level
# ============================================================

grid_eco <- grid50 %>%
  group_by(
    iso3,
    ECO_NAME
  ) %>%
  summarise(
    across(
      all_of(
        ef_cols
      ),
      ~ weighted.mean(
        .x,
        area,
        na.rm = TRUE
      ),
      .names = "weighted_{.col}"
    ),
    .groups = "drop"
  )


# ============================================================
# 13. National carbon-stock averages
# ============================================================

absolute_level <- grid50 %>%
  group_by(
    iso3
  ) %>%
  summarise(
    across(
      all_of(
        ef_cols
      ),
      ~ weighted.mean(
        .x,
        area,
        na.rm = TRUE
      ),
      .names = "weighted_{.col}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with(
      "weighted_"
    ),
    names_to = "Type",
    values_to = "Value"
  )


# ============================================================
# 14. Ecoregion-level transition emission factors
# ============================================================

df_long <- grid_eco %>%
  pivot_longer(
    cols = starts_with(
      "weighted_"
    ),
    names_to = c(
      "pool",
      "landcover"
    ),
    names_pattern =
      "weighted_(soc|biomass_above|biomass_below)_(.*)",
    values_to = "stock"
  )


df_transitions <- df_long %>%
  rename(
    from = landcover,
    stock_from = stock
  ) %>%
  inner_join(
    df_long %>%
      rename(
        to = landcover,
        stock_to = stock
      ),
    by = c(
      "iso3",
      "ECO_NAME",
      "pool"
    ),
    relationship = "many-to-many"
  ) %>%
  filter(
    from != to
  ) %>%
  mutate(
    ef_transition =
      stock_from -
      stock_to
  )


# ------------------------------------------------------------
# Helper for missing carbon pools
# ------------------------------------------------------------

safe_sum <- function(x) {
  
  if (all(is.na(x))) {
    
    NA_real_
    
  } else {
    
    sum(
      x,
      na.rm = TRUE
    )
  }
}


df_transition_total <- df_transitions %>%
  group_by(
    iso3,
    ECO_NAME,
    from,
    to
  ) %>%
  summarise(
    
    ef_soc = safe_sum(
      ef_transition[
        pool == "soc"
      ]
    ),
    
    ef_biomass_above = safe_sum(
      ef_transition[
        pool == "biomass_above"
      ]
    ),
    
    ef_biomass_below = safe_sum(
      ef_transition[
        pool == "biomass_below"
      ]
    ),
    
    ef_biomass =
      ef_biomass_above +
      ef_biomass_below,
    
    .groups = "drop"
  ) %>%
  mutate(
    pools_covered = case_when(
      
      !is.na(ef_soc) &
        !is.na(ef_biomass) ~
        "soc + biomass",
      
      is.na(ef_soc) &
        !is.na(ef_biomass) ~
        "biomass only",
      
      !is.na(ef_soc) &
        is.na(ef_biomass) ~
        "soc only",
      
      TRUE ~
        "none"
    )
  )


# ============================================================
# 15. Cell-level transition emission factors
# ============================================================

df_long_cell <- grid50 %>%
  pivot_longer(
    cols = all_of(
      ef_cols
    ),
    names_to = c(
      "pool",
      "landcover"
    ),
    names_pattern =
      "(soc|biomass_above|biomass_below)_(.*)",
    values_to = "stock"
  )


df_transitions_cell <- df_long_cell %>%
  rename(
    from = landcover,
    stock_from = stock
  ) %>%
  select(
    iso3,
    ECO_NAME,
    id_c,
    pool,
    from,
    stock_from
  ) %>%
  inner_join(
    df_long_cell %>%
      rename(
        to = landcover,
        stock_to = stock
      ) %>%
      select(
        iso3,
        ECO_NAME,
        id_c,
        pool,
        to,
        stock_to
      ),
    by = c(
      "iso3",
      "ECO_NAME",
      "id_c",
      "pool"
    ),
    relationship = "many-to-many"
  ) %>%
  filter(
    from != to
  ) %>%
  mutate(
    ef_transition =
      stock_from -
      stock_to
  )


df_transition_total_cell <- df_transitions_cell %>%
  group_by(
    iso3,
    id_c,
    ECO_NAME,
    from,
    to
  ) %>%
  summarise(
    
    ef_soc = safe_sum(
      ef_transition[
        pool == "soc"
      ]
    ),
    
    ef_biomass_above = safe_sum(
      ef_transition[
        pool == "biomass_above"
      ]
    ),
    
    ef_biomass_below = safe_sum(
      ef_transition[
        pool == "biomass_below"
      ]
    ),
    
    ef_biomass =
      ef_biomass_above +
      ef_biomass_below,
    
    .groups = "drop"
  ) %>%
  mutate(
    pools_covered = case_when(
      
      !is.na(ef_soc) &
        !is.na(ef_biomass) ~
        "soc + biomass",
      
      is.na(ef_soc) &
        !is.na(ef_biomass) ~
        "biomass only",
      
      !is.na(ef_soc) &
        is.na(ef_biomass) ~
        "soc only",
      
      TRUE ~
        "none"
    )
  )


# ============================================================
# 16. Add coordinates for transition maps
# ============================================================

transition_total_maps <- df_transition_total_cell %>%
  left_join(
    grid50 %>%
      select(
        id_c,
        x,
        y
      ),
    by = "id_c"
  )


# ============================================================
# 17. Global biomass transition maps
# ============================================================

transition_map_dir <- file.path(
  run_dir,
  "maps_biomass_emission_factors"
)

dir.create(
  transition_map_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# Common scale for all transition maps
# ------------------------------------------------------------

max_abs <- max(
  abs(
    transition_total_maps$ef_biomass
  ),
  na.rm = TRUE
)

ef_limits <- c(
  -max_abs,
  max_abs
)

ef_breaks <- pretty(
  ef_limits,
  n = 7
)


transitions <- transition_total_maps %>%
  distinct(
    from,
    to
  )


for (i in seq_len(
  nrow(
    transitions
  )
)) {
  
  from_i <- transitions$from[i]
  to_i   <- transitions$to[i]
  
  d <- transition_total_maps %>%
    filter(
      from == from_i,
      to == to_i
    )
  
  if (
    all(
      is.na(
        d$ef_biomass
      )
    )
  ) {
    next
  }
  
  
  p <- ggplot(
    d,
    aes(
      x = x,
      y = y,
      colour = ef_biomass
    )
  ) +
    geom_point(
      size = 0.05
    ) +
    scale_color_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = ef_limits,
      breaks = ef_breaks,
      oob = scales::squish
    ) +
    coord_equal() +
    labs(
      title = paste(
        "Biomass emission factor:",
        from_i,
        "→",
        to_i
      ),
      colour = "Mg C ha-1"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank()
    )
  
  
  print(p)
  
  
  filename <- paste0(
    "biomass_EF_",
    from_i,
    "_to_",
    to_i,
    ".png"
  )
  
  
  ggsave(
    filename = file.path(
      transition_map_dir,
      filename
    ),
    plot = p,
    width = 10,
    height = 5.5,
    units = "in",
    dpi = 300
  )
}


# ============================================================
# 18. Country-specific transition maps
# ============================================================

for (iso3_i in countries_to_map) {
  
  message(
    "\nGenerating individual transition maps for ",
    iso3_i,
    "..."
  )
  
  country_maps <- transition_total_maps %>%
    filter(
      iso3 == iso3_i
    )
  
  if (nrow(country_maps) == 0) {
    
    warning(
      "No map data found for ISO3 = ",
      iso3_i
    )
    
    next
  }
  
  
  # ----------------------------------------------------------
  # Create country output directory
  # ----------------------------------------------------------
  
  country_map_dir <- file.path(
    transition_map_dir,
    iso3_i
  )
  
  dir.create(
    country_map_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Identify available transitions
  # ----------------------------------------------------------
  
  transitions_country <- country_maps %>%
    distinct(
      from,
      to
    )
  
  
  # ----------------------------------------------------------
  # Generate one map per transition
  # ----------------------------------------------------------
  
  for (i in seq_len(
    nrow(
      transitions_country
    )
  )) {
    
    from_i <- transitions_country$from[i]
    to_i   <- transitions_country$to[i]
    
    d <- country_maps %>%
      filter(
        from == from_i,
        to == to_i
      )
    
    if (all(is.na(d$ef_biomass))) {
      next
    }
    
    
    p <- ggplot(
      d,
      aes(
        x = x,
        y = y,
        colour = ef_biomass
      )
    ) +
      geom_point(
        size = 1
      ) +
      scale_color_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0,
        limits = ef_limits,
        breaks = ef_breaks,
        oob = scales::squish
      ) +
      coord_equal() +
      labs(
        title = paste0(
          iso3_i,
          " – Biomass emission factor: ",
          from_i,
          " → ",
          to_i
        ),
        colour = "Mg C ha-1"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    
    print(p)
    
    
    filename <- paste0(
      iso3_i,
      "_biomass_EF_",
      from_i,
      "_to_",
      to_i,
      ".png"
    )
    
    
    ggsave(
      filename = file.path(
        country_map_dir,
        filename
      ),
      plot = p,
      width = 8,
      height = 6,
      units = "in",
      dpi = 300
    )
  }
}

# ============================================================
# 18.b Country transition matrices of biomass emission factors
# ============================================================

# Land-cover order used in all matrices

land_classes <- c(
  "forest",
  "cropland",
  "pasture",
  "otherland",
  "urban"
)


# ------------------------------------------------------------
# Create common matrix output directory
# ------------------------------------------------------------

matrix_dir <- file.path(
  transition_map_dir,
  "transition_matrices"
)

dir.create(
  matrix_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# Use global colour scale
# ------------------------------------------------------------

# Same scale as the global and individual country maps.
# This allows direct comparison between countries.

matrix_limits <- ef_limits
matrix_breaks <- ef_breaks


# ------------------------------------------------------------
# Loop through countries
# ------------------------------------------------------------

for (iso3_i in countries_to_map) {
  
  message(
    "\nGenerating transition matrix for ",
    iso3_i,
    "..."
  )
  
  
  # ----------------------------------------------------------
  # Select country
  # ----------------------------------------------------------
  
  country_matrix_data <- transition_total_maps %>%
    filter(
      iso3 == iso3_i
    )
  
  
  if (nrow(country_matrix_data) == 0) {
    
    warning(
      "No data found for transition matrix for ISO3 = ",
      iso3_i
    )
    
    next
  }
  
  
  # ----------------------------------------------------------
  # Select relevant land-cover transitions
  # ----------------------------------------------------------
  
  # Urban is included as a destination land use but excluded
  # as a source land use.
  
  country_matrix_data <- country_matrix_data %>%
    filter(
      from %in% land_classes,
      to %in% land_classes,
      from != "urban"
    ) %>%
    mutate(
      
      from = factor(
        from,
        levels = land_classes
      ),
      
      to = factor(
        to,
        levels = land_classes
      )
    )
  
  
  # Remove unused urban level from FROM dimension
  
  country_matrix_data$from <- droplevels(
    country_matrix_data$from
  )
  
  
  # ----------------------------------------------------------
  # Create transition matrix
  # ----------------------------------------------------------
  
  p_matrix <- ggplot(
    country_matrix_data,
    aes(
      x = x,
      y = y,
      colour = ef_biomass
    )
  ) +
    
    geom_point(
      size = 0.5
    ) +
    
    scale_color_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = matrix_limits,
      breaks = matrix_breaks,
      oob = scales::squish
    ) +
    
    # Rows = land use BEFORE transition
    # Columns = land use AFTER transition
    
    facet_grid(
      rows = vars(from),
      cols = vars(to),
      drop = FALSE,
      labeller = labeller(
        from = function(x) paste0("From: ", x),
        to   = function(x) paste0("To: ", x)
      )
    ) +
    
    coord_equal() +
    
    labs(
      title = paste0(
        iso3_i,
        " – Biomass emission factors by land-use transition"
      ),
      subtitle = paste0(
        "Rows = initial land use | ",
        "Columns = destination land use | ",
        "Positive values = carbon loss"
      ),
      colour = expression(
        "Biomass EF (Mg C ha"^{-1}*")"
      )
    ) +
    
    theme_minimal() +
    
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      panel.grid = element_blank(),
      
      strip.text.x = element_text(
        size = 9,
        face = "bold"
      ),
      
      strip.text.y = element_text(
        size = 9,
        face = "bold",
        angle = 0
      ),
      
      panel.spacing = grid::unit(
        0.4,
        "lines"
      ),
      
      plot.title = element_text(
        size = 16,
        face = "bold"
      ),
      
      plot.subtitle = element_text(
        size = 10
      ),
      
      legend.position = "right",
      
      plot.margin = margin(
        10,
        10,
        10,
        10
      )
    )
  
  
  print(
    p_matrix
  )
  
  
  # ----------------------------------------------------------
  # Save PNG
  # ----------------------------------------------------------
  
  png_file <- file.path(
    matrix_dir,
    paste0(
      iso3_i,
      "_biomass_EF_transition_matrix.png"
    )
  )
  
  
  ggsave(
    filename = png_file,
    plot = p_matrix,
    width = 16,
    height = 13,
    units = "in",
    dpi = 300
  )
  
  
  # ----------------------------------------------------------
  # Save PDF
  # ----------------------------------------------------------
  
  pdf_file <- file.path(
    matrix_dir,
    paste0(
      iso3_i,
      "_biomass_EF_transition_matrix.pdf"
    )
  )
  
  
  ggsave(
    filename = pdf_file,
    plot = p_matrix,
    width = 16,
    height = 13,
    units = "in"
  )
  
  
  message(
    "Saved transition matrix for ",
    iso3_i
  )
}

# ============================================================
# 19. Export tables and R objects
# ============================================================

# ------------------------------------------------------------
# Ecoregion transition EFs
# ------------------------------------------------------------

writexl::write_xlsx(
  df_transition_total,
  file.path(
    run_dir,
    "EF_Pools_transition_Ecoregion.xlsx"
  )
)


# ------------------------------------------------------------
# National carbon-stock averages
# ------------------------------------------------------------

writexl::write_xlsx(
  absolute_level,
  file.path(
    run_dir,
    "EF_Pools_transition_Country_absolute.xlsx"
  )
)


# ------------------------------------------------------------
# Cell-level transition EFs
# ------------------------------------------------------------

saveRDS(
  df_transition_total_cell,
  file.path(
    run_dir,
    "EF_Pools_transition_Cell.rds"
  )
)


# ------------------------------------------------------------
# Cell-level transition EFs + coordinates
# ------------------------------------------------------------

saveRDS(
  transition_total_maps,
  file.path(
    run_dir,
    "EF_Pools_transition_Cell_maps.rds"
  )
)


# ------------------------------------------------------------
# Ecoregion carbon-stock table
# ------------------------------------------------------------

saveRDS(
  grid_eco,
  file.path(
    run_dir,
    "carbon_stocks_ecoregion.rds"
  )
)


# ============================================================
# 20. Save run metadata
# ============================================================

# This helps identify exactly which carbon-stock mapping
# versions were used to produce each EF run.

run_info <- data.frame(
  run_timestamp = run_timestamp,
  soc_version = soc_version,
  biomass_version = biomass_version,
  soc_input = basename(soc_file),
  biomass_above_input = basename(biomass_above_file),
  biomass_below_input = basename(biomass_below_file),
  stringsAsFactors = FALSE
)


write.csv(
  run_info,
  file.path(
    run_dir,
    "run_info.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 21. Completion
# ============================================================

message(
  "\n============================================================\n",
  "02_emission_factors.R complete\n",
  "============================================================\n",
  "Run: ", run_timestamp, "\n",
  "SOC mapping: ", soc_version, "\n",
  "Biomass mapping: ", biomass_version, "\n",
  "\nAll outputs written to:\n",
  run_dir,
  "\n============================================================\n"
)