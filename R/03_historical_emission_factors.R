# ============================================================
# 03_historical_emission_factors.R
#
# Prepare historical ESA-CCI 2010-2015 land-use transitions
# and link them to spatial land-use-change emission factors
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(FABLEDownscalR)


# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------

data_root <- "data"
out_dir <- "output"

if (!dir.exists(out_dir)) {
  dir.create(
    out_dir,
    recursive = TRUE
  )
}

luc_file <- file.path(
  data_root,
  "LandCoverChange_ESA_CCI_2010_2015_Global_50km.geojson"
)

mapping_file <- file.path(
  data_root,
  "mapping_code.xlsx"
)

ef_file <- file.path(
  out_dir,
  "EF_Pools_transition_Cell.rds"
)

absolute_file <- file.path(
  out_dir,
  "EF_Pools_transition_Country_absolute.xlsx"
)

# ------------------------------------------------------------
# 2. Read global ESA-CCI 2010-2015 transitions
# ------------------------------------------------------------
#
# This reproduces only the part of fdr_load_inputs()
# used to read spatial$landcoverchange.
# No country filtering is applied because the 2010-2015
# dataset is global.
# ------------------------------------------------------------

if (!file.exists(luc_file)) {
  stop("ESA-CCI 2010-2015 transition file not found: ", luc_file)
}

luc_sp <- sf::st_read(
  here("data", "LandCoverChange_ESA_CCI_2010_2015_Global_50km.geojson"),
  quiet = TRUE
)


if (!"id_c" %in% names(luc_sp)) {
  stop("ESA-CCI transition GeoJSON is missing 'id_c'.")
}

landcoverchange <- luc_sp %>%
  sf::st_drop_geometry() %>%
  mutate(
    id_c = as.character(id_c)
  ) %>%
  select(
    -any_of(c("id", "id.x", "id.y"))
  )


# ------------------------------------------------------------
# 3. Read ESA-CCI land-use-change mapping
# ------------------------------------------------------------
#
# Same mapping table selected by fdr_load_inputs() when
# start_map_source = "ESACCI".
# ------------------------------------------------------------

if (!file.exists(mapping_file)) {
  stop("mapping_code.xlsx not found: ", mapping_file)
}

map_ESACCI_LUC <- readxl::read_excel(
  mapping_file,
  sheet = "ESACCI_change"
)


# ------------------------------------------------------------
# 4. Convert ESA-CCI transitions to FABLE land-use transitions
# ------------------------------------------------------------
#
# Use the existing FABLEDownscalR function rather than
# reproducing the ESA-CCI transition-code interpretation here.
# ------------------------------------------------------------

lc_build_country_luc_area <- function(
    LandCoverChange_df,
    map_LUC,
    expected_lu = c(
      "cropland", "forest", "newforest",
      "otherland", "pasture", "urban"
    ),
    Ts = 2010
) {
  
  LandCoverChange_df <- LandCoverChange_df %>%
    dplyr::mutate(id_c = as.character(id_c))
  
  
  # ---- wide -> long ----
  
  luc_long <- LandCoverChange_df %>%
    tidyr::pivot_longer(
      -id_c,
      names_to = "name",
      values_to = "AreaPerCode"
    ) %>%
    dplyr::mutate(
      name = stringr::str_remove(name, "X"),
      AreaPerCode = as.numeric(AreaPerCode)
    ) %>%
    dplyr::left_join(
      map_LUC %>%
        dplyr::mutate(code = as.character(code)),
      by = c("name" = "code")
    ) %>%
    dplyr::filter(
      !(from %in% c("ocean", "water", "not relevant", "other")),
      !(to   %in% c("ocean", "water", "not relevant", "other")),
      !is.na(from),
      !is.na(to)
    )
  
  
  # ---- collapse ESA-CCI classes mapping to same FABLE transition ----
  
  luc_std <- luc_long %>%
    dplyr::group_by(id_c, from, to) %>%
    dplyr::summarise(
      value = sum(AreaPerCode, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      ns      = as.character(id_c),
      lu.from = from,
      lu.to   = to,
      Ts      = Ts,
      value   = value
    )
  
  luc_std
}


luc_historic <- lc_build_country_luc_area(
  landcoverchange,
  map_ESACCI_LUC
)


# ------------------------------------------------------------
# 5. Read spatial land-use-change emission factors
# ------------------------------------------------------------

if (!file.exists(ef_file)) {
  stop("Spatial emission-factor file not found: ", ef_file)
}

EF_LUC <- readRDS(ef_file) %>%
  mutate(
    id_c = as.character(id_c)
  )


# ------------------------------------------------------------
# 5.a Read national carbon-stock averages
# ------------------------------------------------------------
#
# These are the national land-cover-specific carbon-stock
# averages calculated in 02_emission_factors.R.
#
# Aboveground and belowground biomass carbon are summed and
# converted from C to CO2 equivalent using 44/12.
# ------------------------------------------------------------

if (!file.exists(absolute_file)) {
  stop(
    "National carbon-stock file not found: ",
    absolute_file
  )
}

absolute_level <- readxl::read_excel(
  absolute_file
)


carbon_stocks_country <- absolute_level %>%
  filter(
    Type %in% c(
      "weighted_biomass_above_cropland",
      "weighted_biomass_below_cropland",
      "weighted_biomass_above_pasture",
      "weighted_biomass_below_pasture",
      "weighted_biomass_above_forest",
      "weighted_biomass_below_forest",
      "weighted_biomass_above_otherland",
      "weighted_biomass_below_otherland",
      "weighted_biomass_above_urban",
      "weighted_biomass_below_urban"
    )
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  ) %>%
  transmute(
    iso3,
    
    CO2_Cropland =
      (
        weighted_biomass_above_cropland +
          weighted_biomass_below_cropland
      ) * 44 / 12,
    
    CO2_Pasture =
      (
        weighted_biomass_above_pasture +
          weighted_biomass_below_pasture
      ) * 44 / 12,
    
    CO2_Urban =
      (
        weighted_biomass_above_urban +
          weighted_biomass_below_urban
      ) * 44 / 12,
    
    CO2_OtherLand =
      (
        weighted_biomass_above_otherland +
          weighted_biomass_below_otherland
      ) * 44 / 12,
    
    CO2_Forest =
      (
        weighted_biomass_above_forest +
          weighted_biomass_below_forest
      ) * 44 / 12
  )


# ------------------------------------------------------------
# 6. Check compatibility before joining
# ------------------------------------------------------------

required_ef_cols <- c(
  "iso3", "id_c", "from", "to",
  "ef_soc",
  "ef_biomass_above",
  "ef_biomass_below",
  "ef_biomass"
)

missing_ef_cols <- setdiff(
  required_ef_cols,
  names(EF_LUC)
)

if (length(missing_ef_cols) > 0) {
  stop(
    "Missing columns in EF_LUC: ",
    paste(missing_ef_cols, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 7. Merge historical transitions with spatial EFs
# ------------------------------------------------------------

luc_historic_EF <- luc_historic %>%
  rename(
    from = lu.from,
    to   = lu.to,
    id_c = ns
  ) %>%
  left_join(
    EF_LUC,
    by = c(
      "id_c",
      "from",
      "to"
    )
  )


# ------------------------------------------------------------
# 8. Keep historical transitions with positive area
# ------------------------------------------------------------

luc_historic_EF <- luc_historic_EF %>%
  filter(value > 0)


# ------------------------------------------------------------
# 9. Check matching between historical transitions and EFs
# ------------------------------------------------------------

EF_match_check <- luc_historic_EF %>%
  summarise(
    n_transitions      = n(),
    n_with_EF          = sum(!is.na(ef_biomass)),
    n_without_EF       = sum(is.na(ef_biomass)),
    area_total         = sum(value, na.rm = TRUE),
    area_with_EF       = sum(value[!is.na(ef_biomass)], na.rm = TRUE),
    area_without_EF    = sum(value[is.na(ef_biomass)], na.rm = TRUE),
    share_area_with_EF = area_with_EF / area_total
  )

print(EF_match_check)


# Look at transitions for which no spatial EF could be matched

unmatched_EF <- luc_historic_EF %>%
  filter(is.na(ef_biomass)) %>%
  group_by(from, to) %>%
  summarise(
    n_cells = n(),
    area = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(area))

print(unmatched_EF)


# ------------------------------------------------------------
# 10. Calculate historical country-level emission factors
# ------------------------------------------------------------
#
# For each country and land-use transition:
#
#          sum(Area_cell * EF_cell)
# EF = --------------------------------
#              sum(Area_cell)
#
# Only cells with an available biomass EF are included in
# the weighted mean.
#
# ef_biomass is used for consistency with the FABLE-C.
# ------------------------------------------------------------

historical_EF <- luc_historic_EF %>%
  filter(
    !is.na(iso3),
    iso3 != ""
  ) %>%
  group_by(
    iso3,
    from,
    to
  ) %>%
  summarise(
    area_total = sum(value, na.rm = TRUE),
    
    area_with_EF = sum(
      value[!is.na(ef_biomass)],
      na.rm = TRUE
    ),
    
    n_cells = n(),
    n_cells_with_EF = sum(!is.na(ef_biomass)),
    
    ef_biomass = if_else(
      area_with_EF > 0,
      weighted.mean(
        ef_biomass,
        w = value,
        na.rm = TRUE
      ),
      NA_real_
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    EF_area_coverage = if_else(
      area_total > 0,
      area_with_EF / area_total,
      NA_real_
    )
  )


# ------------------------------------------------------------
# 11. Check coverage of historical EF estimates
# ------------------------------------------------------------

historical_EF_coverage <- historical_EF %>%
  arrange(EF_area_coverage) %>%
  select(
    iso3,
    from,
    to,
    area_total,
    area_with_EF,
    EF_area_coverage,
    n_cells,
    n_cells_with_EF
  )

print(historical_EF_coverage)


# ------------------------------------------------------------
# 12. Prepare the FABLE-C historical EF table
# ------------------------------------------------------------
#
# Correspondence:
#
# to cropland  -> EF_Cropland
# to pasture   -> EF_Pasture
# to urban     -> EF_Urban
# to otherland -> SF_Regeneration
# to forest    -> SF_Afforestation
#
# "newforest" is not used here because the spatial EFs
# describe the physical transition to forest.
# ------------------------------------------------------------

historical_EF_FABLE <- historical_EF %>%
  filter(
    from %in% c(
      "forest",
      "otherland",
      "cropland",
      "pasture"
    ),
    to %in% c(
      "forest",
      "otherland",
      "cropland",
      "pasture",
      "urban"
    )
  ) %>%
  mutate(
    variable = case_when(
      to == "cropland"  ~ "EF_Cropland",
      to == "pasture"   ~ "EF_Pasture",
      to == "urban"     ~ "EF_Urban",
      to == "otherland" ~ "SF_Regeneration",
      to == "forest"    ~ "SF_Afforestation"
    )
  ) %>%
  select(
    iso3,
    from,
    variable,
    ef_biomass
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = ef_biomass
  ) %>%
  rename(
    LandCoverInit = from
  ) %>%
  mutate(
    LandCoverInit = recode(
      LandCoverInit,
      forest    = "Forest",
      otherland = "OtherLand",
      cropland  = "Cropland",
      pasture   = "Pasture"
    ),
    YearStart = 2010,
    YearEnd   = 2015
  )


# ------------------------------------------------------------
# 13. Complete the expected FABLE-C rows
# ------------------------------------------------------------
#
# This ensures that each country has the four expected
# LandCoverInit rows even where no historical transition was
# observed.
# ------------------------------------------------------------

countries <- sort(unique(EF_LUC$iso3))

historical_EF_FABLE <- tidyr::crossing(
  iso3 = countries,
  LandCoverInit = c(
    "Forest",
    "OtherLand",
    "Cropland",
    "Pasture"
  )
) %>%
  left_join(
    historical_EF_FABLE,
    by = c("iso3", "LandCoverInit")
  ) %>%
  mutate(
    YearStart = 2010,
    YearEnd   = 2015
  )


# ------------------------------------------------------------
# 13.a Add national CO2-equivalent biomass carbon stocks
# ------------------------------------------------------------
#
# CO2_init corresponds to the biomass carbon stock of the
# land cover specified by LandCoverInit.
#
# Destination CO2 columns are identical for all four initial
# land-cover rows of a country.
# ------------------------------------------------------------

historical_EF_FABLE <- historical_EF_FABLE %>%
  left_join(
    carbon_stocks_country,
    by = "iso3"
  ) %>%
  mutate(
    CO2_init = case_when(
      LandCoverInit == "Forest" ~ CO2_Forest,
      LandCoverInit == "OtherLand" ~ CO2_OtherLand,
      LandCoverInit == "Cropland" ~ CO2_Cropland,
      LandCoverInit == "Pasture" ~ CO2_Pasture,
      TRUE ~ NA_real_
    )
  )


# ------------------------------------------------------------
# 14. Set same-land-cover transitions to zero
# ------------------------------------------------------------
#
# These are not land-use-change emission factors.
# ------------------------------------------------------------

historical_EF_FABLE <- historical_EF_FABLE %>%
  mutate(
    EF_Cropland = if_else(
      LandCoverInit == "Cropland",
      0,
      EF_Cropland
    ),
    
    EF_Pasture = if_else(
      LandCoverInit == "Pasture",
      0,
      EF_Pasture
    ),
    
    SF_Regeneration = if_else(
      LandCoverInit == "OtherLand",
      0,
      SF_Regeneration
    ),
    
    SF_Afforestation = if_else(
      LandCoverInit == "Forest",
      0,
      SF_Afforestation
    )
  )


# ------------------------------------------------------------
# 15. Order table
# ------------------------------------------------------------

historical_EF_FABLE <- historical_EF_FABLE %>%
  mutate(
    LandCoverInit = factor(
      LandCoverInit,
      levels = c(
        "Forest",
        "OtherLand",
        "Cropland",
        "Pasture"
      )
    )
  ) %>%
  arrange(
    iso3,
    LandCoverInit
  ) %>%
  mutate(
    LandCoverInit = as.character(LandCoverInit)
  ) %>%
  select(
    iso3,
    LandCoverInit,
    YearStart,
    YearEnd,
    CO2_init,
    CO2_Cropland,
    CO2_Pasture,
    CO2_Urban,
    CO2_OtherLand,
    CO2_Forest,
    EF_Cropland,
    EF_Pasture,
    EF_Urban,
    SF_Regeneration,
    SF_Afforestation
  )

# ------------------------------------------------------------
# 16. Save outputs
# ------------------------------------------------------------

saveRDS(
  luc_historic_EF,
  file.path(
    out_dir,
    "Historical_LUC_EF_Cell_2010_2015.rds"
  )
)

saveRDS(
  historical_EF,
  file.path(
    out_dir,
    "Historical_EF_Country_2010_2015.rds"
  )
)

saveRDS(
  historical_EF_FABLE,
  file.path(
    out_dir,
    "Historical_EF_FABLE_2010_2015.rds"
  )
)

write.csv(
  historical_EF_FABLE,
  file.path(
    out_dir,
    "Historical_EF_FABLE_2010_2015.csv"
  ),
  row.names = FALSE
)

writexl::write_xlsx(
  historical_EF_FABLE,
  file.path(
    out_dir,
    "Historical_EF_FABLE_2010_2015.xlsx"
  )
)

write.csv(
  historical_EF_coverage,
  file.path(
    out_dir,
    "Historical_EF_Coverage_2010_2015.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 17. Final checks
# ------------------------------------------------------------

print(head(historical_EF_FABLE, 20))

cat(
  "\nNumber of countries:",
  n_distinct(historical_EF_FABLE$iso3),
  "\n"
)

cat(
  "Number of rows:",
  nrow(historical_EF_FABLE),
  "\n"
)

