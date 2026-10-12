#' NERC-regional LCA factors (Task #3 of the LCA-DEEPENING backlog).
#'
#' @description
#' Grid-electricity LCA factors by NERC region, aggregated from the
#' eGRID subregion × year panel (`data/egrid_subregion_year_2009_2024.csv`
#' in the net_energy_equity repo). The subregion→NERC map lives at the
#' top of this file; aggregation uses `total_gen_mwh` as weight so a
#' region's factor is the generation-weighted mean of its subregions'
#' carbon intensity.
#'
#' Why this exists: before this helper, every LCA pathway in
#' emburdenlca used either national (USEEIO) or country-level
#' (EXIOBASE) grid intensities. The DC-E2C3 pipeline's 4,000×
#' variation in health damages across NERC regions was invisible to
#' the household-side HEII attach because grid intensity was national.
#' This closes that gap.
#'
#' @param data_root Path to the net_energy_equity data directory
#'   that holds the eGRID subregion panel + the nerc crosswalk.
#'   Defaults to `~/Documents/apps/net_energy_equity/data`.
#' @return A tibble with columns `nerc_region, year,
#'   co2e_total_kg_mwh, coal_pct, gas_pct, nuclear_pct,
#'   renewable_pct, gen_total_mwh`. CO2e is in kg/MWh (eGRID
#'   publishes lb/MWh; we convert, 1 lb = 0.4536 kg).
#' @export
#' @examples
#' \dontrun{
#' nerc_grid <- load_nerc_grid_factors()
#' dplyr::filter(nerc_grid, year == 2024)
#' }
load_nerc_grid_factors <- function(
  data_root = "~/Documents/apps/net_energy_equity/data"
) {
  stopifnot(requireNamespace("dplyr", quietly = TRUE),
            requireNamespace("readr", quietly = TRUE))
  dir <- path.expand(data_root)
  subregion_panel_path <- file.path(dir, "egrid_subregion_year_2009_2024.csv")
  if (!file.exists(subregion_panel_path)) {
    stop("Missing eGRID subregion panel at ", subregion_panel_path,
         ". Expected net_energy_equity/data/.")
  }
  panel <- readr::read_csv(subregion_panel_path, show_col_types = FALSE)

  # Subregion → NERC map. Prefix encodes the parent NERC for most
  # codes; the handful of non-prefixed codes are listed explicitly.
  # ASCC + HICC are NOT NERC regions (Alaska + Hawaii reliability
  # councils); map to NA and they drop out of the aggregate.
  sr_to_nerc <- c(
    SRSO = "SERC", SRMV = "SERC", SRMW = "SERC",
    SRTV = "SERC", SRVC = "SERC",
    RFCE = "RFC",  RFCM = "RFC",  RFCW = "RFC",
    FRCC = "FRCC",
    NWPP = "WECC", RMPA = "WECC", CAMX = "WECC", AZNM = "WECC",
    MROW = "MRO",  MROE = "MRO",
    SPNO = "SPP",  SPSO = "SPP",
    NYLI = "NPCC", NYCW = "NPCC", NYUP = "NPCC", NEWE = "NPCC",
    ERCT = "TRE",
    HIMS = NA_character_, HIOA = NA_character_,
    AKGD = NA_character_, AKMS = NA_character_,
    PRMS = NA_character_  # Puerto Rico — not NERC
  )
  panel$nerc_region <- unname(sr_to_nerc[panel$subregion])
  panel <- panel |> dplyr::filter(!is.na(.data$nerc_region),
                                   !is.na(.data$carbon_intensity_lb_per_mwh),
                                   !is.na(.data$total_gen_mwh),
                                   .data$total_gen_mwh > 0)
  # Generation-weighted means per (nerc_region, year).
  out <- panel |>
    dplyr::group_by(.data$nerc_region, .data$year) |>
    dplyr::summarise(
      co2e_total_kg_mwh = sum(.data$carbon_intensity_lb_per_mwh *
                               .data$total_gen_mwh, na.rm = TRUE) /
                           sum(.data$total_gen_mwh) * 0.4535924,
      coal_pct      = sum(.data$coal_pct      * .data$total_gen_mwh) /
                       sum(.data$total_gen_mwh),
      gas_pct       = sum(.data$gas_pct       * .data$total_gen_mwh) /
                       sum(.data$total_gen_mwh),
      nuclear_pct   = sum(.data$nuclear_pct   * .data$total_gen_mwh) /
                       sum(.data$total_gen_mwh),
      renewable_pct = sum(.data$renewable_pct * .data$total_gen_mwh) /
                       sum(.data$total_gen_mwh),
      gen_total_mwh = sum(.data$total_gen_mwh),
      .groups = "drop"
    )
  out
}

#' Lookup a NERC region's grid CO2e factor for a given year.
#'
#' @param nerc_region 4-letter NERC code (FRCC, MRO, NPCC, RFC, SERC,
#'   SPP, TRE, WECC).
#' @param year Numeric year (defaults to latest in the panel).
#' @param field Which field to return — defaults to
#'   `"co2e_total_kg_mwh"`.
#' @return A scalar numeric; NA if the region+year is not covered.
#' @export
nerc_grid_factor <- function(nerc_region, year = NULL,
                              field = "co2e_total_kg_mwh") {
  tbl <- load_nerc_grid_factors()
  if (is.null(year)) year <- max(tbl$year, na.rm = TRUE)
  row <- tbl[tbl$nerc_region == nerc_region & tbl$year == year, ]
  if (nrow(row) == 0) return(NA_real_)
  row[[field]]
}

# ---- Per-NERC inst/extdata CSV loader (shared helper) ---------------
.load_inst_extdata_csv <- function(basename) {
  path <- system.file("extdata", basename, package = "emburdenlca")
  if (!nzchar(path) || !file.exists(path)) {
    guess <- file.path("inst", "extdata", basename)
    if (file.exists(guess)) path <- guess
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop(basename, " not found; install emburdenlca or run from its repo root.")
  }
  stopifnot(requireNamespace("readr", quietly = TRUE))
  readr::read_csv(path, show_col_types = FALSE)
}

# ---- Per-NERC AWARE water-use factor (Task #4) ----------------------

#' Load per-NERC AWARE water-use factors.
#'
#' @description
#' AWARE (Available WAter REmaining; Boulay et al. 2018) factors
#' aggregated to NERC regions via population-weighted US state factors
#' + eGRID subregion mapping. Replaces wright's `aware_factor: 12.5`
#' hardcoded Duke-Carolinas value with region-specific scarcity signal.
#'
#' Range: NPCC 2.9 (abundant rainfall + Great Lakes) to CAISO/WECC 42
#' (dry southwest). An identical 300 MW facility consuming 5.7 Mm³/yr
#' direct cooling water has a water_scarcity_m3eq footprint that spans
#' 14× across US regions on this axis alone.
#'
#' @return A tibble with `nerc_region, aware_factor, source_citation`.
#' @export
load_nerc_water_aware <- function() {
  .load_inst_extdata_csv("nerc_aware_factors.csv")
}

# ---- Per-NERC BTM solar capacity factor (Task #5) --------------------

#' Load per-NERC BTM solar annual capacity factor.
#'
#' @description
#' NREL ReV + PVWatts regional CF aggregates for BTM solar (fixed-tilt,
#' south-facing, latitude-adjusted). Replaces wright's hardcoded CAISO
#' default with region-specific CF. Range: NPCC 0.14 to WECC 0.22
#' (1.6×). Drives the duck-curve multiplier used by
#' `compute_storage_sizing_by_nerc()`.
#'
#' Hourly displaced-generation profile is not yet materialized here —
#' when a `wright:...btm_solar_profile_<nerc>.parquet` export lands,
#' this helper will gain a `profile_24x12` column.
#'
#' @return A tibble with `nerc_region, annual_cf, notes`.
#' @export
load_nerc_btm_solar_cf <- function() {
  .load_inst_extdata_csv("nerc_btm_solar_cf.csv")
}

# ---- Per-NERC diesel tier default (Task #6) -------------------------

#' Load per-NERC diesel-tier regulatory default.
#'
#' @description
#' Which EPA tier a new diesel genset defaults to by regulatory
#' environment: CAISO + NPCC (CARB + NY/MA MACT) mandate Tier IV;
#' other NERC regions use Tier II as the dominant default. Overrides
#' wright's `tier: "II"` global config. Tier IV cuts NOx 94% + PM2.5
#' 90% vs Tier II — the single biggest acute-emissions lever in
#' backup design when a mandate applies.
#'
#' Note: this is a REGIONAL default, not a per-county override. Coastal
#' nonattainment counties within non-Tier-IV regions still require
#' Tier IV via local permitting; that lookup is a separate ticket.
#'
#' @return A tibble with `nerc_region, tier_default, regulatory_basis`.
#' @export
load_nerc_diesel_tier_default <- function() {
  .load_inst_extdata_csv("nerc_diesel_tier_default.csv")
}
